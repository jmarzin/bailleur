import java.io.{File, PrintWriter}
import javax.swing.filechooser.FileNameExtensionFilter
import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.swing.Dialog.Message
import scala.swing.{Dialog, FileChooser, Swing}

/**
  * Created by jacquesmarzin on 03/02/2017.
  */

object bailleurApp extends App {
  implicit object MyFormat extends DefaultCSVFormat {
    override val delimiter = '|'
  }
  implicit class Voie(s: String) {
    def nomVoie : (String,Stream[Int]) = {
      val debutNom = s.indexOf("(")
      if (debutNom < 0) return (s,Stream())
      val finNom = if (s.indexOf(")") < 0) s.length else s.indexOf(")")
      val rue = (s.substring(debutNom + 1, finNom) + " " + s.substring(0, debutNom - 1) + (if (finNom == s.length) "" else s.substring(finNom + 1))).replaceAll("' ", "'").trim
      val debut2 = rue.indexOf("(")
      if(debut2 < 0) return (rue, Stream())
      val fin2 = if (rue.indexOf(")") < 0) rue.length else rue.indexOf(")")
      val intervalle = if (fin2 == debut2 + 1) "" else rue.substring(debut2 + 1, fin2)
      if (intervalle == "") return (rue.substring(0,debut2-1), Stream())
      val conditions = intervalle.split(" et ")
      var ensemble: Stream[Int] = Stream()
      val nAfin = """.*(\d+) à la fin""".r
      val nAm = """.*(\d+) à (\d+)""".r
      val impairs = """([Ii]mpairs)""".r
      val pairs = """([Pp]airs)""".r

      for(cond<-conditions) {

        cond match {
          case nAfin(debut) => ensemble = ensemble.append((debut.toInt to 10000 by 2).toStream)
          case nAm(debut,fin) => ensemble = ensemble.append((debut.toInt to fin.toInt by 2).toStream)
          case impairs(_) => ensemble = ensemble.append((1 to 10000 by 2).toStream)
          case pairs(_) => ensemble = ensemble.append((2 to 10000 by  2).toStream)
          case _ =>
        }
      }
      (rue.substring(0,debut2-1).trim, ensemble)
    }
  }
  def choosePlainFile(title: String = ""): Option[File] = {
    val filter = new FileNameExtensionFilter("Fichier csv","csv")
    val chooser = new FileChooser(new File("."))
    chooser.title = title
    chooser.fileFilter = filter
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) Some(chooser.selectedFile) else None
  }

  def rechercheSipCommune(comm: String, reglesCommunes: List[List[String]]): Option[String] = {
    var iCommune = 0
    do {
      val communeTable = tableCommunes.get(comm.replaceFirst(reglesCommunes(iCommune).head,reglesCommunes(iCommune)(1)))
      if(communeTable.isDefined){
        return communeTable
      } else {
        iCommune += 1
      }
    } while (iCommune < reglesCommunes.size)
    None
  }

  def rechercheSipVoie(voie: String, reglesVoies: List[List[String]]): Option[List[(String,Stream[Int])]] = {
    var iVoie = 0
    do {
      val rueTable = tableRues.get(voie.replaceFirst(reglesVoies(iVoie).head,reglesVoies(iVoie)(1)))
      if(rueTable.isDefined){
        return rueTable
      } else {
        iVoie += 1
      }
    } while (iVoie < reglesVoies.size)
    None
  }

  def nouvelleRegle(objet: String,
                           nom: String,
                           regles: List[List[Any]],
                           recherche: (String, List[List[String]]) => Option[String]) : (Option[Any],List[List[Any]]) = {
    var retour: Option[String] = None
    var nouvellesRegles = regles
    do {
      retour = Dialog.showInput(null, nom, "Saisie d'une règle "+objet, Message.Plain,
        Swing.EmptyIcon, Nil, nom + "|")
      if (retour.isDefined) {
        var tableChaines = retour.get.split('|')
        if(tableChaines.length == 1) tableChaines = Array(tableChaines.head,"")
        if (tableChaines.length == 2) {
          val sipCommune = recherche(nom, List(tableChaines.toList))
          if (sipCommune.isDefined) {
            retour = None
            nouvellesRegles = nouvellesRegles :+ tableChaines.toList
            writerRegles.writeRow(List(objet, tableChaines(0), tableChaines(1)))
            writerRegles.flush()
            return(sipCommune,nouvellesRegles)
          }
        }
      }
    } while (retour.isDefined)
    (None,nouvellesRegles)
  }
  // lecture des paramètre


  var listeFichiers = List[String]()
  val fichierParam = "bailleur.par"
  if(new File(fichierParam).exists()){
    listeFichiers = Source.fromFile(fichierParam).getLines().toList
  }

  // initialisation du fichier du bailleur

  val fichierBailleur = if(listeFichiers.isEmpty)
    choosePlainFile("Fichier du bailleur")
  else
    Some(new File(listeFichiers.head))
  if(fichierBailleur.isEmpty) System.exit(-1)
  val readerBailleur = CSVReader.open(fichierBailleur.get)
  val bailleur = readerBailleur.all()
  val bailleurBuf = for(ligne<-bailleur) yield ligne.to[ListBuffer]
  val indexCommuneBailleur = bailleurBuf.head.indexOf("COMMUNE")
  if(indexCommuneBailleur < 0) {
    println("le fichier ne contient pas de colonne COMMUNE")
    System.exit(-1)
  }
  val indexVoie = bailleurBuf.head.indexOf("VOIE")
  if(indexVoie < 0) {
    println("le fichier ne contient pas de colonne VOIE")
    System.exit(-1)
  }
  if(bailleurBuf.head.contains("SIP")) {
    println("le fichier est déjà traité")
    System.exit(-1)
  }
  bailleurBuf(0) += "SIP"

  // initialisation du fichier des communes

  val fichierCommunes = if(listeFichiers.isEmpty)
    choosePlainFile("Fichier des communes")
  else
    Some(new File(listeFichiers(1)))
  if(fichierCommunes.isEmpty) System.exit(-1)
  val readerCommunes = CSVReader.open(fichierCommunes.get)
  val communes = readerCommunes.all()
  val indexCommuneCommunes = communes.head.indexOf("COMMUNE")
  val indexSipCommunes = communes.head.indexOf("Service des Impôts des Particuliers (SIP)")
  val tableCommunes = (for(c<-communes) yield c(indexCommuneCommunes) -> c(indexSipCommunes)).tail.toMap

  // initialisation du fichier des rues du chef-lieu

  val fichierRues = if(listeFichiers.isEmpty)
    choosePlainFile("Fichier des rues du chef-lieu")
  else
    Some(new File(listeFichiers(2)))
  if(fichierRues.isEmpty) System.exit(-1)
  val readerRuesToulouse = CSVReader.open(fichierRues.get)
  val rues = readerRuesToulouse.all()
  var tableRues: scala.collection.mutable.Map[String, List[(String,Stream[Int])]] = _
  for(i<- 1 until rues.size) {
    val rue = rues(i)(1).nomVoie
    if(tableRues == null) {
      tableRues = scala.collection.mutable.Map(rue._1 -> List((rues(i)(2),rue._2)))
    } else if (tableRues.contains(rue._1)) {
      tableRues(rue._1) ::= (rues(i)(2),rue._2)
    } else {
      tableRues(rue._1) = List((rues(i)(2),rue._2))
    }
  }

  if(listeFichiers.isEmpty){
    val pw = new PrintWriter("bailleur.par")
    pw.write(fichierBailleur.get.getPath+"\n"+fichierCommunes.get.getPath+"\n"+fichierRues.get.getPath)
    pw.close()
  }
  // initialisation du fichier des régles de remplacement

  val nomFichierRegles = fichierBailleur.get.getPath.replace(".csv","_regles.csv")
  val fichierRegles = new File(nomFichierRegles)
  var reglesCommunes = List(List("",""))
  var reglesVoies = List(List("",""))
  var reglesNumeros = List(List("",""))
  var writerRegles: CSVWriter = _
  if(fichierRegles.exists()) {
    val readerRegles = CSVReader.open(fichierRegles)
    val regles = readerRegles.all.tail
    reglesCommunes = reglesCommunes ++ regles.filter(_(0).toUpperCase=="COMMUNE").map(p => List(p(1),p(2)))
    reglesVoies = reglesVoies ++ regles.filter(_(0).toUpperCase=="VOIE").map(p => List(p(1),p(2)))
    reglesNumeros = reglesNumeros ++ regles.filter(_(0).toUpperCase=="NUMERO").map(p => List(p(1),p(2)))
    writerRegles = CSVWriter.open(fichierRegles,append = true)
  } else {
    writerRegles = CSVWriter.open(fichierRegles,append = true)
    writerRegles.writeRow(List("COLONNE","VALEUR BAILLEUR","VALEUR REFERENTIEL"))
    writerRegles.flush()
  }
  for(i <- 1 until bailleurBuf.size) {
    val commune = bailleurBuf(i)(indexCommuneBailleur).toUpperCase()
    var sipCommune = rechercheSipCommune(commune, reglesCommunes)
    if(sipCommune.isEmpty) {
      val retour = nouvelleRegle("COMMUNE",commune,reglesCommunes,rechercheSipCommune)
      sipCommune = retour._1.asInstanceOf[Option[String]]
      reglesCommunes = retour._2.asInstanceOf[List[List[String]]]
    }
    if(sipCommune.isEmpty) bailleurBuf(i) += "communeInconnue"
    else if (!sipCommune.get.contains("voir")) bailleurBuf(i) += sipCommune.get
    else {
      val ruechaine = bailleurBuf(i)(indexVoie).replaceAll("é","e").replaceAll("è","e").replaceAll("ç","c").toUpperCase
      var sipVoie = rechercheSipVoie(ruechaine, reglesVoies)
      if(sipVoie.isEmpty) {
        var retour: Option[String] = None
        do {
          retour = Dialog.showInput(null, commune, "Saisie d'une règle VOIE", Message.Plain,
            Swing.EmptyIcon, Nil, ruechaine + "|")
          if (retour.isDefined) {
            var tableChaines = retour.get.split('|')
            if (tableChaines.length == 1) tableChaines = Array(tableChaines.head, "")
            if (tableChaines.length == 2) {
              sipVoie = rechercheSipVoie(ruechaine, List(tableChaines.toList))
              if (sipVoie.isDefined) {
                retour = None
                reglesVoies = reglesVoies :+ tableChaines.toList
                writerRegles.writeRow(List("VOIE", tableChaines(0), tableChaines(1)))
                writerRegles.flush()
              }
            }
          }
        } while (retour.isDefined)
      }
      if(sipVoie.isEmpty) bailleurBuf(i) += "voieInconnue"
      else if (sipVoie.get.size == 1) bailleurBuf(i) += sipVoie.get.head._1
      else {
        val numeroRueC = bailleurBuf(i)(indexVoie - 1)
        var numeroRue = if (numeroRueC.matches("""\d+""")) numeroRueC.toInt else 0
        if(numeroRue == 0) {
          var iNumero = 0
          var numeroTrouve = false
          do {
            if (ruechaine.matches(reglesNumeros(iNumero).head)) {
              numeroRue = reglesNumeros(iNumero)(1).toInt
              numeroTrouve = true
            } else iNumero += 1
          } while (!numeroTrouve && iNumero < reglesNumeros.size)
        }
        val rueRef = sipVoie.get
        val rueRefs = rueRef.filter(_._2.contains(numeroRue))
        if(rueRefs.isEmpty) {
          bailleurBuf(i) += "aLaMain"
        } else {
          bailleurBuf(i) += rueRefs.head._1
        }
      }
    }
  }
  val entete = bailleurBuf.head
  val indexSipBailleur = entete.indexOf("SIP")
  val corps = bailleurBuf.tail.sortWith(_ (indexSipBailleur)< _(indexSipBailleur))
  var sip = ""
  var writerBailleur: CSVWriter = _
  var supprimerALaMain = true
  var supprimerCommuneInconnue = true
  var supprimerVoieInconnue = true
  for(ligne<-corps) {
    if(ligne(indexSipBailleur)!=sip) {
      if(writerBailleur != null) writerBailleur.close()
      sip = ligne(indexSipBailleur)
      if(sip == "aLaMain") supprimerALaMain = false
      if(sip == "communeInconnue") supprimerCommuneInconnue = false
      if(sip == "voieInconnue") supprimerVoieInconnue = false
      writerBailleur = CSVWriter.open(new File("bailleur_complete_" + sip +".csv"))
      writerBailleur.writeRow(entete)
    }
    writerBailleur.writeRow(ligne)
  }
  writerBailleur.close()
  if (supprimerALaMain) new File("bailleur_complete_aLaMain.csv").delete()
  if (supprimerCommuneInconnue) new File("bailleur_complete_communeInconnue.csv").delete()
  if (supprimerVoieInconnue) new File("bailleur_complete_voieInconnue.csv").delete()
  if (supprimerCommuneInconnue && supprimerVoieInconnue && supprimerALaMain) new File("bailleur.par").delete()
}
