import java.io.{File, PrintWriter}
import javax.swing.filechooser.FileNameExtensionFilter
import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
  implicit class UtilitairesString(s: String) {
    def decoupe(longueur: Int) : String = {
      val troncons = this.s.grouped(longueur)
      troncons.foldLeft(""){(chaine,troncon) => chaine + troncon + "\n"}
    }
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

  def litFichier(i :Int, titre: String): (List[List[String]],Option[File]) = {
    val fichier = if (listeFichiers.isEmpty)
      choosePlainFile("Fichier " + titre)
    else
      Some(new File(listeFichiers(i)))
    if (fichier.isEmpty) System.exit(-1)
    val reader = CSVReader.open(fichier.get)
    try {
      (reader.all(), fichier)
    } catch {
      case e: Exception =>
        Dialog.showMessage(null,("Erreur " + e + e.getStackTrace.toString).decoupe(40),"Erreur de lecture",Message.Error)
        System.exit(-1)
        (null,null)
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

  def recherche(listeChaine: List[String],
                          reglesObjet: List[List[String]],
                          table: Map[String,Any]): Option[Any] = {
    for(regle<-reglesObjet){
      val retour = table.get(listeChaine.head.replaceFirst(regle.head,regle(1)))
      if(retour.isDefined) return retour
    }
    None
  }

  def rechercheNumeroVoie(listeChaine: List[String],
                          reglesNumeros: List[List[String]],
                          table: Map[String,Any]): Option[Any] = {
    val modele = """(\d+).*""".r
    var numeroRueC = ""
    listeChaine(0) match {
      case modele(nbc) => return Some(nbc.toInt)
      case _ =>
    }
    for(regle<-reglesNumeros){
      if(listeChaine(1).matches(regle.head))
        if(regle(1) == "") return None
        else return Some(regle(1).toInt)
    }
    None
  }

  def nouvelleRegle(i: Int, objet: String, listeChaine: List[String], regles: List[List[String]],
                    table: Map[String,Any],
                    recherche: (List[String], List[List[String]], Map[String,Any]) => Option[Any]):
                    (Option[Any],List[List[String]]) = {
    var retour: Option[String] = None
    var nouvellesRegles = regles
    var texte = listeChaine.head+"\n"
    //calcul des distances
    if(listeChaine.size == 1){
      val x = new LevenshteinDistance()
      val vect = table.map(p => (p._1,x.apply(p._1,listeChaine.head)))
      texte = texte + "Désignations proches\n" +
                      vect.toVector.sortWith(_._2 < _._2).slice(0,9).map(_._1).reduceLeft(_ + "\n" + _) +
                      "\n"
    }
    do {
      retour = Dialog.showInput(null, texte, "Saisie d'une règle "+objet+ " ligne " + i.toString, Message.Plain,
        Swing.EmptyIcon, Nil, listeChaine.head + "|")
      if (retour.isDefined) {
        var tableChaines = retour.get.split('|')
        if(tableChaines.length == 1) tableChaines = Array(tableChaines.head,"")
        if (tableChaines.length == 2) {
          val resultat = recherche(listeChaine, List(tableChaines.toList),table)
          if (resultat.isDefined) {
            retour = None
            nouvellesRegles = nouvellesRegles :+ tableChaines.toList
            writerRegles.writeRow(List(objet, tableChaines(0), tableChaines(1)))
            writerRegles.flush()
            return(resultat,nouvellesRegles)
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

  var retour = litFichier(0,"du Bailleur")
  val (bailleur, fichierBailleur) = (retour._1, retour._2)
  val bailleurBuf = for(ligne<-bailleur) yield ligne.to[ListBuffer]
  val indexCommuneBailleur = bailleurBuf.head.indexOf("COMMUNE")
  if(indexCommuneBailleur < 0) {
    Dialog.showMessage(null,"Le fichier du bailleur ne contient\n pas de colonne COMMUNE","Erreur de fichier",Message.Error)
    System.exit(-1)
  }
  val indexVoie = bailleurBuf.head.indexOf("VOIE")
  if(indexVoie < 0) {
    Dialog.showMessage(null,"Le fichier du bailleur ne contient\n pas de colonne VOIE","Erreur de fichier",Message.Error)
    System.exit(-1)
  }
  if(bailleurBuf.head.contains("SIP")) {
    Dialog.showMessage(null,"Le fichier du bailleur est déjà traité","Erreur de fichier",Message.Error)
    System.exit(-1)
  }
  bailleurBuf(0) += "SIP"

  // initialisation du fichier des communes

  retour = litFichier(1,"des Communes")
  val (communes, fichierCommunes) = (retour._1, retour._2)
  val indexCommuneCommunes = communes.head.indexOf("COMMUNE")
  val indexSipCommunes = communes.head.indexOf("Service des Impôts des Particuliers (SIP)")
  val tableCommunes = (for(c<-communes) yield c(indexCommuneCommunes) -> c(indexSipCommunes)).tail.toMap

  // initialisation du fichier des rues du chef-lieu

  retour = litFichier(2,"des Rues du chef-lieu")
  val (rues, fichierRues) = (retour._1, retour._2)
  var tableRuesM = mutable.Map[String, List[(String,Stream[Int])]]()
  for(i<- 1 until rues.size) {
    val rue = rues(i)(1).nomVoie
    if(tableRuesM.get(rue._1).isEmpty) {
      tableRuesM(rue._1) = List((rues(i)(2),rue._2))
    } else {
      tableRuesM(rue._1) ::= (rues(i)(2),rue._2)
    }
  }
  val tableRues = tableRuesM.toMap

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

  // Traitement du fichier des bailleurs

  var communesNonTrouvees = mutable.Set[String]()
  var ruesNonTrouvees = mutable.Set[String]()
  var numerosNonTrouves = mutable.Set[List[String]]()

  for(i <- 1 until bailleurBuf.size) {
    val commune = bailleurBuf(i)(indexCommuneBailleur).toUpperCase().trim
    var sipCommune = recherche(List(commune), reglesCommunes, tableCommunes).asInstanceOf[Option[String]]
    if(sipCommune.isEmpty && !communesNonTrouvees.contains(commune)) {
      val retour = nouvelleRegle(i,"COMMUNE",List(commune), reglesCommunes, tableCommunes, recherche)
      sipCommune = retour._1.asInstanceOf[Option[String]]
      reglesCommunes = retour._2
    }
    if(sipCommune.isEmpty) {
      bailleurBuf(i) += "communeInconnue"
      communesNonTrouvees += commune
    }
    else if (!sipCommune.get.contains("voir")) bailleurBuf(i) += sipCommune.get
    else {
      val ruechaine = bailleurBuf(i)(indexVoie).replaceAll("é","e").replaceAll("è","e").replaceAll("ç","c").toUpperCase.trim
      var sipVoie = recherche(List(ruechaine), reglesVoies, tableRues).asInstanceOf[Option[List[(String,Stream[Int])]]]
      if(sipVoie.isEmpty && !ruesNonTrouvees.contains(ruechaine)) {
        val retour = nouvelleRegle(i,"VOIE", List(ruechaine), reglesVoies, tableRues, recherche)
        sipVoie = retour._1.asInstanceOf[Option[List[(String,Stream[Int])]]]
        reglesVoies = retour._2
      }
      if(sipVoie.isEmpty) {
        bailleurBuf(i) += "voieInconnue"
        ruesNonTrouvees += ruechaine
      }
      else if (sipVoie.get.size == 1) bailleurBuf(i) += sipVoie.get.head._1
      else {
        var numeroChaine = bailleurBuf(i)(indexVoie-1).trim()
        var numeroVoie = rechercheNumeroVoie(List(numeroChaine, ruechaine), reglesNumeros, Map())
        if(numeroVoie.isEmpty && !numerosNonTrouves.contains(List(numeroChaine,ruechaine))) {
          val retour = nouvelleRegle(i,"NUMERO", List(numeroChaine, ruechaine), reglesNumeros, Map(), rechercheNumeroVoie)
          numeroVoie = retour._1.asInstanceOf[Option[Int]]
          reglesNumeros = retour._2
        }
        if(numeroVoie.isEmpty) {
          bailleurBuf(i) += "aLaMain"
          numerosNonTrouves += List(numeroChaine,ruechaine)
        } else {
          val rueRef = sipVoie.get
          val rueRefs = rueRef.filter(_._2.contains(numeroVoie.get))
          if (rueRefs.isEmpty) {
            bailleurBuf(i) += "aLaMain"
          } else {
            bailleurBuf(i) += rueRefs.head._1
          }
        }
      }
    }
  }

  // écriture des fichiers éclatés

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
      writerBailleur = CSVWriter.open(new File(fichierBailleur.get.getPath.replace(".csv","_"+sip+".csv")))
      writerBailleur.writeRow(entete)
    }
    writerBailleur.writeRow(ligne)
  }
  writerBailleur.close()

  // suppression des fichiers d'erreurs s'il n'y en a pas

  if (supprimerALaMain) new File("bailleur_complete_aLaMain.csv").delete()
  if (supprimerCommuneInconnue) new File("bailleur_complete_communeInconnue.csv").delete()
  if (supprimerVoieInconnue) new File("bailleur_complete_voieInconnue.csv").delete()
  if (supprimerCommuneInconnue && supprimerVoieInconnue && supprimerALaMain) new File("bailleur.par").delete()
}