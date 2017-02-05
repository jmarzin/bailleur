import java.io.File
import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import scala.collection.mutable.ListBuffer

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
      val rue = s.substring(debutNom + 1, finNom) + " " + s.substring(0, debutNom - 1) + (if(finNom == s.length) "" else s.substring(finNom + 1))
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
      (rue.substring(0,debut2-1), ensemble)
    }
  }

  val readerBailleur = CSVReader.open(new File("bailleur.csv"))
  val bailleur = readerBailleur.all()
  val bailleurBuf = for(ligne<-bailleur) yield ligne.to[ListBuffer]
  if(!bailleurBuf.head.contains("COMMUNE")){
    println("le fichier ne contient pas de commune")
    System.exit(-1)
  }
  if(bailleurBuf.head.contains("SIP")) {
    println("le fichier est déjà traité")
    System.exit(-1)
  }
  //noinspection ZeroIndexToHead
  bailleurBuf(0) += "SIP"

  val indexCommuneBailleur = bailleurBuf.head.indexOf("COMMUNE")

  val readerCommunes = CSVReader.open(new File("communes.csv"))
  val communes = readerCommunes.all()

  val indexCommuneCommunes = communes.head.indexOf("COMMUNE")
  val indexSipCommunes = communes.head.indexOf("Service des Impôts des Particuliers (SIP)")

  val tableSip = (for(c<-communes) yield c(indexCommuneCommunes) -> c(indexSipCommunes)).tail.toMap

  val indexRue = bailleurBuf.head.indexOf("RUE")

  val readerRuesToulouse = CSVReader.open(new File("RUES DE TOULOUSE.csv"))
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

  for(i <- 1 until bailleurBuf.size) {
    val comm = bailleurBuf(i)(indexCommuneBailleur)
    if(comm == "TOULOUSE") {
      val subst = List(("",""),("IMPASSE DU ","IMP "),("IMPASSE ","IMP "),("CHEMIN ","CHEM "),("RUE DE ","RUE "),
        ("FR.  DEVEZE","FRANCOIS DEVEZE"),("AVENUE ","AVE "),("BD DE L'","BLD DE L' "),("RUE ","RUE DE "),
        ("MAURICE MELAT", "MAURICE MELA"),("IMPASSE DU GENERAL AUBUGEOIS","RUE DU GENERAL AUBUGEOIS"),
        ("ALLEE ","ALLS "),("RUE DU ","RUE "))
      var rueTrouvee = false
      var irue = 0
      do {
        val rueTable = tableRues.get(bailleurBuf(i)(indexRue).replaceFirst(subst(irue)._1, subst(irue)._2))
        if (rueTable.isDefined) {
          val rueRef = rueTable.get
          if (rueRef.size == 1) {
            bailleurBuf(i) += rueRef.head._1
          } else {
            val rueRefs = rueRef.filter(_._2.contains(bailleurBuf(i)(indexRue - 1).toInt))
            if(rueRefs.isEmpty) {
              bailleurBuf(i) += "aLaMain"
            } else {
              bailleurBuf(i) += rueRefs.head._1
            }
          }
          rueTrouvee = true
        } else {
          irue += 1
        }
      } while (!rueTrouvee && irue < subst.size)
      if(!rueTrouvee) {
        bailleurBuf(i) += "rueInconnue"
      }
    } else {
      bailleurBuf(i) += tableSip.getOrElse(comm,
        tableSip.getOrElse(comm.replaceFirst("ST ", "SAINT-"),
          tableSip.getOrElse(comm.replaceFirst(" FONSEGRIVES", ""), "communeInconnue")))
    }
  }
  val entete = bailleurBuf.head
  val indexSipBailleur = entete.indexOf("SIP")
  val corps = bailleurBuf.tail.sortWith(_ (indexSipBailleur)< _(indexSipBailleur))
  var sip = ""
  var writerBailleur: CSVWriter = _
  for(ligne<-corps) {
    if(ligne(indexSipBailleur)!=sip) {
      if(writerBailleur != null) writerBailleur.close()
      sip = ligne(indexSipBailleur)
      writerBailleur = CSVWriter.open(new File("bailleur_complete_" + sip +".csv"))
      writerBailleur.writeRow(entete)
    }
    writerBailleur.writeRow(ligne)
  }
  writerBailleur.close()
}
