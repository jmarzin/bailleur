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
    def nomVoie : (String,List[Int]) = {
      val debutNom = s.indexOf("(")
      if (debutNom < 0) {
        (s,List())
      } else {
        val finNom = (if (s.indexOf(")") < 0) s.size else s.indexOf(")"))
        val rue = s.substring(debutNom + 1, finNom) + " " + s.substring(0, debutNom - 1) + (if(finNom == s.size) "" else s.substring(finNom + 1))
        val debut2 = rue.indexOf("(")
        if(debut2 < 0) {
          (rue, List())
        } else {
          val fin2 = (if (rue.indexOf(")") < 0) rue.size else rue.indexOf(")"))
          val intervalle = (if(fin2 == debut2 + 1) "" else rue.substring(debut2+1,fin2))
          if (intervalle == "") {
            (rue.substring(0,debut2-1), List())
          } else {
            val conditions = intervalle.split(" et ")
            var ensemble: Set[Int] = Set()
            for(cond<-conditions) {
              val pattern1 = """(\d+) à la fin""".r
              try {
                val pattern1(debut) = cond.trim
                ensemble = ensemble.union(Range(debut.toInt, 10001, 2).toSet)
              }
              catch {
                case _: Throwable =>
              }
              val pattern2 = """(\d+) à (\d+)""".r
              try {
                val pattern2(debut, fin) = cond.trim
                ensemble = ensemble.union(Range(debut.toInt, fin.toInt + 1, 2).toSet)
              }
              catch {
                case _: Throwable =>
              }
              if (cond.trim.matches("[Ii]mpairs")) {
                ensemble = ensemble.union(Range(1, 10001, 2).toSet)
              }
              if (cond.trim.matches("[Pp]airs")) {
                ensemble = ensemble.union(Range(2, 10001, 2).toSet)
              }
            }
            (rue.substring(0,debut2-1), ensemble.toList.sorted)
          }
        }
      }
    }
  }
  val readerBailleur = CSVReader.open(new File("bailleur.csv"))
  val bailleur = readerBailleur.all()
  val bailleurBuf = for(ligne<-bailleur) yield ligne.to[ListBuffer]
  if(!bailleurBuf(0).contains("COMMUNE")){
    println("le fichier ne contient pas de commune")
    System.exit(-1)
  }
  if(bailleurBuf(0).contains("SIP")) {
    println("le fichier est déjà traité")
    System.exit(-1)
  }
  bailleurBuf(0) += "SIP"

  val indexCommuneBailleur = bailleurBuf(0).indexOf("COMMUNE")

  val readerCommunes = CSVReader.open(new File("communes.csv"))
  val communes = readerCommunes.all()

  val indexCommuneCommunes = communes(0).indexOf("COMMUNE")
  val indexSipCommunes = communes(0).indexOf("Service des Impôts des Particuliers (SIP)")

  val tableSip = (for(c<-communes) yield c(indexCommuneCommunes) -> c(indexSipCommunes)).tail.toMap

  val indexRue = bailleurBuf(0).indexOf("RUE")

  val readerRuesToulouse = CSVReader.open(new File("RUES DE TOULOUSE.csv"))
  val rues = readerRuesToulouse.all()

  var tableRues: scala.collection.mutable.Map[String, List[(String,List[Int])]] = _

  for(i<- 1 to rues.size-1) {
    val rue = rues(i)(1).nomVoie
    if(tableRues == null) {
      tableRues = scala.collection.mutable.Map(rue._1 -> List((rues(i)(2),rue._2)))
    } else if (tableRues.contains(rue._1)) {
      tableRues(rue._1) ::= (rues(i)(2),rue._2)
    } else {
      tableRues(rue._1) = List((rues(i)(2),rue._2))
    }
  }

  for(i <- 1 to bailleurBuf.size -1) {
    val comm = bailleurBuf(i)(indexCommuneBailleur)
    if(comm == "TOULOUSE") {
      val subst = List(("",""),("IMPASSE DU ","IMP "),("IMPASSE ","IMP "),("CHEMIN ","CHEM "),("RUE DE ","RUE "),
        ("FR.  DEVEZE","FRANCOIS DEVEZE"),("AVENUE ","AVE "),("BD DE L'","BLD DE L' "),("RUE ","RUE DE "),
        ("MAURICE MELAT", "MAURICE MELA"),("IMPASSE DU GENERAL AUBUGEOIS","RUE DU GENERAL AUBUGEOIS"),
        ("ALLEE ","ALLS "),("RUE DU ","RUE "))
      var rueTrouvee = false
      var irue = 0
      do {
        var rueTable = tableRues.get(bailleurBuf(i)(indexRue).replaceFirst(subst(irue)._1, subst(irue)._2))
        if (rueTable != None) {
          val rueRef = rueTable.get
          if (rueRef.size == 1) {
            bailleurBuf(i) += rueRef.head._1
          } else {
            var numero = bailleurBuf(i)(indexRue - 1)
            var numeroTrouve = false
            var jnum = 0
            do {
              if (rueRef(jnum)._2.contains(numero.toInt)) {
                numeroTrouve = true
              } else {
                jnum += 1
              }
            } while (!numeroTrouve && jnum < rueRef.size)
            if (numeroTrouve) {
              bailleurBuf(i) += rueRef(jnum)._1
            } else {
              bailleurBuf(i) += "à traiter à la main"
            }
          }
          rueTrouvee = true
        } else {
          irue += 1
        }
      } while (!rueTrouvee && irue < subst.size)
      if(!rueTrouvee) {
        bailleurBuf(i) += "rue non trouvée"
      }
    } else {
      bailleurBuf(i) += tableSip.getOrElse(comm,
        tableSip.getOrElse(comm.replaceFirst("ST ", "SAINT-"),
          tableSip.getOrElse(comm.replaceFirst(" FONSEGRIVES", ""), "")))
    }
  }
  val writerBailleur = CSVWriter.open(new File("bailleur_complete.csv"))
  writerBailleur.writeAll(bailleurBuf)
}
