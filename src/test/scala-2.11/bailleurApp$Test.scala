import org.scalatest.Matchers
import bailleurApp.Voie

/**
  * Created by jacquesmarzin on 04/02/2017.
  */
class bailleurApp$Test extends org.scalatest.FunSpec with Matchers {
  describe("La fonction nomVoie") {

    it("déplace le nom de la voie en tête") {
      "Henri Tagnères (rue)".nomVoie should be(("rue Henri Tagnères",List()))
    }

    it("renvoit la chaîne s'il n'y a pas de parenthèses") {
      "Henri Tagnères".nomVoie should be (("Henri Tagnères",List()))
    }

    it("tient compte de l'absence de parenthèse )") {
      "Henri Tagnères (rue".nomVoie should be(("rue Henri Tagnères",List()))
    }

    it("supprime les deuxièmes parenthèses") {
      "Henri Tagnères (rue) ()".nomVoie should be("rue Henri Tagnères",List())
    }

    it("traite les formules n à m") {
      "Henri Tagnères (rue) (1 à 65)".nomVoie should be("rue Henri Tagnères",(1 to 65 by 2).toList.sorted)
      "Henri Tagnères (rue) (2 à 12)".nomVoie should be("rue Henri Tagnères",(2 to 12 by 2).toList.sorted)
    }

    it("traite les formules n à la fin") {
      "Henri Tagnères (rue) (1 à la fin)".nomVoie should be("rue Henri Tagnères",Range(1,10001,2).toList.sorted)
    }

    it("traite les formules pairs et impairs") {
      "Henri Tagnères (rue) (pairs)".nomVoie should be("rue Henri Tagnères",(2 to 10000 by 2).toList.sorted)
      "Henri Tagnères (rue) (Pairs)".nomVoie should be("rue Henri Tagnères",(2 to 10000 by 2).toList.sorted)
      "Henri Tagnères (rue) (impairs)".nomVoie should be("rue Henri Tagnères",(1 to 10000 by 2).toList.sorted)
      "Henri Tagnères (rue) (Impairs)".nomVoie should be("rue Henri Tagnères",(1 to 10000 by 2).toList.sorted)
    }

    it("traite les jonctions par et") {
      "Henri Tagnères (rue) (pairs et impairs)".nomVoie should be("rue Henri Tagnères",(2 to 10000 by 2).toStream.append((1 to 10000 by 2).toStream))
      "Henri Tagnères (rue) (1 à 11 et 2 à 8)".nomVoie should be("rue Henri Tagnères",(1 to 11 by 2).toStream.append((2 to 8 by 2).toStream))
    }
  }
}
