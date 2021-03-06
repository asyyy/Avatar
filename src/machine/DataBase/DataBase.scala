package machine
import scala.io.Source
import java.io.PrintWriter
import scala.xml._

object DataBase extends DataBaseTrait {

  val DBxml = XML.loadFile("./doc/vAr.xml")
  // \"organization"\"adresses"
  var temp = (DBxml \\ "opendata" \\ "answer" \\ "data")

  val DBxmlReduce = XML.loadFile("./doc/vArTest.xml")
  var tempReduce = (DBxmlReduce \\ "opendata" \\ "answer" \\ "data")

  var baseTexte = Source.fromFile("./doc/DonneesInitiales.txt").getLines.toList
  var baseTexteInv = Source.fromFile("./doc/DonneesInitialesInverse.txt").getLines.toList
  //var baseTexteEmoji = Source.fromFile("./doc/Emojis.txt").getLines.toList
  var baseTexteXML = Source.fromFile("./doc/DBxml.txt").getLines.toList
  var newBase: List[(String, String)] = Nil;


  //Construit la base de données au format List de couple (clé,valeur) 
  //Clé -> Nom de l'organisme/ du lieux
  //Valeur -> Adresse
  def constructionBase(base: List[String]): List[(String, String)] = {
    base match {
      case Nil    => Nil
      case a :: b => newBase ::: (a.slice(1, a.indexOf(";")), a.slice(a.indexOf(";") + 1, a.length())) :: constructionBase(b)
    }
  }

  //Construit la base de données au format List de couple (clé,valeur) 
  //Clé -> Adresse
  //Valeur -> Nom de l'organisme/ du lieux
  def constructionBaseInverse(base: List[(String, String)]): List[(String, String)] = {
    base match {
      case Nil    => Nil
      case a :: b => (a._2,a._1) :: constructionBaseInverse(b)
    }
  }

  //Extrait les informations utiles de la base de données XML à l'aide de requêtes XPath
  //Et les stock dans le fichier DBxml.txt
  def constructionBDxml() {
    var total = ""
    for {
      orga <- DBxml \\ "organization"
      name <- orga \ "name"
      adress <- orga \ "addresses"
      adr <- adress \ "address"
      str <- adr \ "street"
      strName <- str \ "name"
      strNumber <- str \ "number"

    } yield {
      if (strName.text != "" ) total += " " + name.text + ";" + strNumber.text + " " + strName.text + "\n"
    }
    println(total)
    new PrintWriter("./doc/DBxml.txt") { write(total); close }

  }

  
  //Construit le fichier texte : DBxml.txt à partir de vAr.xml
  //constructionBDxml()

  //Construction grosse DB
  var base = constructionBase(baseTexteXML)
  var baseInverse = constructionBaseInverse(base)
  
  //Construction base réduite
  var baseTxt = constructionBase(baseTexte)
  var baseTxtInv = constructionBase(baseTexteInv)
  
  //var baseEmoji = constructionBase(baseTexteEmoji)
  //val baseXML = constructionBase(baseTexteXML)

}
