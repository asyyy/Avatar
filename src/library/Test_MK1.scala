package library
import scala.io.StdIn
import java.io.FileWriter

object Test_MK1 extends App {
  val exemple = Tag("html", List(),
    List(
      Tag("head", List(),
      List(
        Tag("meta", List(("content", "text/html"), ("charset", "iso-8859-1")), List()),
        Tag("title", List(), List(Text("MySpace Lien"))))),
      Tag("body", List(), List(
        Text("&nbsp"),
        Tag("center", List(), List(
          Tag("a", List(("href", "http://www.irisa.fr")),
            List(
              Text("Lien j'aime le chocolat et j'aime encore plus la GEN."),
              Tag("img", List(), List(Text("")))))))))))

  val expr = Word("")
  val expr1 = And(Word("Lien"), Or(Word("MyPae"), Word("MyPage")))
  val expr2 = And(Word("Lien"), Or(Word("MyPae"), Word("MySpace")))
  val expr3 = Word("Lien")
  val expr4 = Or(Word("&nbsp"), Word("beat"))

  println(FHtml.textMatch(exemple.children))
  println(FHtml.filtreHtml(exemple, expr))
  println(FHtml.filtreHtml(exemple, expr1))
  println(FHtml.filtreHtml(exemple, expr2))
  println(FHtml.filtreHtml(exemple, expr3))
  println(FHtml.filtreHtml(exemple, expr4))
}


object Application extends App {

  def expMatch(e: Expression): String = {
    e match {
      case Word(x)   => x
      case And(x, _) => expMatch(x)
      case Or(x, _)  => expMatch(x)
    }
  }

  
  val url1 = "https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords="
  val url2 = "&cat_1=&geosearch_text=&searchGeoId=0"
  val exp = ExpressionParser.readExp
  val w = expMatch(exp)
  val url = url1 + w + url2
  println(url)
  val html = HtmlVersString.process(Prod.resultat2html(Analyse.resultats(url, exp)))
  println(html)
 
  
}