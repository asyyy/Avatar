package library

trait AnalysePage {
  /**
   * A partir d'une URL de requête sur le site de référence et d'une expression exp,
   * retourne de pages issues de la requête et satisfaisant l'expression.
   *
   * @param url l'URL de la requête sur le site de référence
   * @param exp l'expression à vérifier sur les pages trouvées
   * @return la liste des couples (titre,ref) où ref est l'URL d'une page
   * satisfaisant l'expression et titre est son titre.
   */
  def resultats(url: String, exp: Expression): List[(String, String)]
}

object Analyse extends AnalysePage {
  /**
   * @param ls une liste de String des Url
   * @return la liste des éléments de ls convertit en Html
   */
  def stringToHtml(ls: List[String]): List[Html] = {

    var i: Int = 0 // La pour mieux comprendre : A SUPPRIMER
    var lHtml: List[Html] = List()
    for (e <- ls) {
      //if (e.compareTo("https://www.vivastreet.com") != 0) {
        println(e) // La pour mieux comprendre : A SUPPRIMER
        i = i + 1 // La pour mieux comprendre : A SUPPRIMER
        println("i = " + i) // Pour mieux comprendre : A SUPPRIMER
        lHtml = lHtml ++ List(UrlProcessor.fetch(e))
      //}
    }
    lHtml
  }

  /**
   * @param lh une liste d'Html
   * @param ls une liste de String
   * @param exp une expression
   * @return la liste des éléments de lh et ls qui respecte le filtreHtml
   * 				 avec l'expression exp
   */
  def checkFiltre(lh: List[Html], ls: List[String], exp: Expression): List[(String, Html)] = {
    var lCouple: List[(String, Html)] = List()
    println("test1")
    for (e <- lh.zipWithIndex) {

      //e._1 = Html - e._2 = Index

      if (FHtml.filtreHtml(e._1, exp)) {
        lCouple = lCouple ++ List((ls(e._2), e._1))
      }
    }
    lCouple
  }
  /**
   * Fonction intermédiaire de findTitle
   * @param t un Html
   * @return un String, le text de l'Html
   */
  def recupTitle(t: Html): String = {
    t match {
      case Text(x) => x
      case _       => "None1"
    }
  }

  /**
   * @param l : un Html
   * @return un String, le titre de l'Html
   */
  def findTitle(h: Html): String = {
    h match {
      case Tag("title", _, children :: m) => recupTitle(children)
      case Tag(_, _, children :: m)       => findTitle(children)
      case _                              => "None2"
    }
  }
  /**
   * @param lc une List[(String,Html)]
   * @return List[(String,String)] contenant d'abord le titre de Html puis l'URL de l'Html
   */
  def putTheTitle(lc: List[(String, Html)]): List[(String, String)] = {
    var res: List[(String, String)] = List()
    for (e <- lc) {
      res = res ++ List((findTitle(e._2), e._1))
    }
    res
  }
  /**
   * A partir d'une URL de requête sur le site de référence et d'une expression exp,
   * retourne de pages issues de la requête et satisfaisant l'expression.
   *
   * @param url l'URL de la requête sur le site de référence
   * @param exp l'expression à vérifier sur les pages trouvées
   * @return la liste des couples (titre,ref) où ref est l'URL d'une page
   * satisfaisant l'expression et titre est son titre.
   */
  def resultats(url: String, exp: Expression): List[(String, String)] = {

    /*
			 * Etape 1 : FiltreAnnonce
			 * 		On récupère toutes les annonces du site de référence qu'on stock
			 *  	dans lURLs.
			 */

    val lURLs: List[String] = Filtre.filtreAnnonce(UrlProcessor.fetch(url))
    if (lURLs.length != 0) println("lURLs not empty, length = " + lURLs.length)
    println("lURLs length = " + lURLs.length)

    /*
			 * Etape 2 : stringToHtml
			 * 		On traduit toutes les éléments de lURLs en Html
			 * 		qu'on stock dans lHtml
			 */
    val lHtml: List[Html] = stringToHtml(lURLs)
    println("lHtml length = " + lHtml.length)

    /*
			 * Etape 3 : checkFiltre
			 * 		On check si les éléments de lHtml vérifie l'expression,
			 * 		si true, on la stock dans lCouple, qui contient le lien URL(String) et son équivalent HTML
			 * 		sinon on ne l'ajoute pas
			 */
    val lCouple: List[(String, Html)] = checkFiltre(lHtml, lURLs, exp)
    println("lCouple length = " + lCouple.length)

    /*
			 * Etape 4 : putTheTile
			 * 		On utilise l'Html du couple d'un élément de lCouple pour
			 * 		récupérer le titre de l'URL.
			 * 		On créé la liste lRes qui contient le title d'un URL, et l'équivalent HTML de l'URL
			 */
    val lRes: List[(String, String)] = putTheTitle(lCouple)
    println(lRes)
    lRes
  }

}

trait FiltrageURLs {
  /**
   * A partir d'un document Html h, rend la liste des URLs accessibles à partir
   * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
   * d'annonces du site de référence
   *
   * @param h le document Html
   * @return la liste des URLs d'annonces contenues dans h
   */
  def filtreAnnonce(h: Html): List[String]
}

object Filtre extends FiltrageURLs {
  /**
   * Fonction intermédiaire de filtrage
   * @param h un html
   * @return une liste de String des liens se trouvant dans l'html
   */
  def filtreAux(h: Html): List[String] = {
    h match {
      case Tag("a", attribute, children) => if (findHref(attribute).compareTo("") != 0) List(findHref(attribute)) ::: filtreInter(children) else filtreInter(children)
      case Tag(_, _, children)           => filtreInter(children)
      case _                             => Nil
    }
  }
  /**
   * Fonction intermédiaire de filtrage
   * @param l une liste d'html
   * @return une liste de String
   */
  def filtreInter(l: List[Html]): List[String] = {
    var res: List[String] = List()
    for (e <- l) {
      res = res ::: filtreAux(e)
    }
    res
  }
  /**
   * @param l une liste d'attribut
   *  @return la valeur du l'attribut href s'il existe
   */
  def findHref(l: List[(String, String)]): String = {
    l match {
      case Nil    => ""
      case x :: m => if (x._1.compareTo("href") == 0) x._2 else findHref(m)
    }
  }
  /**
   * A partir d'un document Html h, rend la liste des URLs accessibles à partir
   * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
   * d'annonces du site de référence
   *
   * @param h le document Html
   * @return la liste des URLs d'annonces contenues dans h
   */
  def filtreAnnonce(h: Html): List[String] = {
    Nil
  }
}

trait FiltrageHtml {
  /**
   * A partir d'un document Html h et d'une requête e, dit si le document
   * satisfait l'expression e
   *
   * @param h le document Html
   * @param e l'expression
   * @return true si le document satisfait l'expression e
   */
  def filtreHtml(h: Html, e: Expression): Boolean
  true
}

object FHtml extends FiltrageHtml {

  def textMatch(h: List[Html]): String = {
    h match {
      case Nil                      => ""
      case Tag("a", _, _) :: t      => textMatch(t)
      case Tag("script", _, _) :: t => textMatch(t)
      case Tag(_, _, x) :: t        => textMatch(x) + textMatch(t)
      case Text(text) :: t          => text + "\n" + textMatch(t)
    }
  }

  def verifExpression(text: String, e: Expression): Boolean = {
    e match {
      case Word(x)   => text.toLowerCase().contains(x.toLowerCase())
      case And(x, y) => verifExpression(text, x) && verifExpression(text, y)
      case Or(x, y)  => verifExpression(text, x) || verifExpression(text, y)
    }
  }

  def filtreHtml(h: Html, e: Expression): Boolean = {
    verifExpression(textMatch(h :: Nil), e)
  }

}

trait ProductionResultat {
  /**
   * A partir d'une liste de couples (titre,URL), produit un document Html, qui
   * liste les solutions sous la forme de liens cliquables
   *
   * @param l la liste des couples solution (titre,URL)
   * @return le document Html listant les solutions
   */
  def resultat2html(l: List[(String, String)]): Html
}

object Prod extends ProductionResultat {
  /**
   * A partir d'une liste de couples (titre,URL), produit un document Html, qui
   * liste les solutions sous la forme de liens cliquables
   *
   * @param l la liste des couples solution (titre,URL)
   * @return le document Html listant les solutions
   */
  def resultat2html(l: List[(String, String)]): Html = {
    var i: Int = 0
    var listUrl: List[Html] = Nil
    while (l.size > i) {
      listUrl = listUrl ::: List(Tag("a", List(("href", l.apply(i)._2)),
        List(Text(l.apply(i)._1), Tag("br", List(), List()))))
      println(l.apply(i)._1 + ": " + l.apply(i)._2)

      i = i + 1
    }

    val html: Html = Tag("html", List(),
      List(
        Tag("head", List(),
        List(
          Tag("meta", List(("content", "text/html"), ("charset", "\"ISO-8859-1\"")), List()),
          Tag("title", List(), List(Text("MyPage"))))),
        Tag("body", List(), List(
          Tag("left", List(), listUrl)))))

    html
  }
}
trait Html2String {
  /**
   * Produit la chaîne de caractère correspondant à un document Html
   *
   * @param h le document Html
   * @return la chaîne de caractère représentant h
   */
  def process(h: Html): String
}

object HtmlVersString extends Html2String {
  var exceptionListInBalise: List[String] = List("a", "img", "meta", "title");
  var exceptList: List[String] = List("img");
  val baliseOuvertList: List[String] = List();
  var index = -2;

  /**
   * @param h le document Html
   * @return la chaîne de caractère représentant h
   */
  def process(h: Html): String = {
    var str: String = "";
    h match {
      case Tag(a, b, c) => if (exceptList.contains(a))
        "<" + a + list2String(b) + ">" + toHtml(c) + "</" + a + ">";
      else if (exceptionListInBalise.contains(a)) {
        index = index + 2;
        str = addSpace(index) + "<" + a + list2String(b) + ">" + toHtml(c);
        index = index - 2;
        str + "</" + a + ">\n";
      } else {
        index = index + 2;
        str = addSpace(index) + "<" + a + ">\n" + list2String(b) + toHtml(c) + addSpace(index);
        index = index - 2;
        str + "</" + a + ">\n";
      }
      case Text(a) => if (a.length > 0 && a.charAt(0) == '&') addSpace(index + 2) + a + "\n"
      else a
      case _ => ""
    }
  }

  /**
   * @param lh une list de document HTML
   *
   * @return une String respectant le format HTML
   *
   */
  def toHtml(lh: List[Html]): String = {
    lh match {
      case Nil      => ""
      case a :: Nil => process(a)
      case a :: b   => process(a) + toHtml(b);
    }
  }

  /**
   * @param une liste de couple de String (Attributs de la balise, valeur)
   *
   * @return la liste des attributs et leurs param
   * 	exemple : List(("content", "text/html"), ("charset", "iso-8859-1")) ---> content="text/html; charset=iso-8859-1"
   */
  def list2String(l: List[(String, String)]): String = {
    l match {
      case Nil      => ""
      case a :: Nil => list2StringOx2(l) + "\""
      case a :: b   => list2StringOx1(l) + "\""
    }
  }

  /**
   * @param une liste de couple de String (Attributs de la balise, valeur)
   *
   * @return le premier couple d'attributs/valeurs
   * Fonction aux. de list2String
   */
  def list2StringOx1(l: List[(String, String)]): String = {
    l match {
      case Nil      => ""
      case a :: Nil => " " + a._1 + "=" + a._2
      case a :: b   => " " + a._1 + "=\"" + a._2 + ";" + list2StringOx1(b)
    }
  }

  /**
   * @param une liste de couple de String (Attributs de la balise, valeur)
   *
   * @return les couples d'attributs/valeurs sauf le premier
   * Fonction aux. de list2String
   */
  def list2StringOx2(l: List[(String, String)]): String = {
    l match {
      case Nil      => ""
      case a :: Nil => " " + a._1 + "=\"" + a._2
      case _        => ""
    }
  }

  /**
   * @param l une liste de couple de String
   * return Un boolean indiquant si il faut retourner à la ligne ou non
   */
  def checkAlaligne(l: List[(String, String)]): Boolean = {
    l match {
      case Nil      => true
      case a :: Nil => a.getClass().equals(Text)
      case a :: b   => checkAlaligne(b)
    }
    return false;
  }

  /**
   * @param i un entier
   * return le nombre d'espace i, pour respecter les indentations du document HTML
   */
  def addSpace(i: Int): String = {
    var ind: Int = 0;
    var str: String = "";
    while (ind < i) {
      str = str + " ";
      ind += 1;
    }
    return str;
  }

}

