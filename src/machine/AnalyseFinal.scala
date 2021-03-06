package machine

object AnalyseFinal {
  val ponctuations: Array[Char] = Array('\n', '!', '"', '#', '$', '%', '&', '\'', '*', '+', ',', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|', '}', '~', ' ')

  val determinant: List[String] = List("le", "la", "l'", "les", "des", "du", "un", "une", "de")

  val database = machine.DataBase.base

  var newDB: List[(List[String], String)] = List()
  var nomSepNom: List[(List[String], String)] = List()

  //Liste des mots à dire, DetectionLangue la change en même temps que la langue
  var langDB: List[String] = List("Bonjour", "L'adresse de", "est", "J'ai", "réponses possibles", "Je ne comprends pas votre demande", "Quel est votre choix?")

  /**
   * @param s la phrase de l'utilisateur
   * @param salut la liste des salutations
   * @return liste comprenant la réponse à la question ou liste comprenant d'autres propositions
   */
  def AnalysePhraseFinal(s: String, salut: List[String]): List[String] = {
    val need: String = s.toLowerCase()
    var res: List[String] = List()
    val sentence: List[String] = suppress(separator(need), determinant)
    var bonjour: Boolean = false
    var similitude: Int = 0
    var first: List[List[String]] = List()
    var lSim: List[Int] = List() // Liste parallèle a FIRST

    if (onlyHi(sentence, salut)) {
      res = List(langDB.apply(0))
    } else {

      //if (sentence.contains("bonjour") || sentence.contains("bonsoir") || sentence.contains("salut")) bonjour = true
      for (s <- salut) {
        if (sentence.contains(s)) bonjour = true
      }
      if (bonjour) println("Contient bonjour")
      //modif pour la F7
      if (separator(need).contains("pizzéria") || separator(need).contains("restaurant") || separator(need).contains("crêperie")) {
        var found = false
        var finUrl = ""

        for (i <- separator(need)) {
          println(i)
          if (i.equals("pizzéria") || i.equals("restaurant") || i.equals("crêperie")) found = true
          else if (found)
            if (i != separator(need).last) finUrl = finUrl + i + "+"
            else finUrl = finUrl + i
        }

        val urlx = "https://www.linternaute.com/restaurant/guide/ville-rennes-35000/?name=" + finUrl
        //println(urlx)
        List()
      } else {
        //Créé une base de donnée où la 1er partie de la BDD est nettoyée
        //Créé une base de donnée où la 1er partie est le nom nettoyée et la 2è est le nom de base
        for (e <- database) {
          var temp: List[String] = suppress(separator(e._1), determinant)
          newDB = newDB ++ List((temp, e._2))
          nomSepNom = nomSepNom ++ List((temp, e._1))
        }

        for (wordsentence <- sentence) { //On parcourt la phrase
          for (e <- newDB) { //On parcourt la bdd où les phrases sont listés
            for (worde <- e._1) { //On parcourt la liste d'un élément listé
              if (wordsentence.compareTo(worde) == 0) {
                first = first ++ List(e._1)
              }
            }
          }
        }

        //println("First = " + first.distinct)
        first = first.distinct

        //On parcourt la liste des éléments ayant au moins 1 éléments communs à la sentence
        for (f <- first) {
          similitude = 0
          for (fson <- f.distinct) { //On regarde pour chaques éléments s'il correspond à un mot de sentence
            //println(fson)
            for (w <- sentence) {
              if (fson.compareTo(w) == 0) { //Si on trouve un mot, on a une ressemblance
                similitude = similitude + 1
              } else {
                similitude = similitude - 1
              }
            }
          }
          lSim = lSim ++ List(similitude)
        }

      }

      //println("lSim = " + lSim.distinct)
      if (!lSim.isEmpty) {
        var lCommunMax = maxElement(first, lSim)

        //println("lCommunMax = " + lCommunMax)

        //res = recupList(lCommunMax)
        lCommunMax = checkForF1(lCommunMax).distinct

        if (lCommunMax.length == 1) { // 1 Element
          println("Un seul élément trouvé")

          var nom: List[String] = recupList(lCommunMax).distinct
          res = miseEnFormeUnSeul(nom, noFirstSpace(recupAdresse(lCommunMax(0)).distinct), bonjour)

        } else if (lCommunMax.length > 1) { // Plusieurs Elements
          println("Plusieurs éléments trouvés")
          res = miseEnFormePlusieurs(recupList(lCommunMax))

        }
      } else {

        println("Aucun élément trouvé")
        if (!bonjour) res = List(langDB.apply(5))
        else res = List(langDB.apply(0), langDB.apply(5))
      }
    }
    res
  }

  /**
   * @param b boolean
   * @param l la phrase de l'utilisateur
   * @return true si l contient bonjour
   */
  def bonjourOrNotBonjour(b: Boolean, l: List[String]): Boolean = {
    var res: Boolean = false
    if (b) {
      if (l.length == 2) res = true
    }
    if (!b) {
      if (l.length == 1) res = true
    }
    res
  }

  /**
   * @param l liste de string
   * @return l avec ses éléments sans espace comme premier caractères
   */
  def noFirstSpace(l: List[String]): List[String] = {
    var res: List[String] = List()
    for (e <- l) {
      if (e.charAt(0) == ' ') {
        res = res ++ List(e.tail)
      } else res = res ++ List(e)
    }
    res
  }

  /**
   * @param nom liste des noms d'endroits
   * @param adresse liste des adresses des endroits
   * @param cond boolean
   * @return Liste de String de la bonne syntaxe pour répondre à l'utilisateur
   */
  def miseEnFormeUnSeul(nom: List[String], adresse: List[String], cond: Boolean): List[String] = {
    var res: List[String] = List()
    if (cond) {
      res = List(langDB.apply(0), langDB.apply(1) + " " + nom(0).toLowerCase() + " " + langDB.apply(2) + " : " + adresse(0))
    } else {
      res = List(langDB.apply(1) + " " + nom(0).toLowerCase() + " " + langDB.apply(2) + " : " + adresse(0))
    }
    res
  }

  /**
   * @param l liste de string des propositions multiples pour l'utilisateur
   * @return liste de string contenant la bonne syntaxe pour répondre à l'utilisateur
   */
  def miseEnFormePlusieurs(l: List[String]): List[String] = {
    var res: List[String] = List(langDB.apply(3) + " " + l.length + " " + langDB.apply(4) + " :")
    for (e <- l.zipWithIndex) {
      res = res ++ List(e._2 + 1 + ") " + e._1)
    }
    res = res ++ List(langDB.apply(6))
    res
  }

  /**
   * @param l liste String de noms d'endroits
   * @return liste string des adresses associés aux endroits
   */
  def recupAdresse(l: List[String]): List[String] = {
    var res: List[String] = List()

    for (e <- newDB) {

      if (e._1 == l) {

        res = res ++ List(e._2)
      }
    }
    res
  }

  /**
   * @param l liste de liste de string
   * @return liste de string contenant les noms d'endroits
   */
  def recupList(l: List[List[String]]): List[String] = {
    var res: List[String] = List()
    for (e <- l) {
      for (i <- nomSepNom) {
        if (e == i._1) {
          res = res ++ List(i._2)
        }
      }
    }
    res = res.distinct
    //println("recupList = " + res)
    res
  }
  /**
   * @param l liste de liste de string
   * @param s liste de int
   * @return le ou les éléments max de l par rapport à la liste s
   */
  def maxElement(l: List[List[String]], s: List[Int]): List[List[String]] = {
    var res: List[List[String]] = List()
    var max: Int = s(0)

    for (i <- s) {
      if (i > max) max = i
    }
    //println("max = " + max)
    for (i <- s.zipWithIndex) {
      if (i._1 == max) {
        //println(l(i._2))
        res = res ++ List(l(i._2))
      }
    }
    res
  }

  /**
   * @param s liste String
   * @param salut liste de string
   * @return true si s contient un élément de salut
   */
  def onlyHi(s: List[String], salut: List[String]): Boolean = {
    var rep = false
    //if (s.length == 1 && (s(0).compareTo("bonjour") == 0 || s(0).compareTo("salut") == 0 || s(0).compareTo("bonsoir") == 0)) true
    for (s1 <- salut) {
      if (s.length == 1 && s.contains(s1.toLowerCase())) rep = true
    }
    rep

  }
  /**
   * @param s string
   * @return liste des éléments de s séparé par la ponctuation
   */
  def separator(s: String): List[String] = {

    val temp: List[String] = s.split(ponctuations).toList
    var res: List[String] = List()

    for (e <- temp) {
      if (e.compareTo("") != 0) {
        res = res ++ List(e)
      }
    }
    res
  }
  /**
   * @param l liste de string
   * @param m liste de string
   * @param l sans les éléments de m
   */
  def suppress(l: List[String], m: List[String]): List[String] = {
    var res: List[String] = List()
    for (i <- l) {
      if (!m.contains(i.toLowerCase())) {
        res = res ++ List(i.toLowerCase())
      }
    }
    //println(res)
    res
  }

  /**
   * @param l liste de liste de string
   * @return la bonne recherche car la base de données finale est différente de l'initiale
   */
  def checkForF1(l: List[List[String]]): List[List[String]] = {
    var res: List[List[String]] = List()
    for (e <- l) {
      if (e == List("mairie") || e == List("hôtel", "ville")) {
        res = res ++ List(List("mairie", "rennes"))
      } else if (e == List("théâtre", "bretagne") || e == List("tnb")) {
        res = res ++ List(List("théâtre", "national", "bretagne"))
      } else if (e == List("paillette")) {
        res = res ++ List(List("théâtre", "paillette"))
      } else if (e == List("gare")) {
        res = res ++ List(List("gare", "sncf"))
      } else {
        res = res ++ List(e)
      }
    }
    res
  }
}