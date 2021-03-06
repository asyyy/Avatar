package Langues

import util.control.Breaks._
import machine.AnalyseFinal._
import Interface.BoutonEnvoi
import machine.AnalyseFinal

object DetectionLangue {
  /** Liste des mots à chercher pour savoir si l'utilisateur parle français */
  val fr: List[String] = List("bonjour", "salut", "bonsoir", "recherche", "cherche", "où", "est", "donc", "trouve", "trouver", "français")
  /** Liste des mots à chercher pour savoir si l'utilisateur parle anglais */
  val en: List[String] = List("hi", "hello", "morning", "evening", "afternoon", "hey", "seek", "seeking", "search", "searching", "look", "looking", "where", "find", "english")
  /** Liste des mots à chercher pour savoir si l'utilisateur parle espagnol */
  val es: List[String] = List("hola", "buenos", "dias", "donde", "esta", "busco", "buscando", "español")
  /** Liste des mots à chercher pour savoir si l'utilisateur parle allemand */
  val de: List[String] = List("hallo", "guten", "morgen", "tag", "abend", "wo", "ist", "suche", "suchen", "deutsch")
  /** Liste des mots à chercher pour savoir si l'utilisateur parle italien */
  val it: List[String] = List("buongiorno", "ciao", "salve", "buon", "pomeriggio", "buonasera", "incantato", "dove", "trova", "cerco", "cercando", "italiano")

  //Listes des salutations dans les différrentes langues
  val salutFR: List[String] = List("bonjour", "salut", "bonsoir")
  val salutEN: List[String] = List("hi", "hello", "morning", "evening", "afternoon", "hey")
  val salutES: List[String] = List("hola", "buenos", "dias")
  val salutDE: List[String] = List("hallo", "guten", "morgen", "tag", "abend")
  val salutIT: List[String] = List("buongiorno", "ciao", "salve", "buon")

  val langueDefaut: String = "fr"
  var langueActuelle: String = langueDefaut
  var langueUtilisateur: String = langueDefaut
  var etape: Int = 0;

  def detecterLangue(s: String): List[String] = {

    //Vérifie quelle langue parle l'utilisateur
    breakable {
      for (mot <- s.toLowerCase.split(" ")) {
        if (fr.contains(mot)) {
          langueUtilisateur = "fr"
          BoutonEnvoi.voix = "upmc-pierre-hsmm"
          break
        } else if (en.contains(mot)) {
          langueUtilisateur = "en"
          BoutonEnvoi.voix = "cmu-slt-hsmm"
          break
        } else if (es.contains(mot)) {
          langueUtilisateur = "es"
          BoutonEnvoi.voix = ""
          break
        } else if (de.contains(mot)) {
          langueUtilisateur = "de"
          BoutonEnvoi.voix = "dfki-pavoque-neutral-hsmm"
          break
        } else if (it.contains(mot)) {
          langueUtilisateur = "it"
          BoutonEnvoi.voix = "istc-lucia-hsmm"
          break
        }
      }
    }

    if (!langueUtilisateur.equals(langueActuelle)) {
      etape = 1
      langueActuelle = langueUtilisateur
    }

    etape match {
      //Parle à l'utilisateur dans sa langue
      case 0 =>
        if (langueActuelle.equals("fr")) AnalysePhraseFinal(s, salutFR)
        else if (langueActuelle.equals("en")) AnalysePhraseFinal(s, salutEN)
        else if (langueActuelle.equals("es")) AnalysePhraseFinal(s, salutES)
        else if (langueActuelle.equals("de")) AnalysePhraseFinal(s, salutDE)
        else if (langueActuelle.equals("it")) AnalysePhraseFinal(s, salutIT)
        else Nil

      //Demande si l'utilisateur parle la bonne langue
      case 1 =>
        if (langueUtilisateur.equals("fr")) {
          etape = 2
          BoutonEnvoi.voix = "upmc-pierre-hsmm"
          List("Parlez-vous français?")
        } else if (langueUtilisateur.equals("en")) {
          etape = 2
          BoutonEnvoi.voix = "cmu-slt-hsmm"
          List("Do you speak english?")
        } else if (langueUtilisateur.equals("es")) {
          etape = 2
          BoutonEnvoi.voix = ""
          List("Hablas español?")
        } else if (langueUtilisateur.equals("de")) {
          etape = 2
          BoutonEnvoi.voix = "dfki-pavoque-neutral-hsmm"
          List("Sprechen Sie Deutsch?")
        } else if (langueUtilisateur.equals("it")) {
          etape = 2
          BoutonEnvoi.voix = "istc-lucia-hsmm"
          List("Parli italiano?")
        } else {
          Nil
        }

      //Verifie si l'utilisateur parle la bonne langue
      case 2 => suiteLangue(s, langueActuelle)
    }
  }

  /**
   * Detecte si l'utilisateur parle la langue voulu sinon passe à la suivante
   * @param rep La réponse de l'utilisateur
   * @param lang La langue que l'avatar pense que l'utilisateur parle
   */
  private def suiteLangue(rep: String, lang: String): List[String] = {
    lang match {
      case "fr" => if (rep.toLowerCase.equals("oui")) {
        etape = 0
        AnalyseFinal.langDB = List("Bonjour", "L'adresse de", "est", "J'ai", "réponses possibles", "Je ne comprends pas votre demande", "Quel est votre choix?")
        List("D'accord, quelle est votre demande?")
      } else {
        BoutonEnvoi.voix = "cmu-slt-hsmm"
        langueActuelle = "en"
        langueUtilisateur = "en"
        List("Do you speak english?")
      }
      case "en" => if (rep.toLowerCase.equals("yes")) {
        etape = 0
        AnalyseFinal.langDB = List("Hello", "The address of", "is", "I found", "answers", "I do not understand", "What is your choice?")
        List("OK, what is your query?")
      } else {
        BoutonEnvoi.voix = ""
        langueActuelle = "es"
        langueUtilisateur = "es"
        List("Hablas español?")
      }
      case "es" => if (rep.toLowerCase.equals("si")) {
        etape = 0
        AnalyseFinal.langDB = List("Hola", "La dirección de", "es", "Tengo", "opciones", "No comprendo", "Cuál es su elección?")
        List("Está bien, cuál es tu petición?")
      } else {
        BoutonEnvoi.voix = "dfki-pavoque-neutral-hsmm"
        langueActuelle = "de"
        langueUtilisateur = "de"
        List("Sprechen Sie Deutsch?")
      }
      case "de" => if (rep.toLowerCase.equals("ja")) {
        etape = 0
        AnalyseFinal.langDB = List("Hallo", "Die adresse von", "ist", "Ich habe", "Antworten", "Ich verstehe nicht", "Was ist Ihre Wahl?")
        List("Okay, was ist Ihr Wunsch?")
      } else {
        BoutonEnvoi.voix = "istc-lucia-hsmm"
        langueActuelle = "it"
        langueUtilisateur = "it"
        List("Parli italiano?")
      }
      case "it" => if (rep.toLowerCase.equals("si")) {
        etape = 0
        AnalyseFinal.langDB = List("Buongiorno", "Indirizzo di", "è", "Ho", "risposte", "No capisco", "Qual è la vostra scelta?")
        List("Va bene, qual è la tua richiesta?")
      } else {
        BoutonEnvoi.voix = "upmc-pierre-hsmm"
        langueActuelle = "fr"
        langueUtilisateur = "fr"
        List("Parlez-vous français?")
      }
    }
  }
}