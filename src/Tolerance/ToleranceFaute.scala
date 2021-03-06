package Tolerance

import util.control.Breaks._
import Langues.DetectionLangue
import scala.collection.immutable.SortedSet
import machine.DataBase._

object ToleranceFaute {

  /**
   * 	Calcule la distance de Hamming entre deux mots de même longueurs, sans accents ni majuscule
   * 	@param s1 Un mot
   * 	@param s2 Un mot de même longueur que s1
   * 	@return La distance de Hamming entre s1 et s2
   */
  def distanceHamming(s1: String, s2: String): Int = {
    require(s1.length == s2.length)
    var distance = 0
    var s = s2
    if (!s1.equals(s2)) {
      for (c <- s1) {
        if (!s.head.equals(c)) {
          distance += 1
        }
        s = s.tail
      }
    }
    distance
  }

  /**
   * 	Vérifie si s1 est corrigible en s2, si s1 < s2 de 1 rajoute '*' à chaque emplacement possible de s1 pour faire Hamming
   * 	@param s1 Un mot rentré par l'utilisateur (en minuscule et sans accents)
   * 	@param s2 Un mot de la base de donnée (en minuscule et sans accents)
   * 	@return True si s1 est corrigible en s2
   */
  def compare(s1: String, s2: String): Boolean = {
    var distance: Int = 2
    val m1 = enleverCharInutiles(remplacerAccents(s1)).toLowerCase
    val m2 = enleverCharInutiles(remplacerAccents(s2)).toLowerCase
    if (m1.length <= 2) distance = 2
    else if (m1.length == m2.length) distance = distanceHamming(m1, m2)
    else if (m2.length - m1.length == 1) {
      distance = distanceHamming(m1 + "*", m2)
      var s = ""
      var j = 0
      breakable {
        for (i <- 0 to m1.size) {
          for (c <- m1) {
            if (i == j) s += "*"
            s += c
            j += 1
          }
          j = 0
          if (i == s1.length) s += "*"
          distance = distanceHamming(s, m2)
          if (distance <= 1) break
          s = ""
        }
      }
    }
    distance <= 1
  }

  def comparerPhrases(p1: List[String], p2: List[String]): Boolean = {
    require(p1.length == p2.length)
    (p1, p2) match {
      case (Nil, Nil)         => true
      case (h :: t, h2 :: t2) => if (h2.length <= 2 && h.length <= 2) true && comparerPhrases(t, t2) else compare(h, h2) && comparerPhrases(t, t2)
    }
  }

  def corrigeBoutPhrase(l: List[String]): List[String] = {
    var mot: List[String] = Nil
    l match {
      case Nil => Nil
      case h :: Nil =>
        if (bdd.mkString(" ").toLowerCase.contains(h.toLowerCase)) {
          if (h.toLowerCase.equals("gar")) List("Gare")
          else List(h)
        } else {
          for (s1 <- bdd) {
            if (compare(h, s1)) {
              mot = List(s1)
            }
          }
          if (mot == Nil) List(h) else mot
        }
      case h :: t =>
        if (bdd2.mkString(" ").toLowerCase.contains(l.mkString(" ").toLowerCase)) {
          l
        } else {
          for (s1 <- bdd2) {
            val a2 = s1.split(" ").toList
            if (l.length == a2.length) {
              if (comparerPhrases(l, a2)) {
                mot = a2
              }
            }
          }
          if (mot == Nil) corrigeBoutPhrase(t) else mot
        }
    }
  }

  /**
   * Corrige une phrase
   * @param in La phrase rentrée par l'utilisateur
   * @return La phrase corrigée
   */
  def tolerance(in: String): String = {
    val l = enleverCharInutiles(in).split(" ").toList
    val phrase = corrigeBoutPhrase(l)
    if (phrase.length == l.length) phrase.mkString(" ")
    else tolerance(l.dropRight(phrase.length).mkString(" ")) + " " + phrase.mkString(" ")
  }

  /** La base de donnée des mots à corriger */
  private val bdd: SortedSet[String] = stringToSetMot(List("si", "nein", "yes", "no", "oui", "non") ::: DetectionLangue.de ::: DetectionLangue.fr ::: DetectionLangue.es ::: DetectionLangue.en ::: DetectionLangue.it ::: base.map({ case (s1, s2) => s1 }) ::: List("l'hôtel"))

  /** La base de donnée des phrases à corriger */
  private val bdd2: SortedSet[String] = stringToSetMot2(base.map({ case (s1, s2) => s1 }) ::: List("je choisis"))

  
  /**
   * Modifie une liste de phrases en Set de mots unique et trie
   * @param l Une liste de phrases
   * @return Un Set de mots sans doublons et trie
   */  
  private def stringToSetMot(l: List[String]): SortedSet[String] = {
    l match {
      case Nil    => SortedSet()
      case h :: t => SortedSet() ++ h.split(" ").toSet ++ stringToSetMot(t)
    }
  }
  
  /**
   * Modifie une liste de phrases en set de phrases unique et trie
   * @param l Une liste de phrases
   * @return Un Set de phrases sans doublons et trie
   */
  private def stringToSetMot2(l: List[String]): SortedSet[String] = {
    l match {
      case Nil    => SortedSet()
      case h :: t => SortedSet(h) ++ stringToSetMot2(t)
    }
  }

  /**
   * Remplace les accents...
   * @param s Le mot à modifier
   * @return Le mot sans accents
   */
  private def enleverCharInutiles(s: String): String = {
    val charInutiles: Set[Char] = Set(',', ';', '!', '(', ')', '-', '_', '?', '&', '/', ':', '.', ''')
    var res: String = ""
    for (c <- s) {
      if (charInutiles.contains(c)) res += ""
      else res += c
    }
    res
  }

  /**
   * Remplace les accents...
   * @param s Le mot à modifier
   * @return Le mot sans accents
   */
  private def remplacerAccents(s: String): String = {
    val accents: Map[String, String] = Map(
      ("é", "e"),
      ("è", "e"),
      ("ê", "e"),
      ("ë", "e"),
      ("à", "a"),
      ("â", "a"),
      ("ä", "a"),
      ("ù", "u"),
      ("û", "u"),
      ("ü", "u"),
      ("î", "i"),
      ("ï", "i"),
      ("ô", "o"),
      ("ö", "o"),
      ("ç", "c"))
    var res: String = ""
    for (c <- s) {
      if (accents.contains(c.toString)) res += accents.get(c.toString).get
      else res += c
    }
    res
  }
}