package machine

object tolerance_au_faute extends Tolerance {

  def corrige(s: String, couple: List[(String, String)]): String = {
    couple match {
      case Nil    => ""
      case a :: b => if (hamming(s.toLowerCase, a._1.toLowerCase) >= 1) a._2 else corrige(s, b)
    }
  }

  val donnee = DataBase.base

  def correction(s: String): String = {
    val cles = donnee
    var res = s
    if (cles.contains(s)) res
    else
      for (e <- cles) {
        if (hamming(s.toLowerCase, e._1.toLowerCase) >= 1) {
          res = e._1
        }
      }
    res
  }

  def ajoute_Char_alafin(s: String, i: Int): String = {

    var n = ""

    for (k <- 1 to i) {
      n = n + '*'
    }
    s + n
  }

  def taille_egale(s: String, m: String): String = {
    var n = ""

    if (s.length < m.length) {
      n = ajoute_Char_alafin(s, (m.length - s.length)).toLowerCase
    } else if (s.length > m.length) {
      n = ajoute_Char_alafin(m, (s.length - m.length))
    }
    n
  }

  def hamming(s: String, m: String): Int = {
    var distance = 0

    // Cas ou les deux chaines sont de meme taille
    // Il suffit de compter le nb de lettre diff
    if (s.length == m.length) {

      var k = 0
      for (k <- 0 to m.length - 1) {
        if (m.charAt(k) != s.charAt(k)) {
          distance += 1
        }
      }

      // cas ou une chaine est plus grande
      // Il faut lui ajouter des car pour pouvoir la comparer

    } else if (s.length < m.length) {
      var n = ""

      n = taille_egale(s, m)
      for (k <- 0 to n.length - 1) {
        if (n.charAt(k) != m.charAt(k)) {
          distance += 1
        }
      }

      // cas ou L'AUTRE CHAINE est plus grande
      // cas a prendre en compte ?
    } else if (s.length > m.length) {
      var n = ""

      n = taille_egale(s, m)
      for (k <- 0 to n.length - 1) {
        if (n.charAt(k) != s.charAt(k)) {
          distance += 1
        }
      }
    }
    distance
  }

}

 /*def hamming(s1: String, s2: String): Int = {
    if (s1.length != s2.length)
      throw new IllegalArgumentException()
    (s1.toList).zip(s2.toList)
      .filter(current => current._1 != current._2)
      .length
  } */



