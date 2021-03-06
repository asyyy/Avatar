package machine

import Tolerance.ToleranceFaute
import Langues.DetectionLangue._
import util.control.Breaks._

object MachineImpl extends MachineDialogue {
  var optionMultiple: List[String] = List()
  var num: String = ""

  def ask(s: String): List[String] = {
    val tol = ToleranceFaute.tolerance(s)
    println("Correction : " + tol)
    val ack = detecterLangue(tol)
    val sl: List[String] = AnalyseFinal.separator(tol)
    //println("--------------")
    //println("lneed " +lNeed)
    if (num.compareTo("") == 0) {
      //println("Con1")
      if (ack.length ==1 || isFirstBjr(ack)) {
        //println("Con2")
        //Affichage d'un seul élément dans la liste
        ack
      } else {
        //println("Con3")
        //Affichage des options multiples
        optionMultiple = detecterLangue(tol)
        optionMultiple
      }
    } else {
      //println("Con4")
      //println("num =" +num)
      if (num.toInt > ack.length + 1) {
        List("Je ne comprends pas")
      } else {
        val tempNum = num
        val tempOption = optionMultiple
        num = ""
        optionMultiple = List()
        detecterLangue(tempOption(tempNum.toInt))
        //AnalyseFinal.AnalysePhraseFinal(tempOption(tempNum.toInt),avFR,salutFR)
      }
    }
  }
  /**
   * @param l liste de string
   * @return true si le 1ere éléments de l est une salutation
   */
  def isFirstBjr(l : List[String]) : Boolean = {
    var res : Boolean = false
    
    if(l.length <= 2){
      if(salutDE.contains(l(0)) ||
         salutFR.contains(l(0)) ||
         salutEN.contains(l(0)) ||
         salutIT.contains(l(0)) ||
         salutES.contains(l(0))){
        res = true
      }
    }
    res
  }
  

  // Pour la partie test par le client

  def reinit = {
    etape = 0
    langueActuelle = langueDefaut
    AnalyseFinal.langDB = List("Bonjour", "L'adresse de", "est", "J'ai", "réponses possibles", "Je ne comprends pas votre demande", "Quel est votre choix?")
  }

  def test(l: List[String]): List[String] = {
    var res: List[String] = Nil
    println()

    for (s <- l) {
      println("-------- " + s)
      res = res ::: ask(s)
    }

    println("-------- " + res)
    res
  }
}