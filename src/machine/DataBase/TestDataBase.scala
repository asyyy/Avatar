import scala.swing._
import scala.io.Source
import machine.DataBase

object TestDataBase extends App{

  var DB = DataBase.base
  //val DBxml = DataBase.baseXML
  val DBtemp = DataBase.temp
  //println(base.newBase)
  var baseEmo = Source.fromFile("./doc/Emojis.txt").getLines.toList
  var test = DataBase.baseTxt
  var test2 = DataBase.baseTxtInv
  //var base3 : List[(String,String)]= base.con
  println(test)
  println(machineTest.TestAnalyse.bdd)
  println()
  println(test2)
  println(machineTest.TestAnalyse.lInver)
  /*
  for (a <- test){
   // if(a._2.slice(a._2.length-4, a._2.length) == ".png"){    
      //println(Dialog.showMessage(message = a._2))
      println(a._1 + " =====> " + a._2)
      
    }
  //}
  */
  /*
  for ( a <- DBEmo){
    println(a._2)
  }*/
  
 //print(DBtemp)

}