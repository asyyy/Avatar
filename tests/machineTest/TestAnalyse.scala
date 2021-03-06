
package machineTest

import org.junit.Test
import org.junit.Assert._
import machine.AnalyseFinal
import Langues.DetectionLangue

object TestAnalyse {
    val bdd : List[(String,String)] = List(
      ("Hôtel de ville","Place de la Mairie"),("Mairie de Rennes","Place de la Mairie"),("Mairie","Place de la Mairie"),
      ("Gare","19, Place de la Gare"),("Gare SNCF","19, Place de la Gare"),
      ("Théâtre National de Bretagne","1, Rue Saint-Hélier"),("TNB","1, Rue Saint-Hélier"),("Théâtre de Bretagne","1, Rue Saint-Hélier"),
      ("Théâtre La Paillette","2, Rue du Pré de Bris"),("la Paillette","2, Rue du Pré de Bris"))
    val lInver : List[(String,String)] = List(
      ("Place de la Mairie","Mairie de Rennes"),
      ("19, Place de la Gare","Gare SNCF"),
      ("1, Rue Saint-Hélier","Théâtre National de Bretagne"),
      ("2, Rue du Pré de Bris","Théâtre La Paillette")
      )
}
  class main {
    
  @Test 
  def testAContainsNotInDB{    
    assertEquals(List("Je ne comprends pas votre demande"),AnalyseFinal.AnalysePhraseFinal("Michelle c'est le Brésil", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains1{    
    assertEquals(List("L'adresse de mairie de rennes est : Place de la Mairie"),AnalyseFinal.AnalysePhraseFinal("Où est donc la Mairie de Rennes?", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains1_2{    
    assertEquals(List("L'adresse de mairie de rennes est : Place de la Mairie"),AnalyseFinal.AnalysePhraseFinal("Où est donc la Mairie de Rennes?", DetectionLangue.salutFR))
  }
   
  @Test 
  def testAContains2{
    assertEquals(List("L'adresse de théâtre national de bretagne est : 1, Rue Saint-Hélier"),AnalyseFinal.AnalysePhraseFinal("Où se trouve le Théâtre National de Bretagne? ", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains3{    
    assertEquals(List("L'adresse de mairie de rennes est : Place de la Mairie"),AnalyseFinal.AnalysePhraseFinal("Où est la Mairie?", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains4{    
    assertEquals(List("L'adresse de théâtre la paillette est : 2, Rue du Pré de Bris"),AnalyseFinal.AnalysePhraseFinal("Je recherche le Théâtre de la Paillette", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains5{    
    assertEquals(List("L'adresse de théâtre national de bretagne est : 1, Rue Saint-Hélier"),AnalyseFinal.AnalysePhraseFinal("Où est le TNB?", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains6{    
    assertEquals(List("Je ne comprends pas votre demande"),AnalyseFinal.AnalysePhraseFinal("ou trouver?", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains7{    
    assertEquals(List("Je ne comprends pas votre demande"),AnalyseFinal.AnalysePhraseFinal("Je cherche", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains8{    
    assertEquals(List("Je ne comprends pas votre demande"),AnalyseFinal.AnalysePhraseFinal("askdhlkajh", DetectionLangue.salutFR))
  }
  @Test 
  def testAContains9{    
    assertEquals(List("L'adresse de gare sncf est : 19, Place de la Gare"),AnalyseFinal.AnalysePhraseFinal("Où est la Gare?", DetectionLangue.salutFR))
  }
  
  @Test 
  def testAContainsBonjour{
    assertEquals(List("Bonjour","L'adresse de théâtre national de bretagne est : 1, Rue Saint-Hélier"),AnalyseFinal.AnalysePhraseFinal("bonjour tnb", DetectionLangue.salutFR))
  }
  @Test 
  def testAContainsBonjourAlone{
    assertEquals(List("Bonjour"),AnalyseFinal.AnalysePhraseFinal("bonjour", DetectionLangue.salutFR))
  }
  @Test 
  def testAContainsNotBonjour{
    assertEquals(List("L'adresse de théâtre national de bretagne est : 1, Rue Saint-Hélier"),AnalyseFinal.AnalysePhraseFinal("tnb", DetectionLangue.salutFR))
  }
  
  @Test 
  def testAContainsEmpty{    
    assertEquals(List("Je ne comprends pas votre demande"),AnalyseFinal.AnalysePhraseFinal("", DetectionLangue.salutFR))
  }
  @Test 
  def testAContainsOnlySpace{    
    assertEquals(List("Je ne comprends pas votre demande"),AnalyseFinal.AnalysePhraseFinal("  ", DetectionLangue.salutFR))
  }
 
  
 
}