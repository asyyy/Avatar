package ToleranceTests

import org.junit.Test
import org.junit.Assert._
import Tolerance.ToleranceFaute._

class TestTolérance {
  
  @Test
  def testHamming1 {
    assertEquals(distanceHamming("malo", "malo"), 0)
  }
  
  @Test
  def testHamming2 {
    assertEquals(distanceHamming("mall", "malo"), 1)
  }
  
  @Test
  def testHamming3 {
    assertEquals(distanceHamming("beurel", "mairie"), 5)
  }
  
  @Test
  def testCompare1 {
    assertEquals(compare("malo", "malo"), true)
  }
  
  @Test
  def testCompare2 {
    assertEquals(compare("mall", "malo"), true)
  }
  
  @Test
  def testCompare3 {
    assertEquals(compare("beurel", "mairie"), false)
  }
  
  @Test
  def testCompare4 {
    assertEquals(compare("barbot", "malo"), false)
  }
  
  @Test
  def testCompare5 {
    assertEquals(compare("barbo", "barbot"), true)
  }
  
  @Test
  def testCompare6 {
    assertEquals(compare("babot", "barbot"), true)
  }
  
  @Test
  def test1 {
    assertEquals("mairier", tolerance("mairier"))
    assertEquals("mire", tolerance("mire"))
    assertEquals("tete", tolerance("tete"))
    assertEquals("tu", tolerance("tu"))
  }

  @Test
  def test2 {
    assertEquals("hotelo", tolerance("hotelo"))
    assertEquals("hoteol", tolerance("hoteol"))
    assertEquals("hotoel", tolerance("hotoel"))
    assertEquals("Hôtel", tolerance("otel"))
    assertEquals("Hôtel", tolerance("hotej"))
    assertEquals("hoteloo", tolerance("hoteloo"))
  }

  @Test
  def test3 {
    assertEquals("Bonjour", tolerance("bonjour"))
    assertEquals("Bonjour", tolerance("Bonour"))
    assertEquals("Bonjour", tolerance("Bonrour"))
    assertEquals("pBonjour", tolerance("pBonjour"))

  }
  @Test
  def test4 {
    assertEquals("bjour", tolerance("bjour"))
    assertEquals("Boour", tolerance("Boour"))
    assertEquals("Bijoure", tolerance("Bijoure"))
    assertEquals("ghnjour", tolerance("ghnjour"))
  }

}