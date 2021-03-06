package machineTest
import org.junit.Test
import org.junit.Assert._
import machine.tolerance_au_faute
class TestTolérance {
  
  @Test
  def test1 {
    assertEquals(tolerance_au_faute.correction("mairier"), "mairie")
    assertEquals(tolerance_au_faute.correction("mire"), "mire")
    assertEquals(tolerance_au_faute.correction("tete"), "tete")
    assertEquals(tolerance_au_faute.correction("tu"), "tu")
  }

  @Test
  def test2 {
    assertEquals(tolerance_au_faute.correction("hotelo"), "hotel")
    assertEquals(tolerance_au_faute.correction("hoteol"), "hotel")
    assertEquals(tolerance_au_faute.correction("hotoel"), "hotel")
    assertEquals(tolerance_au_faute.correction("otel"), "hotel")
    assertEquals(tolerance_au_faute.correction("hotej"), "hotel")
    assertEquals(tolerance_au_faute.correction("hoteloo"), "hoteloo")
  }

  @Test
  def test3 {
    assertEquals(tolerance_au_faute.correction("bonjour"), "bonjour")
    assertEquals(tolerance_au_faute.correction("Bonour"), "bonjour")
    assertEquals(tolerance_au_faute.correction("Bonrour"), "bonjour")
    assertEquals(tolerance_au_faute.correction("pBonjour"), "bonjour")

  }
  @Test
  def test4 {
    assertEquals(tolerance_au_faute.correction("bjour"), "bjour")
    assertEquals(tolerance_au_faute.correction("Boour"), "Boour")
    assertEquals(tolerance_au_faute.correction("Bijoure"), "Bijoure")
    assertEquals(tolerance_au_faute.correction("ghnjour"), "ghnjour")
  }
  


}