package Interface

import scala.swing._
import java.awt.RenderingHints

class Bulle(txt: String, c: Color) extends BorderPanel {
  add(Swing.VStrut(5), BorderPanel.Position.North)
  add(Swing.VStrut(5), BorderPanel.Position.South)
  add(Swing.HStrut(15), BorderPanel.Position.East)
  add(Swing.HStrut(15), BorderPanel.Position.West)
  background = Couleurs.fond
  add(new Label(txt), BorderPanel.Position.Center)
  override def paintBorder(g: Graphics2D) {
    g.setColor(c)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.fillRoundRect(0, 0, size.getWidth.toInt, size.getHeight.toInt, size.getHeight.toInt, size.getHeight.toInt)
  }
}