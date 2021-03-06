package Interface

import scala.swing._
import event._
import machine.Tolerance

object UI extends MainFrame {
  title = "Fenêktre"
  preferredSize = new Dimension(640, 720)

  val entree: BorderPanel = new BorderPanel {
    background = Couleurs.fond
    add(InField, BorderPanel.Position.Center)
    add(BoutonEnvoi, BorderPanel.Position.East)
  }

  contents = new BorderPanel {
    background = Couleurs.fond
    add(Dialogue, BorderPanel.Position.North)
    add(entree, BorderPanel.Position.South)
  }

  pack.centerOnScreen

  listenTo(this)
  reactions += {
    case UIElementResized(_) => {
      val h: Int = (size.getHeight / 14).toInt
      entree.preferredSize = new Dimension(640, h)
      Dialogue.preferredSize = new Dimension(640, size.getHeight.toInt - h - 40)
    }
  }
}
