package Interface

import scala.swing._
import scala.swing.event.FocusEvent
import java.awt.Scrollbar
import util.control.Breaks._
import java.awt.Graphics2D
import machine._
import Tolerance.ToleranceFaute

object Dialogue extends ScrollPane {
  
  def retourChariot(s: String): String = {
    var res: String = "<html>"
    var i: Int = 0
    var espace: Boolean = false
    for (c <- s) {
      if (c == '\n') res += "<br>"
      else {
        res += c
        i += 1
      }

      if (i % 40 == 0) {
        espace = false
        breakable {
          if (i < s.length && s.charAt(i) == ' ') break
          else if (i == s.length) break
          for (j <- i - 40 to i - 1) {
            if (res.charAt(j) == ' ') {
              espace = true
              break
            }
          }
        }
        if (espace) res = res.reverse.replaceFirst(" ", ">rb<").reverse
        else res += "<br>"
      }
    }
    res
  }

  background = Couleurs.fond
  border = Swing.LineBorder(Couleurs.fond)

  val boite = new BoxPanel(Orientation.Vertical) {
    background = Couleurs.fond
  }

  def ajoutQuestion(txt: String) {
    boite.contents += new BorderPanel {
      background = Couleurs.fond
      add(Swing.HStrut(10), BorderPanel.Position.West)
      add(new BorderPanel {
        background = Couleurs.fond
        add(new Bulle(retourChariot(txt), Couleurs.vert), BorderPanel.Position.West)
      }, BorderPanel.Position.Center)
    }

    boite.contents += Swing.VStrut(10)

    boite.revalidate
    Swing.onEDT(verticalScrollBar.peer.setValue(verticalScrollBar.peer.getMaximum))
  }
  
  var repList:List[String] = Nil 

  def ajoutTextes(txt: String) {
    boite.contents += Swing.VStrut(10)

    boite.contents += new BorderPanel {
      background = Couleurs.fond
      add(Swing.HStrut(10), BorderPanel.Position.East)
      add(new BorderPanel {
        background = Couleurs.fond
        add(new Bulle(retourChariot(txt), Couleurs.bleu), BorderPanel.Position.East)
      }, BorderPanel.Position.Center)
    }

    boite.contents += Swing.VStrut(10)
    ///////////
    
   
    def isAllDigits(x: String) = x forall Character.isDigit
    val sl : List[String] = AnalyseFinal.separator(Tolerance.ToleranceFaute.tolerance(txt))
    var num : String = ""
     
     for(e <- sl){
      if(isAllDigits(e)){
        if(!machine.MachineImpl.optionMultiple.isEmpty){
          machine.MachineImpl.num = e
        }
      }
     }
   
    repList = machine.MachineImpl.ask(txt)
    
    ////
    for (s <- repList) {
      boite.contents += new BorderPanel {
        background = Couleurs.fond
        add(Swing.HStrut(10), BorderPanel.Position.West)
        add(new BorderPanel {
          background = Couleurs.fond
          add(new Bulle(retourChariot(s), Couleurs.vert), BorderPanel.Position.West)
        }, BorderPanel.Position.Center)
      }

      boite.contents += Swing.VStrut(10)
    }
    boite.revalidate
    Swing.onEDT(verticalScrollBar.peer.setValue(verticalScrollBar.peer.getMaximum))
  }

  contents = new BorderPanel {
    background = Couleurs.fond
    add(boite, BorderPanel.Position.North)
  }
}
