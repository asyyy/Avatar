package Interface

import scala.swing._
import event._
import scala.collection.mutable.Buffer

object InField extends EditorPane {

  background = Couleurs.gris
  foreground = Couleurs.fond
  caret.color = Couleurs.fond
  text = ""
  border = Swing.LineBorder(Couleurs.gris)
  
/*
  listenTo(keys)
  var pressedKeys = Buffer[Key.Value]()
  reactions += {
    case KeyPressed(_,key,_,_) => {
      pressedKeys += key
      if (pressedKeys.contains(Key.Alt) && pressedKeys.contains(Key.Enter)) text += '\n'
      else if(pressedKeys contains Key.Enter)
      {
        if (text != "") {
          Dialogue.ajoutTextes(text)
          text = "\n" //Pour rafraichir l'interface de dialogue, n'essayez pas de comprendre ^_^
          text = ""
        }
      }
    }

    case KeyReleased(_,Key.Enter,_,_) => {
      if(!pressedKeys.contains(Key.Alt)) text = ""
    }
    case KeyReleased(_,key, _, _) => pressedKeys = Buffer[Key.Value]()
  }
*/
}
