package Interface

import scala.swing._
import event._
import marytts.LocalMaryInterface;
import marytts.MaryInterface;
import marytts.util.data.audio.AudioPlayer

object BoutonEnvoi extends Button {

  text = "Envoyer"
  background = Couleurs.fond
  foreground = Couleurs.gris

  var voix: String = "upmc-pierre-hsmm"

  def lire(txt: String) = {
    if (!voix.equals("")) {
      val marytts: MaryInterface = new LocalMaryInterface
      marytts.setVoice(voix)
      val ap = new AudioPlayer
      ap.setAudio(marytts.generateAudio(txt))
      ap.run
    }
  }

  reactions += {
    case ButtonClicked(_) => {
      if (InField.text != "") {
        Dialogue.ajoutTextes(InField.text)
        InField.text = "\n" //Pour rafraichir l'interface de dialogue, n'essayez pas de comprendre ^_^
        InField.text = ""

        for (s <- Dialogue.repList) {
          Swing.onEDT(lire(s))
        }
      }
    }
  }

}
