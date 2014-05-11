import scala.swing._
import scala.swing.event._
import java.awt.event._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
import java.awt.{Color => AWTColor}

object Game extends SimpleSwingApplication {

  val bluishGray = new AWTColor(48, 99, 99)
  val bluishSilver = new AWTColor(210, 255, 255)

  def generatePanel(i: Int): BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      background = bluishSilver
      minimumSize = new Dimension(100, 100)
      maximumSize = minimumSize
      var label = new Label(i.toString, null, Alignment.Center) {
        xAlignment = Alignment.Center
        horizontalTextPosition = Alignment.Center
        verticalTextPosition = Alignment.Center
      }
      contents += label
      opaque = true
    }
  }

  def top = new MainFrame {
    title = "2048"
    centerOnScreen
    preferredSize = new Dimension(300, 300)
    maximumSize = new Dimension(300, 300)
    minimumSize = new Dimension(300, 300)

    val panel1 = generatePanel(1)
    val panel2 = generatePanel(2)
    val panel3 = generatePanel(3)
    val panel4 = generatePanel(4)
    val panel5 = generatePanel(5)
    val panel6 = generatePanel(6)
    val panel7 = generatePanel(7)
    val panel8 = generatePanel(8)
    val panel9 = generatePanel(9)

    val row1 = new BoxPanel(Orientation.Horizontal) {
      contents += panel1
      contents += panel2
      contents += panel3
    }

    val row2 = new BoxPanel(Orientation.Horizontal) {
      contents += panel4
      contents += panel5
      contents += panel6
    }

    val row3 = new BoxPanel(Orientation.Horizontal) {
      contents += panel7
      contents += panel8
      contents += panel9
    }


    contents = new BoxPanel(Orientation.Vertical) {
      contents += row1
      contents += row2
      contents += row3

      background = bluishGray

      listenTo(keys)
      reactions += {
        case KeyPressed(_, Key.Left, _, _) =>
          panel1.background = bluishGray
        case KeyPressed(_, Key.Right, _, _) =>
          background = bluishSilver
        case KeyPressed(_, Key.Up, _, _) =>
          background = bluishGray
        case KeyPressed(_, Key.Down, _, _) =>
          background = bluishGray
      }
      focusable = true
      requestFocus
    }
  }
}
