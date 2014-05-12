import scala.swing._
import scala.swing.event._
import java.awt.event._
import java.awt.{Dimension}
import java.awt.{Color => AWTColor}

object Play extends SimpleSwingApplication {

  val bluishGray = new AWTColor(48, 99, 99)
  val bluishSilver = new AWTColor(210, 255, 255)

  def generateLabel(i: Int): Label = {
    new Label(i.toString) {
      background = bluishSilver
      border = new javax.swing.border.LineBorder(java.awt.Color.WHITE)
      minimumSize = new Dimension(100, 100)
      maximumSize = minimumSize
      xAlignment = Alignment.Center
      yAlignment = Alignment.Center
      opaque = true
    }
  }

  // Ugly for now....
  var label1 = generateLabel(1)
  var label2 = generateLabel(2)
  var label3 = generateLabel(3)
  var label4 = generateLabel(4)
  var label5 = generateLabel(5)
  var label6 = generateLabel(6)
  var label7 = generateLabel(7)
  var label8 = generateLabel(8)
  var label9 = generateLabel(9)
  var label10 = generateLabel(10)
  var label11 = generateLabel(11)
  var label12 = generateLabel(12)
  var label13 = generateLabel(13)
  var label14 = generateLabel(14)
  var label15 = generateLabel(15)
  var label16 = generateLabel(16)

  val row1 = new BoxPanel(Orientation.Horizontal) {
    contents += label1
    contents += label2
    contents += label3
    contents += label4
  }

  val row2 = new BoxPanel(Orientation.Horizontal) {
    contents += label5
    contents += label6
    contents += label7
    contents += label8
  }

  val row3 = new BoxPanel(Orientation.Horizontal) {
    contents += label9
    contents += label10
    contents += label11
    contents += label12
  }

  val row4 = new BoxPanel(Orientation.Horizontal) {
    contents += label13
    contents += label14
    contents += label15
    contents += label16
  }

  def top = new MainFrame {
    title = "2048"
    centerOnScreen
    preferredSize = new Dimension(400, 400)
    maximumSize = new Dimension(400, 400)
    minimumSize = new Dimension(400, 400)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += row1
      contents += row2
      contents += row3
      contents += row4

      background = bluishSilver

      focusable = true
      requestFocus

      listenTo(keys)
      reactions += {
        case KeyPressed(_, Key.Left, _, _) =>
          label1.text = "LEFT";
        case KeyPressed(_, Key.Right, _, _) =>
        case KeyPressed(_, Key.Up, _, _) =>
        case KeyPressed(_, Key.Down, _, _) =>
          label2.text = (label2.text.toInt + 1).toString
      }
    }
  }
}
