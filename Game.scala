import scala.swing._
import scala.swing.event._
import java.awt.event._
import java.awt.{Dimension}
import java.awt.{Color => AWTColor}

object Play extends SimpleSwingApplication {

  val bluishGray = new AWTColor(48, 99, 99)
  val bluishSilver = new AWTColor(210, 255, 255)

  def generateLabel(): Label = {
    new Label() {
      background = bluishSilver
      border = new javax.swing.border.LineBorder(java.awt.Color.WHITE)
      minimumSize = new Dimension(100, 100)
      maximumSize = minimumSize
      xAlignment = Alignment.Center
      yAlignment = Alignment.Center
      opaque = true
    }
  }

  // Look at those side effects.
  val labels = 1 to 16 map { i => generateLabel() }

  val row1 = new BoxPanel(Orientation.Horizontal) {
    contents += labels(0)
    contents += labels(1)
    contents += labels(2)
    contents += labels(3)
  }

  val row2 = new BoxPanel(Orientation.Horizontal) {
    contents += labels(4)
    contents += labels(5)
    contents += labels(6)
    contents += labels(7)
  }

  val row3 = new BoxPanel(Orientation.Horizontal) {
    contents += labels(8)
    contents += labels(9)
    contents += labels(10)
    contents += labels(11)
  }

  val row4 = new BoxPanel(Orientation.Horizontal) {
    contents += labels(12)
    contents += labels(13)
    contents += labels(14)
    contents += labels(15)
  }

  val icon = "o"
  var position = 0
  labels(position).text = icon

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
          if ((position != 0) && (position != 4) && (position != 8) && (position != 12)) {
            labels(position).text = null
            position = position - 1
            labels(position).text = icon
          }
        case KeyPressed(_, Key.Right, _, _) =>
          if ((position != 3) && (position != 7) && (position != 11) && (position != 15)) {
            labels(position).text = null
            position = position + 1
            labels(position).text = icon
          }
        case KeyPressed(_, Key.Up, _, _) =>
          if ((position != 0) && (position != 1) && (position != 2) && (position != 3)) {
            labels(position).text = null
            position = position - 4
            labels(position).text = icon
          }
        case KeyPressed(_, Key.Down, _, _) =>
          if ((position != 12) && (position != 13) && (position != 14) && (position != 15)) {
            labels(position).text = null
            position = position + 4
            labels(position).text = icon
          }
      }
    }
  }
}
