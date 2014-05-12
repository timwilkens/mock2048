import scala.swing._
import scala.swing.event._
import java.awt.event._
import java.awt.{Dimension}
import java.awt.{Color => AWTColor}

object Play extends SimpleSwingApplication {

  val cream         = new AWTColor(255, 255, 238)
  val lightestBlue  = new AWTColor(204, 242, 255)
  val lightBlue     = new AWTColor(153, 229, 255)
  val blue          = new AWTColor(63, 111, 127)
  val darkBlue      = new AWTColor(0, 126, 168)
  val darkerBlue    = new AWTColor (0, 96, 128)
  val darkestBlue   = new AWTColor (0, 85, 102)

  def generateLabel(): Label = {
    new Label() {
      background = cream
      border = new javax.swing.border.LineBorder(java.awt.Color.BLACK)
      minimumSize = new Dimension(100, 100)
      maximumSize = minimumSize
      xAlignment = Alignment.Center
      yAlignment = Alignment.Center
      opaque = true
    }
  }

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

  def isWin(): Boolean = {
    val color = labels(0).background
    for(i <- 1 to 15) {
      if (labels(i).background != color)
        return true
    }
    return true
  }

  def top = new MainFrame {
    title = "2048"
    centerOnScreen()
    preferredSize = new Dimension(400, 400)
    maximumSize = new Dimension(400, 400)
    minimumSize = new Dimension(400, 400)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += row1
      contents += row2
      contents += row3
      contents += row4

      background = cream

      focusable = true
      requestFocus

      def displayWinner {
      val res = Dialog.showConfirmation(contents.head,
                                        "Winner! Play Again?",
                                        optionType=Dialog.Options.YesNo,
                                        title = "Winner",
                                        icon = null)
        if (res == Dialog.Result.No)
          sys.exit(0)
      }

      listenTo(keys)
      reactions += {
        case KeyReleased(_, Key.Left, _, _) =>
          if ((position != 0) && (position != 4) && (position != 8) && (position != 12)) {
            labels(position).text = null
            position = position - 1
            labels(position).text = icon
            labels(position).background = lightBlue
//            if (isWin) displayWinner
          }
        case KeyReleased(_, Key.Right, _, _) =>
          if ((position != 3) && (position != 7) && (position != 11) && (position != 15)) {
            labels(position).text = null
            position = position + 1
            labels(position).text = icon
            labels(position).background = blue
          }
        case KeyReleased(_, Key.Up, _, _) =>
          if ((position != 0) && (position != 1) && (position != 2) && (position != 3)) {
            labels(position).text = null
            position = position - 4
            labels(position).text = icon
            labels(position).background = darkBlue
          }
        case KeyReleased(_, Key.Down, _, _) =>
          if ((position != 12) && (position != 13) && (position != 14) && (position != 15)) {
            labels(position).text = null
            position = position + 4
            labels(position).text = icon
            labels(position).background = darkestBlue
          }
      }
    }
  }
}
