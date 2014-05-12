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

  val colors = Array(lightestBlue, lightBlue, blue, darkBlue, darkerBlue, darkestBlue)
  var colorIndex = 1

  val scoreLabel = new ScoreLabel
  scoreLabel.text = "Score: "

  val score = new ScoreLabel
  score.text = "0"

  val randomIterator = scala.util.Random

  val header = new BoxPanel(Orientation.Horizontal) {
    contents += scoreLabel
    contents += score
  }

  val labels = 1 to 16 map { i => new GameLabel }

  def generateRow(i: Int): BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += labels(i)
      contents += labels(i + 1)
      contents += labels(i + 2)
      contents += labels(i + 3)
    }
  }

  val row1 = generateRow(0)
  val row2 = generateRow(4)
  val row3 = generateRow(8)
  val row4 = generateRow(12)

  val icon = "o"
  var position = randomIterator.nextInt(16)
  labels(position).text = icon
  labels(position).background = lightestBlue 

  def handleMove(i: Int, f: (Int, Int) => Int): Unit  = {
    labels(position).text = null
     position = f(position, i)
     labels(position).text = icon
     colorIndex = (colorIndex + 1) % 5
     labels(position).background = colors(colorIndex)
     score.text = ((score.text.toInt) + 1).toString
  }

  def top = new MainFrame {
    title = "2048"
    background = cream
    centerOnScreen()
    preferredSize = new Dimension(400, 450)
    maximumSize = new Dimension(400, 450)
    minimumSize = new Dimension(400, 450)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += header
      contents += row1
      contents += row2
      contents += row3
      contents += row4

      focusable = true
      requestFocus

      listenTo(keys)
      reactions += {
        case KeyReleased(_, Key.Left, _, _) =>
          if ((position != 0) && (position != 4) && (position != 8) && (position != 12)) {
            handleMove(1, (x, y) => x - y)
          }
        case KeyReleased(_, Key.Right, _, _) =>
          if ((position != 3) && (position != 7) && (position != 11) && (position != 15)) {
            handleMove(1, (x, y) => x + y)
          }
        case KeyReleased(_, Key.Up, _, _) =>
          if ((position != 0) && (position != 1) && (position != 2) && (position != 3)) {
            handleMove(4, (x, y) => x - y)
          }
        case KeyReleased(_, Key.Down, _, _) =>
          if ((position != 12) && (position != 13) && (position != 14) && (position != 15)) {
            handleMove(4, (x, y) => x + y)
          }
      }
    }
  }
}

class GeneralLabel extends Label {
  background = new AWTColor(255, 255, 238)
  xAlignment = Alignment.Center
  yAlignment = Alignment.Center
  opaque = true
}

class ScoreLabel extends GeneralLabel {
  minimumSize = new Dimension(200,50)
  maximumSize = minimumSize
  font = new Font("Scala", 0, 24)
}

class GameLabel extends GeneralLabel {
  border = new javax.swing.border.LineBorder(java.awt.Color.BLACK)
  minimumSize = new Dimension(100, 100)
  maximumSize = minimumSize
  foreground = new AWTColor(255, 255, 238)
}

