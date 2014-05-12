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

  var position = randomIterator.nextInt(16)
  labels(position).text = "2"
  labels(position).background = lightestBlue 

  position = randomIterator.nextInt(16)
  labels(position).text = "2"
  labels(position).background = lightBlue 

  position = randomIterator.nextInt(16)
  labels(position).text = "2"
  labels(position).background = blue 

  position = randomIterator.nextInt(16)
  labels(position).text = "2"
  labels(position).background = darkBlue 

  def handleMove(position: Int, i: Int, f: (Int, Int) => Int, max: Int): Unit  = {
     if (labels(position).background == cream) return
     val prevText = labels(position).text
     val prevColor = labels(position).background
     var newPosition = f(position, i)
     if (labels(newPosition).background != cream) return
        
     if (max > 1) {
       // Probably a much nicer recursive solution.
       for (x <- 0 to max - 2) {
         val temp = f(newPosition, i)
         if ((temp <= 15) && (temp >= 0) && (labels(temp).background == cream)) {
           newPosition = temp
         }
       }
     }

     labels(position).text = null
     labels(position).background = cream
     labels(newPosition).text = prevText
     labels(newPosition).background = prevColor
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
          for (s <- 0 to 15) {
            if ((s != 0) && (s != 4) && (s != 8) && (s != 12)) {
              var maxMoves = 0;
              if ((s == 1) || (s == 5) || (s == 9) || (s == 13)) {
                maxMoves = 1
              } else if ((s == 2) || (s == 6) || (s == 10) || (s == 14)) {
                maxMoves = 2
              } else {
                maxMoves = 3
              }
              handleMove(s, 1, (x, y) => x - y, maxMoves)
            }
          }
        case KeyReleased(_, Key.Right, _, _) =>
          for (s <- (0 to 15).reverse) {
            if ((s != 3) && (s != 7) && (s != 11) && (s != 15)) {
                handleMove(s, 1, (x, y) => x + y, 0)
            }
          }
        case KeyReleased(_, Key.Up, _, _) =>
          for (s <- (0 to 12 by +4) ++ (1 to 13 by +4) ++ (2 to 14 by +4) ++ (3 to 15 by +4)) {
            if ((s != 0) && (s != 1) && (s != 2) && (s != 3)) {
              handleMove(s, 4, (x, y) => x - y, 0)
            }
          }
        case KeyReleased(_, Key.Down, _, _) =>
          for (s <- (12 to 0 by -4) ++ (13 to 1 by -4) ++ (14 to 2 by -4) ++ (15 to 3 by -4)) {
            if ((s != 12) && (s != 13) && (s != 14) && (s != 15)) {
              handleMove(s, 4, (x, y) => x + y, 0)
            }
          }
      }
    }
  }
}

class GeneralLabel extends Label {
  background = new AWTColor(255, 255, 238)
  xAlignment = Alignment.Center
  yAlignment = Alignment.Center
  font = new Font("Scala", 0, 24)
  opaque = true
}

class ScoreLabel extends GeneralLabel {
  minimumSize = new Dimension(200,50)
  maximumSize = minimumSize
}

class GameLabel extends GeneralLabel {
  border = new javax.swing.border.LineBorder(java.awt.Color.BLACK)
  minimumSize = new Dimension(100, 100)
  maximumSize = minimumSize
  foreground = new AWTColor(255, 255, 238)
}

