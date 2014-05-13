import scala.swing._
import scala.swing.event._
import java.awt.event._
import java.awt.{Dimension}
import java.awt.{Color => AWTColor}

object Play extends SimpleSwingApplication {

  val cream         = new AWTColor(255, 255, 238)
  val colorMap  = Map( 2    -> new AWTColor(144, 238, 144),
                       4    -> new AWTColor(46, 139, 87),
                       8    -> new AWTColor(0, 128, 128),
                       16   -> new AWTColor(0, 206, 209),
                       32   -> new AWTColor(204, 242, 255),
                       64   -> new AWTColor(153, 229, 255),
                       128  -> new AWTColor(63, 111, 124),
                       256  -> new AWTColor(0, 126, 168),
                       512  -> new AWTColor(0, 96, 128),
                       1024 -> new AWTColor(0, 85, 102),
                       1024 -> new AWTColor(138, 43, 226) )

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

  def computerPlay(): Unit = {
    val available = (0 to 15).filter(x => (labels(x).background == cream))
    if (available.size == 0) return
    var position = 0

    if (available.size == 1) {
      position = available(0)
    } else {
      position = available(randomIterator.nextInt(available.size))
    }

    val r = randomIterator.nextInt(100)
    val newValue = if (r % 2 == 0) "2" else "4"
    labels(position).text = newValue
    labels(position).background = colorMap(labels(position).text.toInt)
  }

  // Side Effects!
  computerPlay()

  def mergeLabels(merger: Int, mergeInto: Int): Unit = {
    val newValue = (labels(merger).text.toInt * 2)

    // Wipe the old label.
    labels(merger).text = null
    labels(merger).background = cream

    // Upgrade.
    labels(mergeInto).text = newValue.toString
    labels(mergeInto).background = colorMap(newValue)
  }


  def handleMove(position: Int, i: Int, f: (Int, Int) => Int, max: Int): Int  = {
    if (labels(position).background == cream) return 0
    val prevText = labels(position).text
    val prevColor = labels(position).background
    var newPosition = f(position, i)

    var merge = if (labels(newPosition).background == labels(position).background) true else false

    // We have labels to merge.
    if (merge) {
      mergeLabels(position, newPosition)
      return 1
    }

    // We can't move here.
    if ((labels(newPosition).background != cream) && !merge) return 0
        
    if (max > 1) {
       // Probably a much nicer recursive solution.
      for (x <- 0 to max - 2) {
        val temp = f(newPosition, i)
        if ((temp <= 15) && (temp >= 0)) {
          if (labels(temp).background == cream) {
            newPosition = temp
          } else if (labels(temp).background == labels(position).background) {
            // Short circuit.
            mergeLabels(position, temp)
            return 1
          }
        }
      }
    }

    // Wipe the old position.
    labels(position).text = null
    labels(position).background = cream

    // Make the move.
    labels(newPosition).text = prevText
    labels(newPosition).background = prevColor

    // How should we score?
    score.text = ((score.text.toInt) + 1).toString

    return 1 
  }

  def maxMovesLeft(s: Int): Int = {
    if (Array(1, 5, 9, 13) contains s) {
      1
    } else if (Array(2, 6, 10, 14) contains s) {
      2
    } else {
      3
    }
  }

  def maxMovesRight(s: Int): Int = {
    if (Array(2, 6, 10, 14) contains s) {
      1
    } else if (Array(1, 5, 9, 10) contains s) {
      2
    } else {
      3
    }
  }

  def maxMovesUp(s: Int): Int = {
    if (s < 8) {
      1
    } else if (s < 12) {
      2
    } else {
      3
    }
  }

  def maxMovesDown(s: Int): Int = {
    if (s > 7) {
      1
    } else if (s > 4) {
      2
    } else {
      3
    }
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
      // Seriously violating DRY...
      reactions += {
        case KeyReleased(_, Key.Left, _, _) => {
          var counter = 0
          for (s <- Array(1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15)) {
            val maxMoves = maxMovesLeft(s)
            counter += handleMove(s, 1, (x, y) => x - y, maxMoves)
          }
          if (counter > 0) computerPlay()
        }
        case KeyReleased(_, Key.Right, _, _) => {
          var counter = 0
          for (s <- Array(14, 13, 12, 10, 9, 8, 6, 5, 4, 2, 1, 0)) {
            val maxMoves = maxMovesRight(s)
            counter += handleMove(s, 1, (x, y) => x + y, maxMoves)
          }
          if (counter > 0) computerPlay()
        }
        case KeyReleased(_, Key.Up, _, _) => {
          var counter = 0
          for (s <- 4 to 15) {
            val maxMoves = maxMovesUp(s)
            counter += handleMove(s, 4, (x, y) => x - y, maxMoves)
          }
          if (counter > 0) computerPlay()
        }
        case KeyReleased(_, Key.Down, _, _) => {
          var counter = 0
          for (s <- (0 to 11).reverse) {
            val maxMoves = maxMovesDown(s)
            counter += handleMove(s, 4, (x, y) => x + y, maxMoves)
          }
          if (counter > 0) computerPlay()
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

