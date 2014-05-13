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
                       32   -> new AWTColor(95, 158, 160),
                       64   -> new AWTColor(153, 229, 255),
                       128  -> new AWTColor(63, 111, 124),
                       256  -> new AWTColor(0, 126, 168),
                       512  -> new AWTColor(0, 96, 128),
                       1024 -> new AWTColor(0, 85, 102),
                       1024 -> new AWTColor(138, 43, 226) )

  // Used to check if board is fully blocked.
  val neighbors = Map(0  -> Array(1, 4),
                      1  -> Array(0, 2, 5),
                      2  -> Array(1, 3, 6),
                      3  -> Array(2, 7),
                      4  -> Array(0, 5, 8),
                      5  -> Array(1, 4, 6, 9),
                      6  -> Array(2, 5, 7, 10),
                      7  -> Array(3, 6, 11),
                      8  -> Array(4, 9, 12),
                      9  -> Array(5, 8, 10, 13),
                      10 -> Array(6, 9, 11, 14),
                      11 -> Array(7, 10, 15),
                      12 -> Array(8, 13),
                      13 -> Array(9, 12, 14),
                      14 -> Array(10, 13, 15),
                      15 -> Array(11, 14))

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

  def isLoser(): Boolean = {
    val available = (0 to 15).filter(x => (labels(x).background == cream))
    if (available.size > 0)
      false
    else {
      for (cell <- 0 to 15) {
        for (neighbor <- neighbors(cell)) {
          if (labels(neighbor).background == labels(cell).background)
            return false
        }
      }
      true
    }
  }

  def isWinner(): Boolean = {
    for (s <- 0 to 15) {
    
      if ((labels(s).background != cream) && ((labels(s).text.toInt) == 2048))
        return true
    } 
    false
  }

  def gameOver(message: String): Unit = {
    val displayMessage = message + " Play again?"
    val res = Dialog.showConfirmation(null, 
				      displayMessage, 
				      optionType=Dialog.Options.YesNo)
    if (res == Dialog.Result.No)
      sys.exit(0)
    
    if (res == Dialog.Result.Yes)
      wipeBoard()
  }

  def wipeBoard(): Unit = {
    for (s <- 0 to 15) {
      labels(s).background = cream
      labels(s).text = null
    }
    score.text = "0"
    computerPlay()
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

    // Points!
    score.text = ((score.text.toInt) + newValue).toString
  }


  def handleMove(position: Int, f: Int => Int, max: Int): Int  = {
    if (labels(position).background == cream) return 0
    val prevText = labels(position).text
    val prevColor = labels(position).background
    var newPosition = f(position)

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
        val temp = f(newPosition)
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
    preferredSize = new Dimension(600, 700)
    maximumSize = new Dimension(600, 700)
    minimumSize = new Dimension(600, 700)

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
            counter += handleMove(s, x => x - 1, maxMoves)
          }
          if (counter > 0) computerPlay()
          if (isLoser) gameOver("You Lose.")
          if (isWinner) gameOver("Winner.")
        }
        case KeyReleased(_, Key.Right, _, _) => {
          var counter = 0
          for (s <- Array(14, 13, 12, 10, 9, 8, 6, 5, 4, 2, 1, 0)) {
            val maxMoves = maxMovesRight(s)
            counter += handleMove(s, x => x + 1, maxMoves)
          }
          if (counter > 0) computerPlay()
          if (isLoser) gameOver("You Lose.")
          if (isWinner) gameOver("Winner.")
        }
        case KeyReleased(_, Key.Up, _, _) => {
          var counter = 0
          for (s <- 4 to 15) {
            val maxMoves = maxMovesUp(s)
            counter += handleMove(s, x => x - 4, maxMoves)
          }
          if (counter > 0) computerPlay()
          if (isLoser) gameOver("You Lose.")
          if (isWinner) gameOver("Winner.")
        }
        case KeyReleased(_, Key.Down, _, _) => {
          var counter = 0
          for (s <- (0 to 11).reverse) {
            val maxMoves = maxMovesDown(s)
            counter += handleMove(s, x => x + 4, maxMoves)
          }
          if (counter > 0) computerPlay()
          if (isLoser) gameOver("You Lose.")
          if (isWinner) gameOver("Winner.")
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
  minimumSize = new Dimension(400,100)
  maximumSize = minimumSize
}

class GameLabel extends GeneralLabel {
  val cream = new AWTColor(255, 255, 238)
  border = new javax.swing.border.LineBorder(cream, 20, true)
  minimumSize = new Dimension(200, 200)
  maximumSize = minimumSize
  foreground = cream // Text color
}

