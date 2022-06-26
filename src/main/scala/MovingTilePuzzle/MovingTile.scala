package MovingTilePuzzle

import game.game
import javafx.scene.text.Font
import scalafx.Includes.when
import scalafx.application.{JFXApp3, Platform}
import scalafx.concurrent.{ScheduledService, Task}
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color.{Blue, DarkBlue, Grey, LightGrey, White}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.{Group, Scene}

import scala.util.Random

object MovingTile extends game {
  def WIDTH = 2

  def HEIGHT = 4

  def fxCELLWIDTH = 50

  var movingTile: Int = 0;
  var board = Array.ofDim[Int](HEIGHT, WIDTH)
  var folded = Array.ofDim[Int](HEIGHT * WIDTH)
  for (i <- 0 until HEIGHT * WIDTH - 1) {
    folded(i) = i + 1
  }
  folded(HEIGHT * WIDTH - 1) = -1
  for (i <- 0 until HEIGHT) {
    for (j <- 0 until WIDTH) {
      val ran = Random.nextInt(folded.length)
      board(i)(j) = folded(ran)
      if (folded(ran) == -1) {
        movingTile = i * WIDTH + j
      }
      folded = folded.take(ran).++(folded.drop(ran + 1))
    }
  }


  def tileRow(movingTile: Int) = movingTile / WIDTH

  def tileCol(movingTile: Int) = movingTile % WIDTH

  def canPushL(movingTile: Int): Boolean = tileCol(movingTile) - 1 >= 0

  def canPushR(movingTile: Int): Boolean = tileCol(movingTile) + 1 < WIDTH

  def canPushU(movingTile: Int): Boolean = tileRow(movingTile) - 1 >= 0

  def canPushD(movingTile: Int): Boolean = tileRow(movingTile) + 1 < HEIGHT

  def pushL(movingTile: Int, board: Array[Array[Int]]) = {
    swap(board, tileRow(movingTile), tileCol(movingTile), tileRow(movingTile), tileCol(movingTile) - 1); movingTile - 1
  }

  def pushR(movingTile: Int, board: Array[Array[Int]]) = {
    swap(board, tileRow(movingTile), tileCol(movingTile), tileRow(movingTile), tileCol(movingTile) + 1); movingTile + 1
  }

  def pushU(movingTile: Int, board: Array[Array[Int]]) = {
    swap(board, tileRow(movingTile), tileCol(movingTile), tileRow(movingTile) - 1, tileCol(movingTile)); movingTile - WIDTH
  }

  def pushD(movingTile: Int, board: Array[Array[Int]]) = {
    swap(board, tileRow(movingTile), tileCol(movingTile), tileRow(movingTile) + 1, tileCol(movingTile)); movingTile + WIDTH
  }


  def swap(array: Array[Array[Int]], i: Int, j: Int, a: Int, b: Int): Unit = {
    val temp = array(i)(j)
    array(i)(j) = array(a)(b)
    array(a)(b) = temp
  }

  def endTest(board: Array[Array[Int]]): Boolean = {
    var returned = true
    var max = -1
    for (i <- 0 until HEIGHT) {
      for (j <- 0 until WIDTH) {
        if (board(i)(j) != -1) {
          if (board(i)(j) < max) {
            returned = false
          }
          max = board(i)(j)
        }
      }
    }
    returned
  }

  def drawBoard(board: Array[Array[Int]]): Group = {
    var fxBoard = new Group()
    var i = 0;
    var j = 0;
    for (i <- 0 until HEIGHT) {
      for (j <- 0 until WIDTH) {
        val r = new Rectangle()
        r.setX((j) * fxCELLWIDTH)
        r.setY((i) * fxCELLWIDTH)
        r.setWidth(fxCELLWIDTH)
        r.setHeight(fxCELLWIDTH)
        r.fill <== when(r.hover) choose DarkBlue otherwise Blue

        //        r.setEffect(Effect())
        r.setStroke(White)
        r.setStrokeWidth(8)
        r.setArcHeight(3)
        r.setArcWidth(3)
        sfxGroup2jfx(fxBoard).getChildren.add(r)
        val t = new Text() {
          val w = 20
          this.text = board(i)(j).toString
          this.layoutX = (j) * fxCELLWIDTH + fxCELLWIDTH / 2.0 - w / 2
          this.layoutY = (i) * fxCELLWIDTH + 0.5 * fxCELLWIDTH + w / 2
          this.fill = White
          this.font.value = new Font("Comic-sans", w)
        }
        sfxGroup2jfx(fxBoard).getChildren.add(t)
      }
    }
    val r = new Rectangle()
    r.setX(tileCol(movingTile) * fxCELLWIDTH + 4.5)
    r.setY(tileRow(movingTile) * fxCELLWIDTH + 4.5)
    r.setWidth(fxCELLWIDTH - 9)
    r.setHeight(fxCELLWIDTH - 9)
    //        r.setEffect(Effect())
    r.setArcHeight(3)
    r.setArcWidth(3)
    r.fill <== when(r.hover) choose LightGrey otherwise White
    r.stroke = Grey
    r.setStrokeWidth(0.5)
    sfxGroup2jfx(fxBoard).getChildren.add(r)


    fxBoard
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Moving Tile"
      scene = new Scene(WIDTH * fxCELLWIDTH, HEIGHT * fxCELLWIDTH) {
        val Root = new Group()
        root = Root;

        def draw(): Unit = {
          sfxGroup2jfx(Root).getChildren.clear();
          sfxGroup2jfx(Root).getChildren.add(drawBoard(board));
        }

        draw();

        def showSolution() = {
          println("""huuuh""")
          var solution = tileSolver.Solve((board, movingTile, Array.emptyIntArray))
          println(solution.mkString)
          val t = ScheduledService.apply(
            Task.apply(() -> {
              val move = solution.head
              println(move)
              solution = solution.drop(1)
              move match {
                case 0 => movingTile = pushL(movingTile, board)
                case 1 => movingTile = pushD(movingTile, board)
                case 2 => movingTile = pushR(movingTile, board)
                case 3 => movingTile = pushU(movingTile, board)
              }
              Platform.runLater(() -> {
                draw()
              });
            })
          )
          t.setPeriod(javafx.util.Duration.millis(700))
          t.start()
        }

        onKeyPressed = key => {
          key.getCode match {
            case KeyCode.Left.delegate => if (canPushL(movingTile)) movingTile = pushL(movingTile, board);
            case KeyCode.Right.delegate => if (canPushR(movingTile)) movingTile = pushR(movingTile, board)
            case KeyCode.Up.delegate => if (canPushU(movingTile)) movingTile = pushU(movingTile, board);
            case KeyCode.Down.delegate => if (canPushD(movingTile)) movingTile = pushD(movingTile, board);
            case KeyCode.S.delegate => print("s"); showSolution();
            case _ => // do nothing
          }
          draw()
          if (endTest(board)) {
            new Alert(AlertType.Information, "Well Done !").showAndWait()
          }
        }
      }
    }
    stage.setResizable(false)
  }
}
