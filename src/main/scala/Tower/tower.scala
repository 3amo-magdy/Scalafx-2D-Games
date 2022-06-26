package Tower

import game.game
import scalafx.Includes.when
import scalafx.application.{JFXApp3, Platform}
import scalafx.concurrent.{ScheduledService, Task}
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color.{Blue, DarkBlue, Red, White}
import scalafx.scene.shape.Rectangle
import scalafx.scene.{Group, Scene}

object tower extends game {
  def WIDTH = 12

  def HEIGHT = 14

  def BARWIDTH = 4

  def fxCELLWIDTH = 40

  var board = Array.fill(HEIGHT, WIDTH)(false)
  var level = HEIGHT - 1;
  var slidingBar = Array(0, BARWIDTH, -1)
  val Root = new Group()

  val t = ScheduledService.apply(
    Task.apply(() -> {
      if (slidingBar(0) == WIDTH - slidingBar(1) || slidingBar(0) == 0) {
        slidingBar(2) = slidingBar(2) * -1
      }
      slidingBar(0) += slidingBar(2)
      Platform.runLater(() -> {
        draw()
      });
    })
  )

  def Reset() = {
    board = Array.fill(HEIGHT, WIDTH)(false)
    level = HEIGHT - 1;
    slidingBar = Array(0, BARWIDTH, -1)
    draw()
    t.setPeriod(javafx.util.Duration.millis(300))
    t.restart()
  }

  def signal(unit: Unit) = {
    var newSize = 0
    var newColumn = -1
    for (i <- slidingBar(0) until slidingBar(0) + slidingBar(1)) {
      if (level == HEIGHT - 1 || board(level + 1)(i)) {
        newSize += 1
        if (newColumn == -1) {
          newColumn = i
        }
        board(level)(i) = true
      }
    }
    if (newSize == 0) {
      //game.game over
      t.cancel()
      draw()
      val a = new Alert(AlertType.Information, "U suck .. hehe")
      a.showAndWait()
      Reset()
    }
    else if (level == 0) {
      //win condition
      t.cancel()
      draw()
      new Alert(AlertType.Information, "Well Done !").showAndWait()
      Reset()
    }
    else {
      slidingBar(0) = 0
      slidingBar(1) = newSize
      slidingBar(2) = -1
      level -= 1
      t.setPeriod(javafx.util.Duration.millis(30 + 270 * level / HEIGHT))
    }

  }

  def drawBoard(board: Array[Array[Boolean]]): Group = {
    var L = new Group()
    for (i <- 0 until HEIGHT) {
      for (j <- 0 until WIDTH) {
        if (board(i)(j)) {
          val r = new Rectangle()
          r.setX((j) * fxCELLWIDTH)
          r.setY((i) * fxCELLWIDTH)
          r.setWidth(fxCELLWIDTH)
          r.setHeight(fxCELLWIDTH)
          r.fill <== when(r.hover) choose DarkBlue otherwise Blue

          //        r.setEffect(Effect())
          r.setStroke(White)
          r.setStrokeWidth(2)
          r.setArcHeight(3)
          r.setArcWidth(3)
          sfxGroup2jfx(L).getChildren.add(r)
        }
      }
    }
    for (i <- 0 until slidingBar(1)) {
      val r = new Rectangle()
      r.setX(((i + slidingBar(0)) * fxCELLWIDTH))
      r.setY((level) * fxCELLWIDTH)
      r.setWidth(fxCELLWIDTH)
      r.setHeight(fxCELLWIDTH)
      r.fill = Red
      //        r.setEffect(Effect())
      r.setStroke(White)
      r.setStrokeWidth(2)
      r.setArcHeight(3)
      r.setArcWidth(3)
      sfxGroup2jfx(L).getChildren.add(r)
    }
    L
  }

  def draw(): Unit = {
    sfxGroup2jfx(Root).getChildren.clear();
    sfxGroup2jfx(Root).getChildren.add(drawBoard(board));
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Moving Tile"
      scene = new Scene(WIDTH * fxCELLWIDTH, HEIGHT * fxCELLWIDTH) {
        root = Root;
        draw();
        t.setPeriod(javafx.util.Duration.millis(300))
        t.start()

        onKeyPressed = key => {
          key.getCode match {
            case KeyCode.Space.delegate => signal();
            case _ => // do nothing
          }
          draw()
        }
      }
    }
    stage.setResizable(false)
  }
}
