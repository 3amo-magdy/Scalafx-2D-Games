package Tetris

import game.game
import javafx.scene.shape.Rectangle
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.IntegerProperty
import scalafx.concurrent.{ScheduledService, Task}
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color.{Blue, White}
import scalafx.scene.text.{Font, Text}
import scalafx.scene.{Group, Scene}

import scala.io.Source
import scala.util.Random

object tetris extends game {
  def WIDTH = 16

  def HEIGHT = 12

  def fxCELLWIDTH = 30

  def presets = {
    var L = List[List[Array[Boolean]]]()
    var shp = List[Array[Boolean]]()
    Source.fromFile("shapes.txt").getLines().foreach((x: String) => {
      println(x);
      if (x.equals(".")) {
        L = L.appended(shp)
        shp = List()
      }
      else {
        var row = new Array[Boolean](x.length)
        var i = 0;
        x.foreach((c) => {
          if (c == '0') {
            row(i) = false
          }
          else {
            row(i) = (true)
          }
          i += 1;
        })
        shp = shp.appended(row)
      }
    })
    L
  }

  val preset_shapes = presets.map((p) => {
    new shape(WIDTH / 2, 0, p.toArray, randomColor());
  })
  var shapes: List[shape] = List()
  var board = Array.fill[Boolean](HEIGHT, WIDTH)(false)
  var fxBoard = new Group();
  var score = new Text() {
    this.text = "SCORE: __"
    this.layoutX = 2
    this.layoutY = 15
    this.stroke = Blue
    this.font.value = new Font("Comic-sans", 13)
  }

  var SCORE = new IntegerProperty() {
    onChange { (_, _, newValue) =>
      Platform.runLater(() -> {
        score.setText(s"SCORE: ${newValue.toString()}");
      });
    }
  };
  SCORE.set(0);

  def randomColor() = (scalafx.scene.paint.Color.rgb(Random.nextInt(255), Random.nextInt(255), Random.nextInt(255)))


  def generateShape(): shape = {
    val newshape = preset_shapes(math.abs(Random.nextInt(preset_shapes.length)))
    new shape(WIDTH / 2, 0, newshape.data.clone(), newshape.color)
  }

  var s = generateShape()

  def nextShape() = {
    for (i <- 0 until s.data.length) {
      for (j <- 0 until s.data(0).length) {
        board(s.y + i)(s.x + j) = board(s.y + i)(s.x + j) || s.data(i)(j)
      }
    }
    shapes = shapes.appended(s);
    s = generateShape();
    SCORE.set(SCORE.get() + 5);
  }

  def drawShapeOnTop(x: shape) = {
    for (i <- 0 until x.data.length) {
      for (j <- 0 until x.data(i).length) {
        if (x.data(i)(j)) {
          val r = new Rectangle()
          r.setX((x.x + j) * fxCELLWIDTH)
          r.setY((x.y + i) * fxCELLWIDTH)
          r.setWidth(fxCELLWIDTH)
          r.setHeight(fxCELLWIDTH)
          r.setFill(x.color)
          r.setStroke(White)
          r.setArcHeight(2.4)
          sfxGroup2jfx(fxBoard).getChildren.add(r)
        }
      }
    }
  }

  def reduce(): Unit = {
    for (i <- 0 until HEIGHT) {
      var rowDone = true;
      board(i).foreach((x: Boolean) => {
        rowDone &&= x
      })
      if (rowDone) {
        SCORE.set(SCORE.get() + 10)
        board = (Array.fill(1, WIDTH)(false)).++(board.take(i)).++(board.drop(i + 1))
        shapes.foreach((a: shape) => {
          if (a.y <= i && a.y + a.data.length >= i) {
            a.data = a.data.take(i - a.y).++(a.data.drop(i - a.y + 1))
          }
          if (a.y <= i) {
            a.y += 1
          }
        })
        draw()
      }
    }
  }

  def draw(): Unit = {
    Platform.runLater(() -> {
      fxBoard.getChildren.clear()
      shapes.foreach(drawShapeOnTop)
      drawShapeOnTop(s)
    });
  }

  def go_dooown(): Unit = {
    while (do_the_thing()) {
      // aaaaaaaugh
    }
  }

  def resetGame() = {
    shapes = List()
    board = Array.fill[Boolean](HEIGHT, WIDTH)(false)
    fxBoard.getChildren.clear();
  }

  def do_the_thing(): Boolean = {
    val row_under_index = (s.y + s.data.size)
    if (row_under_index == HEIGHT) {
      nextShape();
      false;
    }
    else {
      val row_under = board(row_under_index)
      val last_row = s.data.last
      var CanGoDown = true;
      for (i <- 0 until s.data.length) {
        for (j <- 0 until s.data(i).length) {
          if ((s.x + j) >= WIDTH || (s.y + 1 + i) >= HEIGHT || s.data(i)(j) && board(s.y + 1 + i)(s.x + j)) {
            CanGoDown = false;
          }
        }
      }
      if (CanGoDown) {
        s.pushDown();
      }
      else {
        if (s.y <= 0) {
          println(s"SCORE : ${SCORE.get()}")
          SCORE.set(0)
          //game.game over
          resetGame()
        }
        else {
          nextShape()
        }
      }
      draw()
      CanGoDown
    }
  }

  def canPushL(): Boolean = {
    var returned = true;
    if (s.x < 1) {
      returned = false
    }
    else {
      for (i <- 0 until s.data.length) {
        for (j <- 0 until s.data(i).length) {
          if ((s.x - 1 + j) >= WIDTH || (s.y + i) >= HEIGHT || s.data(i)(j) && board(s.y + i)(s.x - 1 + j)) {
            returned = false;
          }
        }
      }
    }
    returned
  }

  def canPushR(): Boolean = {
    var returned = true;
    if (s.x + s.data(0).length >= WIDTH) {
      returned = false
    }
    else {
      for (i <- 0 until s.data.length) {
        for (j <- 0 until s.data(i).length) {
          if ((s.x + 1 + j) >= WIDTH || (s.y + i) >= HEIGHT || s.data(i)(j) && board(s.y + i)(s.x + 1 + j)) {
            returned = false;
          }
        }
      }
    }
    returned
  }

  def canRotate(cw: Boolean): Boolean = {
    val hypo_s = new shape(s.x, s.y, s.data.clone(), s.color)
    if (cw) {
      hypo_s.rotateCW();
    }
    else {
      hypo_s.rotateCCW();
    }
    var returned = true;
    for (i <- 0 until hypo_s.data.length) {
      for (j <- 0 until hypo_s.data(i).length) {
        if ((s.x + j) >= WIDTH || (s.y + i) >= HEIGHT || hypo_s.data(i)(j) && board(s.y + i)(s.x + j)) {
          returned = false;
        }
      }
    }
    returned
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Tetris"
      val t = ScheduledService.apply(
        Task.apply(() -> {
          do_the_thing(); reduce();
        })
      )
      t.setPeriod(javafx.util.Duration.seconds(1))
      scene = new Scene(WIDTH * fxCELLWIDTH, HEIGHT * fxCELLWIDTH) {
        val Root = new Group()
        sfxGroup2jfx(Root).getChildren.addAll(fxBoard, score);
        root = Root;
        onKeyPressed = (key) => {
          key.getCode match {
            case KeyCode.LEFT.delegate => if (canPushL()) s.pushL(); draw()
            case KeyCode.RIGHT.delegate => if (canPushR()) s.pushR(); draw()
            case KeyCode.SPACE.delegate => if (canRotate(true)) s.rotateCW(); draw()
            case KeyCode.ALT.delegate => if (canRotate(false)) s.rotateCCW(); draw()
            case KeyCode.SHIFT.delegate => go_dooown(); draw()
            case _ => // do nothing
          }
        }
        t.start()
      }
    }
    stage.setResizable(false)
  }
}
