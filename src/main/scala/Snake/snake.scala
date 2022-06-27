package Snake

import game.game
import scalafx.Includes.when
import scalafx.application.{JFXApp3, Platform}
import scalafx.concurrent.{ScheduledService, Task}
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.{Group, Scene}
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color.{Black, Blue, DarkBlue, DarkGreen, DarkMagenta, DarkRed, Green, White}
import scalafx.scene.shape.{Circle, Rectangle}

import scala.::
import scala.util.Random

object snake extends game{
  def Step = 1
  def StepTime = 100 //millis
  def HEIGHT:Int = 40
  def WIDTH:Int = 40
  def fxCELLWIDTH = 16
  def fxSNAKECELL = 20

  def RIGHT = Step
  def LEFT = - Step
  def DOWN = WIDTH * Step
  def UP = -WIDTH * Step

  def translate(direction: Int):Int = {
    val pos=  convert(snake.head)
    if(direction == UP && pos._1 ==0) {
      convert(HEIGHT-1,pos._2)
    }
    else if(direction == RIGHT && pos._2 == WIDTH-1){
      convert(pos._1,0)
    }
    else if(direction == LEFT && pos._2 == 0){
      convert(pos._1,WIDTH-1)
    }
    else if(direction == DOWN && pos._1 + Step >= HEIGHT ) {
      convert(0,pos._2)
    }
    else {
      snake.head+direction
    }
  }

  def distance(p1: (Int, Int), p2: (Int, Int)) = Math.sqrt(Math.pow(p1._1-p2._1,2)+Math.pow(p1._2-p2._2,2))

  var score = 0

  def Reset(): Unit = {
    snake= Array((WIDTH * HEIGHT * 0.25 + HEIGHT / 4).toInt)
    direction = RIGHT
    food = WIDTH * HEIGHT / 2 + WIDTH / 2
    Platform.runLater(() -> {
      t.cancel
      val a = new Alert(AlertType.Information, s"Score : ${score}")
      a.showAndWait()
      score = 0;
      t.restart()
      draw();
    });
  }

  val t = ScheduledService.apply(
    Task.apply(() -> {
      var over = false
      snake = (Array(translate(direction))).++(snake.dropRight(1))

      snake.drop(4).foreach(s=>{
        val sx = convert(s)._1
        val sy = convert(s)._2
        val hx = convert(snake.head)._1
        val hy = convert(snake.head)._2
        if(!over && Math.abs(sx-hx)*fxCELLWIDTH<fxSNAKECELL && Math.abs(sy-hy)*fxCELLWIDTH<fxSNAKECELL){
          over = true
          println(s"(${hx},${hy}) - (${sx},${sy}) ")
          Reset();
        }
      })
      if(!over) {
        if (distance(convert(snake.head), convert(food)) * fxCELLWIDTH <= fxSNAKECELL / 1.7) {
          println(s"${distance(convert(snake.head), convert(food))} ,snake: (${convert(snake.head)._1},${convert(snake.head)._2}), food: (${convert(food)._1},${convert(food)._2})")
          snake = snake.++(snake.takeRight(1))
          score += Array(3, 4, 5, 6, 7)(Random.nextInt(4))
          generateFood()
        }
        Platform.runLater(() -> {
          draw();
        });
      }
    })
  )

  var snake : Array[Int]=Array((WIDTH*HEIGHT*0.25+HEIGHT/4).toInt)
  var direction = RIGHT
  var food = WIDTH*HEIGHT/2+WIDTH/2
  var Root = new Group();
  def generateFood()={
    var f = 0
    do{f = Random.nextInt(WIDTH*HEIGHT)}
    while(snake.contains(f));
    this.food=f
  }
  def drawFrame():Group = {
    val snake2d = snake.map(convert)
    val fxBoard = new Group()
    val b = new Rectangle()
    b.setX(0)
    b.setY(0)
    b.setWidth(WIDTH*fxCELLWIDTH)
    b.setHeight(HEIGHT*fxCELLWIDTH)
    b.fill = Black
    sfxGroup2jfx(fxBoard).getChildren.add(b)
    val r = new Circle()
    r.centerX = ((convert(food)._2) * fxCELLWIDTH)
    r.centerY = (convert(food)._1 * fxCELLWIDTH)
    r.radius = (fxSNAKECELL/2)
    r.fill <== when(r.hover) choose Green otherwise DarkGreen
    r.setStrokeWidth(0)
    sfxGroup2jfx(fxBoard).getChildren.add(r)
    for(i<-0 until snake2d.length) {
      val r = new Rectangle()
      r.setX((snake2d(i)._2 * fxCELLWIDTH)-fxSNAKECELL/2)
      r.setY((snake2d(i)._1 * fxCELLWIDTH)-fxSNAKECELL/2)
      r.setWidth(fxSNAKECELL)
      r.setHeight(fxSNAKECELL)
      if(i!=0) {
        r.fill = Blue
      }
      r.setStrokeWidth(0)
        if(i==0) {
          r.fill = DarkRed
        }
      sfxGroup2jfx(fxBoard).getChildren.add(r)
    }
    fxBoard
  }

  def convert(x:Int) = (x/WIDTH,x%WIDTH)
  def convert(row:Int,col:Int) = row*WIDTH+col
  def draw() = {
    sfxGroup2jfx(Root).getChildren.clear()
    sfxGroup2jfx(Root).getChildren.add(drawFrame())
  }
  var lastChange = time()
  def time() = {
    System.currentTimeMillis()
  }
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Moving Tile"
      scene = new Scene(WIDTH * fxCELLWIDTH, HEIGHT * fxCELLWIDTH) {
        root = Root;

        draw();
        t.setPeriod(javafx.util.Duration.millis(20))
        t.start()

        onKeyPressed = key => {
          key.getCode match {
            case KeyCode.Up.delegate => if((direction == LEFT ||direction == RIGHT) && time - lastChange > StepTime/2){direction=UP};lastChange=time()
            case KeyCode.Down.delegate => if((direction == LEFT ||direction == RIGHT )&&time - lastChange > StepTime/2){direction=DOWN};lastChange=time()
            case KeyCode.Left.delegate => if((direction == UP ||direction == DOWN )&&time - lastChange > StepTime/2){direction=LEFT};lastChange=time()
            case KeyCode.Right.delegate => if((direction == UP ||direction == DOWN )&&time - lastChange > StepTime/2){direction=RIGHT};lastChange=time()
            case _ => // do nothing
          }
          draw()
        }
      }
    }
    stage.setResizable(false)
  }
}
