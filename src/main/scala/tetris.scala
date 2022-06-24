import javafx.scene.shape.Rectangle
import scalafx.application.{JFXApp3, Platform}
import scalafx.concurrent.{ScheduledService, Task}
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color.Red
import scalafx.scene.{Group, Scene, paint}

import java.util
import java.util.{Timer, TimerTask}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.Random

object tetris extends JFXApp3 {
  def WIDTH = 8
  def HEIGHT = 12
  def fxCELLWIDTH = 60

  var shapes : List[shape] =  List()
  var board = Array.fill[Boolean](HEIGHT,WIDTH)(false)

  var fxBoard = new Group();

  def randomColor() = paint.Color.Red

  def generateShape(): shape = {
//    val w = Random.nextInt()%4 + 1
//    val h = Random.nextInt()%4 + 1
//    val data = Array.fill(h,w)(Random.nextBoolean())
    val data = Array(Array(true,true,true),Array(true,false,true))
    val color = randomColor()
    print(data)
    new shape(2,0,data,color);
  }

  var s =  generateShape()
  def nextShape() = {
    for(i<-0 until s.data.length){
      for(j<-0 until s.data(0).length){
        board(s.y+i)(s.x+j)=board(s.y+i)(s.x+j)||s.data(i)(j)
      }
    }
    shapes=shapes.appended(s);
    s = generateShape();
  }
  def drawShapeOnTop(x:shape)={
    for(i<-0 until x.data.length){
      for(j<-0 until x.data(i).length){
        if(x.data(i)(j)) {
          val r = new Rectangle()
          r.setX((x.x + j) * fxCELLWIDTH)
          r.setY((x.y + i) * fxCELLWIDTH)
          r.setWidth(fxCELLWIDTH)
          r.setHeight(fxCELLWIDTH)
          r.setFill(Red)
          print(s"${r.getX} , ${r.getY}\n")
          sfxGroup2jfx(fxBoard).getChildren.add(r)
        }
      }
    }
  }
  def draw(): Unit = {
    Platform.runLater(()->{
      fxBoard.getChildren.clear()
      shapes.foreach(drawShapeOnTop)
      drawShapeOnTop(s)
    });
  }

  def do_it(): Unit = {
    val row_under_index = (s.y + s.data.size)
    print(row_under_index);
    if (row_under_index == HEIGHT) {
      nextShape();
    }
    val row_under = board(row_under_index)
    print(util.Arrays.toString(row_under))
    val last_row = s.data.last
    var CanGoDown = true;
    for (i <- 0 until last_row.length) {
      if (last_row(i) && row_under(i+s.x)) {
        //can't go down anymore
        CanGoDown = false
      }
    }
    if (CanGoDown) {
      print(" --- pushing P\n")
      s.pushDown();
      draw()
    }
    else {
      nextShape()
    }
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Tetris"
      val t = ScheduledService.apply(
         Task.apply(()-> {print("yo");do_it()})
         )
      t.setPeriod(javafx.util.Duration.seconds(1))
      scene = new Scene(WIDTH*fxCELLWIDTH,HEIGHT*fxCELLWIDTH){
        root = fxBoard

        def canPushL(): Boolean = {
          var returned = true;
          if(s.x<1){
            returned = false
          }
          else{
            for(i<-0 until s.data.length){
              for(j<- 0 until s.data(i).length){
                if((s.x-1+j)>=WIDTH||(s.y+i)>=HEIGHT||s.data(i)(j)&&board(s.y+i)(s.x-1+j)){
                  returned = false;
                }
              }
            }
          }
          returned
        }
        def canPushR(): Boolean = {
          var returned = true;
          if(s.x>=WIDTH){
            returned = false
          }
          else{
            for(i<-0 until s.data.length){
              for(j<- 0 until s.data(i).length){
                if((s.x+1+j)>=WIDTH||(s.y+i)>=HEIGHT||s.data(i)(j)&&board(s.y+i)(s.x+1+j)){
                  returned = false;
                }
              }
            }
          }
          returned
        }
        def canRotate(cw: Boolean):Boolean={
          val hypo_s = new shape(s.x,s.y,s.data.clone(),s.color)
          if(cw){
            hypo_s.rotateCW();
          }
          else{
            hypo_s.rotateCCW();
          }
          var returned=true;
          for(i<-0 until hypo_s.data.length){
            for(j<- 0 until hypo_s.data(i).length){
              if((s.x+j)>=WIDTH||(s.y+i)>=HEIGHT||hypo_s.data(i)(j)&&board(s.y+i)(s.x+j)){
                returned = false;
              }
            }
          }
          returned
        }
        onKeyPressed = (key)=>{
          key.getCode match {
            case KeyCode.LEFT.delegate => if(canPushL()) s.pushL();draw()
            case KeyCode.RIGHT.delegate => if(canPushR()) s.pushR();draw()
            case KeyCode.ALT.delegate=>if(canRotate(true)) s.rotateCW();draw()
            case KeyCode.SPACE.delegate=>if(canRotate(false)) s.rotateCCW();draw()
          }
        }
        t.start()
      }
    }
    stage.setResizable(false)
  }
}
