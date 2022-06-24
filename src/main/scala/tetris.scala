import javafx.scene.shape.Rectangle
import scalafx.application.{JFXApp3, Platform}
import scalafx.concurrent.{ScheduledService, Task}
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color.{Blue, Red, White}
import scalafx.scene.{Group, Scene, paint}
import tetris.map

import java.util
import java.util.{Timer, TimerTask}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.Random
object tetris extends JFXApp3 {
  def WIDTH = 16
  def HEIGHT = 12
  def fxCELLWIDTH = 30

  var shapes : List[shape] =  List()
  var board = Array.fill[Boolean](HEIGHT,WIDTH)(false)
  var map: util.HashMap[Int,shape] = new util.HashMap[Int,shape]()
  var fxBoard = new Group();

  def randomColor() = (scalafx.scene.paint.Color.rgb(Random.nextInt(255),Random.nextInt(255),Random.nextInt(255)))


  def generateShape(): shape = {
//    val w = Random.nextInt()%4 + 1
//    val h = Random.nextInt()%4 + 1
//    val data = Array.fill(h,w)(Random.nextBoolean())
    val data = Array(Array(true,true,true,true))
    val color = randomColor()
    new shape(WIDTH/2,0,data,color);
  }

  var s =  generateShape()
  def nextShape() = {
    for(i<-0 until s.data.length){
      for(j<-0 until s.data(0).length){
        board(s.y+i)(s.x+j)=board(s.y+i)(s.x+j)||s.data(i)(j)
        map.put((s.y+i)*WIDTH+s.x+j,s);
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
          r.setFill(x.color)
          r.setStroke(White)
          r.setArcHeight(2.4)
          sfxGroup2jfx(fxBoard).getChildren.add(r)
        }
      }
    }
  }
  def reduce():Unit={
    for(i<-0 until HEIGHT){
      var rowDone = true;
      board(i).foreach((x:Boolean)=>{
        rowDone&&=x
      })
      if(rowDone){
        print(s"row ${i} done")
        board = (Array.fill(1,WIDTH)(false)).++(board.take(i)).++(board.drop(i+1))
        shapes.foreach((a:shape)=>{
          if(a.y<=i&&a.y+a.data.length>=i){
            a.data=a.data.take(i-a.y).++(a.data.drop(i-a.y+1))
            a.y+=1
          }
        })
        draw()

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
  def go_dooown():Unit={
    while(do_the_thing()){
      // aaaaaaaugh
    }
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
      for(i<-0 until s.data.length){
        for(j<- 0 until s.data(i).length){
          if((s.x+j)>=WIDTH||(s.y+1+i)>=HEIGHT||s.data(i)(j)&&board(s.y+1+i)(s.x+j)){
            CanGoDown = false;
          }
        }
      }
      if (CanGoDown) {
        s.pushDown();
      }
      else {
        nextShape()
      }
      draw()
      CanGoDown
    }
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Tetris"
      val t = ScheduledService.apply(
         Task.apply(()-> {do_the_thing();reduce();})
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
          if(s.x+s.data(0).length>=WIDTH){
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
            case KeyCode.SPACE.delegate=>if(canRotate(true)) s.rotateCW();draw()
            case KeyCode.ALT.delegate=>if(canRotate(false)) s.rotateCCW();draw()
            case KeyCode.SHIFT.delegate=> go_dooown();draw()

          }
        }
        t.start()
      }
    }
    stage.setResizable(false)
  }
}
