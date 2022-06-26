class shape( var x:Int,var y:Int = 0,
             var data: Array[Array[Boolean]],val color:scalafx.scene.paint.Color
           )
{
  def rotateCW()=()->{
    this.data = data.reverse.transpose[Boolean];
  }
  def rotateCCW()=()->{
    this.rotateCW; this.rotateCW; this.rotateCW;
  }
  def pushR() =  x+=1
  def pushL() =  x-=1
  def pushDown() = this.y+=1
}
