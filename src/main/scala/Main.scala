
import java.util

object Main {
  def swap(array: Array[Array[Int]],i:Int,j:Int,a:Int,b:Int): Unit ={
        val temp = array(i)(j)
        array(i)(j) = array(a)(b)
        array(a)(b) = temp
      }
    def tileRow(movingTile:Int)=movingTile/2
    def tileCol(movingTile:Int)=movingTile%2
    def pushR(movingTile:Int,board:Array[Array[Int]]) = {swap(board,tileRow(movingTile),tileCol(movingTile),tileRow(movingTile),tileCol(movingTile)+1);movingTile+1}

  def main(args: Array[String]): Unit = {
    var arr = Array(Array(0,1),Array(2,3))
//    swap(arr,0,0,1,0)
    pushR(2,arr)
    arr.foreach(row=>{row.foreach(x=> print(s"${x} "));println();})
    val arr2 = arr.map(row=>row.clone())
    pushR(0,arr2)
    arr2.foreach(row=>{row.foreach(x=> print(s"${x} "));println();})
    arr.foreach(row=>{row.foreach(x=> print(s"${x} "));println();})

  }
}