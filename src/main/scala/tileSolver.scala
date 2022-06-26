import MovingTile._

import java.util
/*
0 Left
1 Down
2 Right
3 Up
*/


object tileSolver{
  def L = 0
  def D = 1
  def R = 2
  def U = 3
  def Solve(initState:(Array[Array[Int]], Int,Array[Int])):Array[Int] ={
    var  nonvisited = List(initState)
    var visited = List[(Array[Array[Int]], Int,Array[Int])]()
    var returned = Array.emptyIntArray
    var solved = false
    while(!solved && nonvisited.nonEmpty){
//      val state = nonvisited.maxBy(x=>weigh(x._1))
      val state = nonvisited.last
      visited = visited.::(state)
      nonvisited = nonvisited.dropRight(1)
      if(endTest(state._1)){
        returned = state._3
        solved = true
        println(state._3.mkString)
      }
      else{
        if(canPushL(state._2)){
          val newBoard = state._1.map(row=>row.clone())
          val newPos = pushL(state._2,newBoard)
          if(visited.forall(visited=> !Match(visited._1,newBoard))){
            nonvisited=nonvisited.::(newBoard,newPos,state._3.appended(L))
          }
        }
        if(canPushR(state._2)){
          val newBoard = state._1.map(row=>row.clone())
          val newPos = pushR(state._2,newBoard)
          if(visited.forall(visited=> !Match(visited._1,newBoard))){
            nonvisited=nonvisited.::(newBoard,newPos,state._3.appended(R))
          }
        }
        if(canPushD(state._2)){
          val newBoard = state._1.map(row=>row.clone())
          val newPos = pushD(state._2,newBoard)
          if(visited.forall(visited=> !Match(visited._1,newBoard))){
            nonvisited=nonvisited.::(newBoard,newPos,state._3.appended(D))
          }
        }
        if(canPushU(state._2)){
          val newBoard = state._1.map(row=>row.clone())
          val newPos = pushU(state._2,newBoard)
          if(visited.forall(visited=> !Match(visited._1,newBoard))){
            nonvisited=nonvisited.::(newBoard,newPos,state._3.appended(U))
          }
        }
      }
    }
    returned
  }
  def weigh(state:Array[Array[Int]]): Int ={
    //sorted row or column -> 10 points for each
    //nearlysorted :
    0
  }
//  def compareStates(state1:Array[Array[Int]],state2:Array[Array[Int]]): Array[Array[Int]] ={
//    if(weigh(state2)<=weigh(state1)){
//      state1
//    }
//    else{
//      state2
//    }
//  }
  def Match(state1:Array[Array[Int]],state2:Array[Array[Int]]): Boolean = {
    var flag = true
    for(i<- 0 until state1.length){
      flag &&= state1(i) sameElements state2(i)
    }
    flag
  }
}
