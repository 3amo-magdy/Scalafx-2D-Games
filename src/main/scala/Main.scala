
import MovingTilePuzzle.MovingTile
import Tetris.tetris
import Tower.tower
import game.game
import scalafx.application.JFXApp3

import java.util
import scala.util.Random

object Main {
  def main(args:Array[String]): Unit ={
    val games:Array[game]=Array(MovingTile,tetris,tower);
    games(Random.nextInt(games.length)).Launch();
  }
}