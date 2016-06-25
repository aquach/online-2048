package org.quach

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import scala.util.Random

abstract class Direction
case object DirUp extends Direction
case object DirDown extends Direction
case object DirLeft extends Direction
case object DirRight extends Direction

abstract class Error
case object NoOp extends Error
case object GameOver extends Error
case object Victory extends Error

object Constants {
  val DIRECTIONS = List[Direction](DirLeft, DirRight, DirDown, DirUp)
  val GRID_SIZE = 4
}

case class Grid(val tiles: Vector[Option[Int]]) {
  if (tiles.size != Constants.GRID_SIZE * Constants.GRID_SIZE) throw new Exception("Bad grid size! Got " + tiles.size)

  private def gridToVectors(fromFront: Boolean, isHorizontal: Boolean) = {
    val vectors =
      (0 until Constants.GRID_SIZE).map(axis1 =>
        (0 until Constants.GRID_SIZE).map(axis2 => if (isHorizontal) getTile(axis1, axis2) else getTile(axis2, axis1))
      )
    if (fromFront) vectors else vectors.map(_.reverse)
  }

  private def vectorsToGrid(vectors: Seq[Seq[Option[Int]]], fromFront: Boolean, isHorizontal: Boolean) = {
    val unflipped = if (fromFront) vectors else vectors.map(_.reverse)
    (if (isHorizontal) unflipped else unflipped.transpose).flatten.toVector
  }

  private def collapse(vector: Seq[Option[Int]]): Seq[Option[Int]] = vector match {
    case Some(tile1) +: Some(tile2) +: rest if tile1 == tile2 => Some(tile1 + tile2) +: collapse(rest)
    case Some(tile1) +: Some(tile2) +: rest if tile1 != tile2 => Some(tile1) +: collapse(Some(tile2) +: rest)
    case other => other
  }

  def slide(dir: Direction): Grid = {
    val shouldTakeFromFront = dir == DirUp || dir == DirLeft
    val isHorizontal = dir == DirLeft || dir == DirRight

    val vectors = gridToVectors(shouldTakeFromFront, isHorizontal)
    val slidedVectors = vectors.map(_.filter(_.isDefined))
    val collapsedVectors = slidedVectors.map(v => collapse(v).padTo(Constants.GRID_SIZE, None))

    Grid(vectorsToGrid(collapsedVectors, shouldTakeFromFront, isHorizontal))
  }

  def getTile(row: Int, col: Int) =
      tiles(row * Constants.GRID_SIZE + col)

  def printGrid() {
    println("--- --- --- ---")
    (0 until Constants.GRID_SIZE).foreach { row =>
      (0 until Constants.GRID_SIZE).foreach { col =>
        print(getTile(row, col).map(_.toString.padTo(4, ' ')).getOrElse("    "))
      }
      println()
    }
    println("--- --- --- ---")
  }

  def withRandomTwo: Grid = {
    val freeTiles = tiles.zipWithIndex.filter(_._1.isEmpty).map(_._2)
    Grid(tiles.updated(freeTiles(Random.nextInt(freeTiles.size)), Some(2)))
  }

  def makeMove(dir: Direction, addTwo: Boolean = false): Either[Grid, Error] = {
    val slidGrid = slide(dir)
    if (slidGrid == this)
      return Right(NoOp)

    if (tiles.exists(_.exists(_ == 2048)))
      return Right(Victory)

    val resultGrid = if (addTwo) slidGrid.withRandomTwo else slidGrid
    if (resultGrid.isGameOver)
      return Right(GameOver)

    Left(resultGrid)
  }

  def isGameOver = Constants.DIRECTIONS.forall(dir => slide(dir) == this)
}

object Grid {
  def newGrid() =
    Grid(Vector().padTo(Constants.GRID_SIZE * Constants.GRID_SIZE, None)).withRandomTwo.withRandomTwo
}

trait AI {
  def decideMove(g: Grid): Direction
}

class RandomAI extends AI {
  override def decideMove(g: Grid): Direction = {
    val choice = Constants.DIRECTIONS(Random.nextInt(Constants.DIRECTIONS.size))
    if (g.makeMove(choice) == Right(NoOp))
      decideMove(g)
    else
      choice
  }
}

abstract class LookAheadGreedyHeuristicAI(lookaheadDepth: Int) extends AI {
  val GAME_OVER_VALUE = Int.MaxValue / 10
  val VICTORY_VALUE = Int.MinValue / 10

  override def decideMove(g: Grid): Direction =
    decideMove(g, lookaheadDepth)

  private def decideMove(g: Grid, maxDepth: Int): Direction =
    Constants.DIRECTIONS.flatMap(d => {
      val afterSlideGrid = g.makeMove(d)

      val value = afterSlideGrid match {
        case Left(grid) => {
          val possibleGrids = possibleGridsAfterRandomTwo(grid)
          Some(
            if (possibleGrids.isEmpty)
              GAME_OVER_VALUE // All grids lead to game over.
            else
              // Compute expected value by best move across all possible grids.
            possibleGrids.map({ grid =>
              val finalState = if (maxDepth == 1)
              Left(grid)
              else
                grid.makeMove(decideMove(grid, maxDepth - 1))

              // After recursing, translate state into value.
              finalState.fold(heuristic, {
                  case Victory => VICTORY_VALUE
                  case GameOver => GAME_OVER_VALUE
                })
            }).sum / possibleGrids.size
          )
        }
        case Right(NoOp) => None // Never consider a NoOp move as valid.
        case Right(GameOver) => Some(GAME_OVER_VALUE)
        case Right(Victory) => Some(VICTORY_VALUE)
        case _ => ???
      }

      value.map(v => (d, v))
    }).minBy(_._2)._1

  def possibleGridsAfterRandomTwo(g: Grid): Seq[Grid] = {
    val freeTiles = g.tiles.zipWithIndex.filter(_._1.isEmpty).map(_._2)
    freeTiles.map(tileIndex => Grid(g.tiles.updated(tileIndex, Some(2))))
      .filter(!_.isGameOver)
  }

  def heuristic(g: Grid): Int
}

class TileMinimizingAI(lookaheadDepth: Int) extends LookAheadGreedyHeuristicAI(lookaheadDepth) {
  override def heuristic(g: Grid): Int = g.tiles.count(_.isDefined)
}

class HeuristicAI(lookaheadDepth: Int) extends LookAheadGreedyHeuristicAI(lookaheadDepth) {
  override def heuristic(g: Grid): Int =
    (for (
      row <- 0 until Constants.GRID_SIZE;
      col <- 0 until Constants.GRID_SIZE;
      value <- g.getTile(row, col)
    ) yield {
      val multiplier = (row * 2 + 1) + (col * 2 + 1)
      -multiplier * value * value
    }).sum
}

case class GameStats(numGamesPlayed: Int, numGamesWon: Int, tileSum: Int)
object GameRunner {
  val NUM_TRIALS = 10
  def simulateGames(ai: AI): GameStats = {
    val games = (0 until NUM_TRIALS).map(i => { println(s"Trial $i.."); playGame(ai) })
    val numGamesWon = games.count(_._2 == Victory)
    val tileSum = games.map(_._1.tiles.flatten.sum).sum / NUM_TRIALS

    GameStats(NUM_TRIALS, numGamesWon, tileSum)
  }

  def playGame(ai: AI, printMoves: Boolean = false): (Grid, Error) = {
    var grid = Grid.newGrid()
    while (true) {
      if (printMoves) grid.printGrid()
      val move = ai.decideMove(grid)
      if (printMoves) println(move)
      grid.makeMove(move, addTwo = true) match {
        case Left(newGrid) => grid = newGrid
        case Right(error) => return (grid, error)
      }
    }
    return null
  }
}

//println(GameRunner.playGame(new HeuristicAI(2), true)._2)

// Seq(new RandomAI, new TileMinimizingAI(1), new HeuristicAI(1), new HeuristicAI(2)).foreach { ai =>
//   println(ai.getClass.getName)
//   println(GameRunner.simulateGames(ai))
// }

object Online2048 extends JSApp {

  val ai = new HeuristicAI(2)

  def main() {
    println("Hello world!")
  }

  @JSExport
  def getMove(jsGrid: js.Array[js.Array[Double]]): Int = {
    val gridVector = jsGrid.map(row => row.map(_.toInt).map({
      case 0 => None
      case x => Some(x)
    }).toList).toList.flatten.toVector

    val grid = Grid(gridVector)
    val move = ai.decideMove(grid)

    move match {
      case DirLeft => 37
      case DirUp => 38
      case DirRight => 39
      case DirDown => 40
    }
  }
}
