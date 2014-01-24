import java.io.PrintStream

import scala.Array.canBuildFrom
import scala.util.Random

import wator.FISH
import wator.FishGridType
import wator.SHARK
import wator.WATER

object Wator extends App {
  val types = Array(SHARK, FISH, WATER)
  def randomType: Int = types(Random.nextInt(3))

  // Initial grid configuration, with random placement
  def random(numCells: Int): FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (y, x) => randomType)
  }

  // Initial grid configuration for debugging, with clear field separation
  def splitTank(numCells: Int): FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (y, x) => (
        if (x < numCells / 2) {
          if (y < numCells / 2) FISH
          else SHARK
        } else WATER))
  }

  // Initial grid configuration, with lonely fish swimming the ocean
  def singleFish(numCells: Int): FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (y, x) => (if (x == 0 && y == 0) FISH else WATER))
  }

  def printOutput(grid: FishGridType, out: PrintStream) {
    val otherMapping = Map(WATER -> "w", FISH -> "f", SHARK -> "s")
    grid.foreach(
      line => out.println(
        ("" /: line.map(item => otherMapping(item))) { _ + _ }))
    out.close()
  }

  val shift = if (args.size == 5) 0 else 1
  val numberOfRounds = args(1 - shift).toInt
  val eggTimeLimitFish = args(2 - shift).toInt
  val eggTimeLimitSharks = args(3 - shift).toInt
  val starvationTimeSharks = args(4 - shift).toInt

  var grid: FishGridType = null

  if (args.size == 5) {
    val inFileName = args(0)
    val mapping = Map("w" -> WATER, "f" -> FISH, "s" -> SHARK)
    grid =
      scala.io.Source.fromFile(inFileName).getLines().map(
        line => line.toCharArray.map(
          typ => mapping(typ.toString()))).toArray
  } else grid = singleFish(4)

  //maybe problem if written from other thread? works with def, but not with val
  def outFile = new PrintStream("output.txt")
  new FishGrid(grid, eggTimeLimitFish, eggTimeLimitSharks, starvationTimeSharks,
    numberOfRounds, printOutput(_, outFile)).start
}