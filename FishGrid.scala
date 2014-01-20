import scala.util.Random

import wator.FISH
import wator.FishGridType
import wator.SHARK
import wator.WATER

// Represents an ocean grid data structure, together with helper functions
class FishGrid(numCells: Int, maxAge: Int, maxEnergy: Int) {
  import scala.util.Random

  val nCells = numCells
  var data: FishGridType = random // this is the initial ocean setup

  // generator for (x, y) coordinate tuples
  def coordinates =
    for (x <- 0 until numCells; y <- 0 until numCells)
      yield (y, x)

  // for debugging with small sizes
  def printGrid: Unit = {
    for (y <- 0 until numCells) {
      for (x <- 0 until numCells) {
        print(data(x)(y)_1)
      }
      println()
    }
  }

  // Initial grid configuration, needed for "oldGrid" painting optimization below
  def invalid: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => (-1, maxAge, maxEnergy))
  }

  // Initial grid configuration, with random placement
  def random: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => (Random.nextInt(3), maxAge, maxEnergy))
  }

  // Initial grid configuration for debugging, with clear field separation
  def splitTank: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => ((
        if (x < numCells / 2) {
          if (y < numCells / 2)
            FISH
          else
            SHARK
        } else WATER), maxAge, maxEnergy))
  }

  // Initial grid configuration, with lonely fish swimming the ocean
  def singleFish: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => ((if (x == 0 && y == 0) FISH else WATER), maxAge, maxEnergy))
  }

  // computation of one simulation round
  // ############################################################################################
  // ####################### ADD YOUR CODE HERE #################################################
  // ############################################################################################
  def nextGeneration: Unit = {
    def inRandomDirection(c: Tuple2[Int, Int], f: (Int, Int) => Boolean): Tuple2[Int, Int] = {
      val directions = Random.shuffle(List((-1, 0), (1, 0), (0, -1), (0, 1)))
      def internalIter(index: Int): Tuple2[Int, Int] = {
        if (index == directions.size) c
        else {
          val x = (c._1 + directions(index)._1 + numCells) % numCells
          val y = (c._2 + directions(index)._2 + numCells) % numCells
          if (f(x, y)) (x, y)
          else internalIter(index + 1)
        }
      }
      internalIter(0)
    }

    def spawnDescendant(c: Tuple2[Int, Int]) {
      val old = data(c._1)(c._2)
      if (old._2 > 0) data(c._1)(c._2) = (old._1, old._2 - 1, old._3)
      else {
        inRandomDirection(c, (x, y) => {
          if (data(x)(y)._1 == WATER) {
            data(x)(y) = (old._1, maxAge, maxEnergy)
            data(c._1)(c._2) = (old._1, maxAge, old._3)
            true
          } else false
        })
      }
    }

    def moveInRandomDirection(c: Tuple2[Int, Int]): Tuple2[Int, Int] = {
      inRandomDirection(c, (x, y) => {
        if (data(x)(y)._1 == WATER) {
          data(x)(y) = data(c._1)(c._2)
          data(c._1)(c._2) = (WATER, maxAge, maxEnergy)
          true
        } else false
      })
    }

    coordinates.foreach(c => {
      data(c._1)(c._2)._1 match {
        case FISH => {
          val newC = moveInRandomDirection(c)
          spawnDescendant(newC)
        }

        case SHARK => {
          val old = data(c._1)(c._2)

          if (old._3 > 0) {
            //has not starved yet
            data(c._1)(c._2) = (old._1, old._2, old._3 - 1)

            //try to eat fish
            var newC = inRandomDirection(c, (x, y) => {
              if (data(x)(y)._1 == FISH) {
                //println("shark ate fish")
                data(x)(y) = (old._1, old._2, maxEnergy)
                data(c._1)(c._2) = (WATER, maxAge, maxEnergy)
                true
              } else false
            })

            //if that failed, moved randomly
            if (c == newC) newC = moveInRandomDirection(c)

            spawnDescendant(newC)

            //must starve
          } else {
            //println("shark starved")
            data(c._1)(c._2) = (WATER, maxAge, maxEnergy)
          }
        }

        case WATER =>
      }
    })
  }
}