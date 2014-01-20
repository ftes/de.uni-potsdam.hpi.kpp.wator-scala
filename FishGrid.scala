import scala.util.Random

import wator.FISH
import wator.FishGridType
import wator.SHARK
import wator.WATER

// Represents an ocean grid data structure, together with helper functions
class FishGrid(numCells: Int, eggTimeLimitFish: Int, eggTimeLimitSharks: Int, starvationTimeSharks: Int) {
  import scala.util.Random
  
  var computed = Array.ofDim[Boolean](numCells, numCells)

  val water = (WATER, 0, 0)
  val fish = (FISH, eggTimeLimitFish, 0)
  val shark = (SHARK, eggTimeLimitSharks, starvationTimeSharks)
  val types = Array(water, fish, shark)
  
  def getType(typ: Int) = types(typ)
  
  def randomType(): Tuple3[Int, Int, Int] = types(Random.nextInt(3))
  
  val nCells = numCells
  var data: FishGridType = splitTank // this is the initial ocean setup

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
      (x, y) => (-1, 0, 0))
  }

  // Initial grid configuration, with random placement
  def random: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => randomType)
  }

  // Initial grid configuration for debugging, with clear field separation
  def splitTank: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => (
        if (x < numCells / 2) {
          if (y < numCells / 2) fish
          else shark
        } else water))
  }

  // Initial grid configuration, with lonely fish swimming the ocean
  def singleFish: FishGridType = {
    return Array.tabulate(numCells, numCells)(
      (x, y) => (if (x == 0 && y == 0) fish else water))
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
            data(x)(y) = types(old._1)
            computed(x)(y) = true
            data(c._1)(c._2) = (old._1, types(old._1)._2, old._3)
            true
          } else false
        })
      }
    }

    def moveInRandomDirection(c: Tuple2[Int, Int]): Tuple2[Int, Int] = {
      inRandomDirection(c, (x, y) => {
        if (data(x)(y)._1 == WATER) {
          data(x)(y) = data(c._1)(c._2)
          data(c._1)(c._2) = water
          computed(x)(y) = true
          true
        } else false
      })
    }
    
    computed = Array.ofDim[Boolean](numCells, numCells)

    coordinates.foreach(c => {
      (data(c._1)(c._2)._1, computed(c._1)(c._2)) match {
        case (FISH, false) => {
          val newC = moveInRandomDirection(c)
          spawnDescendant(newC)
        }

        case (SHARK, false) => {
          val old = data(c._1)(c._2)

          if (old._3 > 0) {
            //has not starved yet
            data(c._1)(c._2) = (old._1, old._2, old._3 - 1)

            //try to eat fish
            var newC = inRandomDirection(c, (x, y) => {
              if (data(x)(y)._1 == FISH) {
                //println("shark ate fish")
                data(x)(y) = (old._1, old._2, starvationTimeSharks)
                data(c._1)(c._2) = water
                computed(x)(y) = true
                true
              } else false
            })

            //if that failed, moved randomly
            if (c == newC) newC = moveInRandomDirection(c)

            spawnDescendant(newC)

            //must starve
          } else {
            //println("shark starved")
            data(c._1)(c._2) = water
          }
        }

        case _ =>
      }
    })
  }
}