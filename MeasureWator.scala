

object MeasureWater extends App {
  // Fire up the Swing application with the given parameters
  val gridSizes = Array(20, 50, 100)
  val eggAndStarvationTimeLimits = Array(1, 2, 5)
  val results = Array()
  
  val noRounds = 100
  
  println("Grid size, egg time fish, egg time sharks, starvation time sharks: generations per second")
  for (gridSize <- gridSizes) {
    for (eggTimeLimitFish <- eggAndStarvationTimeLimits) {
      for (eggTimeLimitSharks <- eggAndStarvationTimeLimits) {
        for (starvationTimeSharks <- eggAndStarvationTimeLimits) {
          val grid = new FishGrid(gridSize, eggTimeLimitFish, eggTimeLimitSharks, starvationTimeSharks)
          val start = System.nanoTime
          for (i <- 1 to noRounds) grid.nextGeneration
          val genPerSec = noRounds / ((System.nanoTime - start).toFloat / 1000000000)
          printf("%3d, %3d, %3d, %3d: %5.1f\n", gridSize, eggTimeLimitFish,
              eggTimeLimitSharks, starvationTimeSharks, genPerSec)
        }
      }
    }
  }
}