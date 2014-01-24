

object MeasureWater extends App {
  // Fire up the Swing application with the given parameters
  val gridSizes = Array(50)
  val eggAndStarvationTimeLimits = Array(1)
  val results = Array()

  val numRounds = 100

  var start: Long = 0

  def printResult(gridSize: Int, eggTimeLimitFish: Int, eggTimeLimitSharks: Int, starvationTimeSharks: Int) {
    val genPerSec = numRounds / ((System.nanoTime - start).toFloat / 1000000000)
    printf("%3d, %3d, %3d, %3d: %5.1f\n", gridSize, eggTimeLimitFish,
      eggTimeLimitSharks, starvationTimeSharks, genPerSec)
  }

  println("Grid size, egg time fish, egg time sharks, starvation time sharks: generations per second")
  for (gridSize <- gridSizes) {
    for (eggTimeLimitFish <- eggAndStarvationTimeLimits) {
      for (eggTimeLimitSharks <- eggAndStarvationTimeLimits) {
        for (starvationTimeSharks <- eggAndStarvationTimeLimits) {
          val startLayout = Wator.splitTank(gridSize)
          val grid = new FishGrid(startLayout, eggTimeLimitFish, eggTimeLimitSharks, starvationTimeSharks,
            numRounds, result => printResult(gridSize, eggTimeLimitFish, eggTimeLimitSharks, starvationTimeSharks))
          start = System.nanoTime
          grid.start
        }
      }
    }
  }
}