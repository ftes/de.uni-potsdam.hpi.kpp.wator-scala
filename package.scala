package object wator {
  // Tank grid: 2D array with following elements per cell:
  // 1: Cell content (0 - water, 1- fish, 2-shark)
  // 2: Age: for spawning new fish/sharks
  // 3: Energy: shark dies if energy == 0
  type FishGridType = Array[Array[Tuple3[Int, Int, Int]]]
  val WATER = 0
  val FISH = 1
  val SHARK = 2
}