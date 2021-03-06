import scala.actors.Actor
package object wator {
  // Tank grid: 2D array with following elements per cell:
  // 1: Cell content (0 - water, 1- fish, 2-shark)
  // 2: Egg Time Limit Fish/sharks
  // 3: Starvation Time Sharks
  type Cell = Tuple3[Int, Int, Int]
  type FishGridType = Array[Array[Int]]
  val WATER = 0
  val FISH = 1
  val SHARK = 2
}