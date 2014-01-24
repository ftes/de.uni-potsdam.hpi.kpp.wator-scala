import scala.actors.Actor
import scala.util.Random

import wator.Cell
import wator.FISH
import wator.FishGridType
import wator.SHARK
import wator.WATER
import scala.util.Random

// Represents an ocean grid data structure, together with helper functions
class FishGrid(initialSetup: Array[Array[Int]], eggTimeLimitFish: Int, eggTimeLimitSharks: Int,
  starvationTimeSharks: Int, numRounds: Int, callback: Array[Array[Int]] => Unit) {
  case class Done(x: Int, y: Int, content: Int)
  case class Neighbours(t: Actor, l: Actor, r: Actor, b: Actor)
  case class Content(from: Actor, content: Int)
  case class MyOccupantIsComing(cell: Cell)
  case object Ack
  case class Deny(newContent: Int)
  val ACT_FIRST = 0
  val ACT_LAST = 1
  val SPAWN = 0
  val MOVE = 1

  val numCells = initialSetup.size
  val water = (WATER, 0, 0)
  val fish = (FISH, eggTimeLimitFish, 0)
  val shark = (SHARK, eggTimeLimitSharks, starvationTimeSharks)
  val types = Array(water, fish, shark)

  val result = Array.fill(numCells, numCells)(0)
  var receivedResults = 0

  def getType(typ: Int) = types(typ)

  val coordinator = new Actor {
    def act {
      while(true) {
        receive {
          case Done(x, y, content) => {
            result(y)(x) = content
            receivedResults += 1
            if (receivedResults == numCells * numCells) {
              callback(result)
              exit
            }
          }
        }
      }
    }
  }
  coordinator.start

  def createCellActor(x: Int, y: Int, content: Int): Actor = new CellActor(coordinator, x, y, numRounds, types(content))

  var data = Array.tabulate(numCells, numCells)(
    (y, x) => createCellActor(x, y, initialSetup(y)(x)))

  def start {
    //inform actors about neighbours, and start them
    for (y <- 0 until numCells; x <- 0 until numCells) {
      val xLeft = (x - 1 + numCells) % numCells
      val xRight = (x + 1) % numCells
      var yTop = (y - 1 + numCells) % numCells
      val yBottom = (y + 1) % numCells

      data(y)(x).start

      //    printf("Neighbours for (%d,%d): top(%d,%d), left(%d,%d), right(%d,%d), bottom(%d,%d)\n", x, y,
      //        x, yTop, xLeft, y, xRight, y, x, yBottom)

      data(y)(x) ! Neighbours(data(yTop)(x), data(y)(xLeft), data(y)(xRight), data(yBottom)(x))
    }
  }

  class CellActor(coordinator: Actor, x: Int, y: Int, var numberOfRounds: Int, var cell: Cell) extends Actor {
    var neighbours: List[Actor] = null
    val whenToAct = (x + y) % 2

    var doneWithRound = false

    var receivedContent: Map[Actor, Int] = Map()

    def inDirection(f: (Actor, Int) => Boolean): Boolean = {
      def internalIter(index: Int): Boolean = {
        if (index != neighbours.size) {
          val neighbour = neighbours(index)
          val neighbourContent = receivedContent(neighbour)
          if (f(neighbour, neighbourContent)) true
          else internalIter(index + 1)
        } else false
      }
      internalIter(0)
    }

    def moveOrSpawn(fishOrWater: Int, spawnOrMove: Int): Boolean = {
      val cellToTransfer = if (spawnOrMove == SPAWN) types(cell._1) else cell
      inDirection((neighbour, content) => {
        if (content == fishOrWater) {
          neighbour !? MyOccupantIsComing(cellToTransfer) match {
            case Ack => {
              receivedContent + (neighbour -> cell._1)
              if (spawnOrMove == MOVE) cell = water
              true
            }
            case Deny(newContent) => {
              receivedContent + (neighbour -> newContent)
              false
            }
          }
        } else false
      })
    }

    def moveTo(fishOrWater: Int): Boolean = moveOrSpawn(fishOrWater, MOVE)
    def spawnDescendant: Boolean = moveOrSpawn(WATER, SPAWN)

    def informNeighbours = neighbours.foreach(n => { n ! Content(this, cell._1) })

    def performRound {
      if (numberOfRounds == 0) {
        coordinator ! Done(x, y, cell._1)
        if (whenToAct == ACT_FIRST) informNeighbours
        exit
      } else {
        //printf("%d,%d with content %d performing round, %d rounds to go\n", x, y, cell._1, numberOfRounds)
        numberOfRounds -= 1
        if (doneWithRound == false && cell._1 != WATER) {
          neighbours = Random.shuffle(neighbours)

          //since receiving the content of the neighbours, the following things can have happened
          // - fish on neighbouring cell got eaten by shark -> now shark is on field
          // - shark moved to field
          // - fish moved to field
          //what can not have happened: field was occupied, but now is water -> neighbour cells are passive atm

          //spawn descendant
          if (cell._2 == 0) {
            cell = (cell._1, types(cell._1)._2, cell._3)
            spawnDescendant
          } else {
            cell = (cell._1, cell._2 - 1, cell._3)
            cell._1 match {
              case FISH => moveTo(WATER)
              case SHARK => {
                //has to die
                if (cell._3 == 0) cell = water
                else {
                  cell = (cell._1, cell._2, cell._3 - 1)
                  if (!moveTo(FISH)) moveTo(WATER)
                }
              }
            }
          }
        }

        receivedContent = Map()
        doneWithRound = false
        informNeighbours
      }
    }

    def act {
      while(true) { //vs while, because it allows scheduler to reschedule between iterations -> does not block thread
        receive { //vs receive, receive allocates thread and saves state, whereas react would need to bind to a thread each time
          case Neighbours(t, l, r, b) => {
            neighbours = List(t, l, r, b)
            if (receivedContent.size == 4) performRound
            if (whenToAct == ACT_LAST) {
              informNeighbours
            }
          }

          case Content(from, content) => {
            receivedContent += (from -> content)
            //printf("%d,%d: Received data from %d neighbours\n", x, y, receivedContent.size)
            if (neighbours != null && receivedContent.size == 4) performRound
          }

          case MyOccupantIsComing(incoming) => {
            (incoming._1, cell._1) match {
              case (SHARK, FISH | WATER) | (FISH, WATER) => {
                //shark ate fish?
                var restoreStarvationTime = incoming._1 == SHARK && cell._1 == FISH
                reply(Ack)
                doneWithRound = true
                cell = (incoming._1, incoming._2, if (restoreStarvationTime) starvationTimeSharks else incoming._3)
              }
              case _ => reply(Deny(cell._1))
            }
          }
        }
      }
    }
  }
}