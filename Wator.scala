import wator.FISH
import wator.SHARK
import wator.WATER
import wator.FishGridType
import java.io.PrintWriter
import java.io.File
import java.io.PrintStream

object Wator extends App {
	val inFileName = args(0)
	val numberOfRounds = args(1).toInt
	val eggTimeLimitFish = args(2).toInt
	val eggTimeLimitSharks = args(3).toInt
	val starvationTimeSharks = args(4).toInt
	
	val mapping = Map("w" -> (WATER, 0, 0),
	    "f" -> (FISH, eggTimeLimitFish, 0),
	    "s" -> (SHARK, eggTimeLimitSharks, starvationTimeSharks))
	val otherMapping = Map(WATER -> "w", FISH -> "f", SHARK -> "s")
	
	val grid: FishGridType = 
	  	scala.io.Source.fromFile(inFileName).getLines().map(
	  	    line => line.toCharArray.map(
	  	        typ => mapping(typ.toString()))).toArray
	  	        
	val fishGrid = new FishGrid(grid.size, eggTimeLimitFish, eggTimeLimitSharks, starvationTimeSharks)
	fishGrid.data = grid

	def output(out: PrintStream) {
	  	fishGrid.data.foreach(
	    line => out.println(
	    		("" /: line.map(item => otherMapping(item._1))) {_+_}
	))
	}
	
	for(i <- 1 to numberOfRounds) {
	  fishGrid.nextGeneration
	}
	
	val outFile = new PrintStream("output.txt")
	output(outFile)
	outFile.close()
}