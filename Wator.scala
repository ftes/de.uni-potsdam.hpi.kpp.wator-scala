import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.Label
import scala.swing.BorderPanel

class Wator(numCells: Int, cellSize: Int, maxAge: Int, maxEnergy: Int) extends SimpleSwingApplication { 
	val grid = new FishGrid(numCells, maxAge, maxEnergy)
	val display = new FishDisplay(grid, cellSize)
	val info = new Label()

	def top = new MainFrame {
		title = "The Wator Ocean" 
		contents = new BorderPanel {
			add(display, BorderPanel.Position.North)
			add(info, BorderPanel.Position.South)
		}
	}
	
	override def main(args: Array[String]):Unit = {
		super.main(args)
		var lastSend = System.currentTimeMillis();
		while (1==1) {
			info.text = "Refresh Delay: "+(System.currentTimeMillis()-lastSend)+" ms"
			lastSend = System.currentTimeMillis()
			grid.nextGeneration
			display.dataChannel.write(grid)
		}
	}
}