import swing._
import event._
import java.awt.event.{ActionListener, ActionEvent}
import java.awt.{Color => AWTColor}
import scala.concurrent.SyncChannel
import java.awt.{Color => AWTColor}
import wator._

// Wrapper class for Swing component painting the ocean
class FishDisplay(initialgrid: FishGrid, cellSize: Int) extends Panel {

  	preferredSize = new Dimension(cellSize*initialgrid.nCells, cellSize*initialgrid.nCells)
	var dataChannel = new SyncChannel[FishGrid]
	var oldData: FishGridType = initialgrid.invalid		// enforce complete repaint on first round
	var disabled: Boolean = false

	// fetching updates directly with an actor was magnitudes slower,
	// since SwingUtilities.invokeLater() was neccessary to be used
	new javax.swing.Timer(1, new ActionListener {		// milliseconds
		def actionPerformed(ae:ActionEvent) { 
			repaint 
		}
    }).start

	def paintCell(data: FishGridType, g: Graphics2D, x:Int,y:Int) : Unit = {
		if (oldData(x)(y) != data(x)(y)) {
			data(x)(y)_1 match {
				case WATER =>					
					g.setColor(AWTColor.black)		
				case FISH =>					
					g.setColor(AWTColor.blue)		
				case SHARK =>					
					g.setColor(AWTColor.red)		
			}
			g.fillRect(x*cellSize, y*cellSize, cellSize, cellSize)
			oldData(x)(y) = data(x)(y)
		}
	}		    
    
	override def paintComponent(g: Graphics2D) {
		val grid=dataChannel.read
		if (!disabled) grid.coordinates.foreach(c => paintCell(grid.data, g, c._1, c._2))
	}	
}