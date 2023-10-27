package life

import introprog.PixelWindow
import introprog.PixelWindow.Event
import java.awt.Color as JColor
import scala.compiletime.ops.int

object LifeWindow:
    val EventMaxWait = 1 // milliseconds
    var NextGenerationDelay = 200 // milliseconds
    // lägg till fler användbara konstanter här tex färger etc.
    object Colors:
        val cellLiving = new JColor( 242, 128, 161 )
        val space      = new JColor( 0, 0, 0 )
        val grid       = new JColor( 255, 255, 255 )

    val windowSize = (80, 60)

class LifeWindow(rows: Int, cols: Int):
    import LifeWindow.* // importera namn från kompanjon

    var life = Life.empty(rows, cols)
    val window: PixelWindow = PixelWindow(windowSize._1, windowSize._2)
    var quit = false
    var play = false

    def drawGrid(): Unit = 
        ???

    // def drawCell(pos: (Int, Int), color: JColor): Unit =
    //     val x = pos._1 * blockSize
    //     val y = pos._2 * blockSize
    //     pixelWindow.fill(x, y, blockSize, blockSize, color)

    def update(newLife: Life): Unit =
        val oldLife = life
        life = newLife
        life.cells.foreachIndex{ ??? }

    def handleKey(key: String): Unit = ???

    def handleClick(pos: (Int, Int)): Unit = ???

    def loopUntilQuit(): Unit = while !quit do
        val t0 = System.currentTimeMillis
        if play then update(life.evolved())
        window.awaitEvent(EventMaxWait)
        while window.lastEventType != Event.Undefined do
        window.lastEventType match
            case Event.KeyPressed  =>  handleKey(window.lastKey)
            case Event.MousePressed => handleClick(window.lastMousePos)
            case Event.WindowClosed => quit = true
            case _ =>
        window.awaitEvent(EventMaxWait)
        val elapsed = System.currentTimeMillis - t0
        Thread.sleep((NextGenerationDelay - elapsed) max 0)

    def start(): Unit = { drawGrid(); loopUntilQuit() }