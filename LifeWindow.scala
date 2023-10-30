package life

import introprog.PixelWindow
import introprog.PixelWindow.Event
import java.awt.Color as JColor
import life.LifeWindow.Colors.grid

object LifeWindow:
    val EventMaxWait = 1 // milliseconds
    var NextGenerationDelay = 100 // milliseconds
    // lägg till fler användbara konstanter här tex färger etc.
    object Colors:
        val cellLiving = new JColor( 242, 128, 161 )
        val space      = new JColor( 0, 0, 0 )
        val grid       = new JColor( 232, 232, 232, 50 )

    val cellSize = 10 // pixels across
    val gridBorderThic = 1
    val title = "The Game of Life"

class LifeWindow(rows: Int, cols: Int):
    import LifeWindow.* // importera namn från kompanjon

    var life = Life.empty(rows, cols)
    val window: PixelWindow = PixelWindow(cols * cellSize, rows * cellSize, title)
    var quit = false
    var play = false
    // var inputFlag = false

    def drawGrid(): Unit = 
        window.fill(0, 0, cols * cellSize, rows * cellSize, Colors.grid)
        life.cells.foreachIndex( (rows, cols) => 
            drawCell(rows, cols, Colors.space)
        )

    def drawCell(row: Int, col: Int, color: JColor): Unit =
        window.fill(col * cellSize + gridBorderThic, row * cellSize + gridBorderThic, 
        cellSize - gridBorderThic * 2, cellSize - gridBorderThic * 2, color)

    def update(newLife: Life): Unit =
        val oldLife = life
        life = newLife
        life.cells.foreachIndex( (r, c) =>
            if oldLife(r, c) != life(r, c) then
                if life(r, c) then drawCell(r, c, Colors.cellLiving)
                else drawCell(r, c, Colors.space)
        )

    def handleKey(key: String): Unit = 
        key match
            case " " => play = !play
            case "Up" => LifeWindow.NextGenerationDelay -= 10
            case "Down" => LifeWindow.NextGenerationDelay += 10
            case "Enter" => update(life.evolved())
            case "r" => update(Life.random(rows, cols))
            case "Backspace" => update(Life.empty(rows, cols))
            case "q" => quit = true
            case _ => 

    def handleClick(pos: (Int, Int)): Unit = 
        val cellPos = (pos._2 / cellSize, pos._1 / cellSize)
        update(life.toggled(cellPos._1, cellPos._2))

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