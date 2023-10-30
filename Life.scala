package life

case class Life(cells: Matrix[Boolean]):

    /** Ger true om cellen på plats (row, col) är vid liv annars false.
    * Ger false om indexeringen är utanför universums gränser.
    */
    def apply(row: Int, col: Int): Boolean = 
        // check if out of universe bounds
        if row < 0 || row > cells.dim._1 - 1 || col < 0 || col > cells.dim._2 - 1 
            then false
        else 
            cells(row, col)

    /** Sätter status på cellen på plats (row, col) till value. */
    def updated(row: Int, col: Int, value: Boolean): Life = 
        Life(cells.updated(row, col)(value))

    /** Växlar status på cellen på plats (row, col). */
    def toggled(row: Int, col: Int): Life = 
        val oldValue = cells(row, col)
        Life(cells.updated(row, col)(!oldValue))

    /** Räknar antalet levande grannar till cellen i (row, col) enligt Moore's Neighbourhood. */
    def mooreNeighbours(row: Int, col: Int): Int =
        val rowStart = Math.max(0, row - 1)
        val rowEnd = Math.min(cells.dim._1 - 1, row + 1)
        val colStart = Math.max(0, col - 1)
        val colEnd = Math.min(cells.dim._2 - 1, col + 1)

        var neighbours: Int = 0

        for {
            r <- rowStart to rowEnd
            c <- colStart to colEnd
            if r != row || c != col
            if cells(r, c)
        } neighbours += 1
        neighbours
    
    /** Räknar antalet levande grannar till cellen i (row, col) enligt von Neumann's Neighbourhood. */
    def vonNeumannNeighbours(row: Int, col: Int): Int =
        val neighbors = List(
            (row - 1, col), // North
            (row + 1, col), // South
            (row, col - 1), // West
            (row, col + 1)  // East
        )
        neighbors.count{
            case (r, c) => 
                r >= 0 && r < cells.dim._1 && c >= 0 && c < cells.dim._2 && cells(r, c)
        }

    /** Skapar en ny Life-instans med nästa generation av universum.
        * Detta sker genom att applicera funktionen \code{rule} på cellerna.
        */
    def evolved(rule: (Int, Int, Life) => Boolean = Life.castles): Life =
        var nextGeneration = Life.empty(cells.dim)
        cells.foreachIndex( (row, col) =>
            val nextState = rule(row, col, this)
            nextGeneration = nextGeneration.updated(row, col, nextState)
        )
        nextGeneration

    /** Radseparerad text där 0 är levande cell och - är död cell. */
    override def toString: String = 
        var res: String = ""
        cells.foreachIndex( (row, col) => 
            if cells(row, col) then res += '0'
            else res += '-'
            if col < cells.dim._2 - 1 then res += ' '
            if col == cells.dim._2 - 1 && row < cells.dim._1 - 1 then res += '\n'
        )
        res

object Life:
    /** Skapar ett universum med döda celler. */
    def empty(dim: (Int, Int)): Life = 
        Life(Matrix.fill(dim)(false))

    /** Skapar ett unviversum med slumpmässigt liv. */
    def random(dim: (Int, Int)): Life = 
        import scala.util.Random.nextBoolean
        var life = empty(dim)
        life.cells.foreachIndex( (r, c) =>
            // toggle random cells
            if nextBoolean() then
                life = life.toggled(r, c)
        )
        life

    /** Implementerar reglerna enligt Conways Game of Life. */
    def defaultRule(row: Int, col: Int, current: Life): Boolean = 
        val nbrNeighbors = current.mooreNeighbours(row, col)
        var newLife: Life = current
        if current(row, col) then // Lives
            if nbrNeighbors > 3 || 
                nbrNeighbors < 2 then newLife = current.toggled(row, col) // Kill
        else // Is dead
            if nbrNeighbors == 3 then newLife = current.toggled(row, col) // Birth

        newLife(row, col)

    /** Invertamaze */
    def invertamaze(row: Int, col: Int, current: Life): Boolean = 
        val nbrNeighbors = current.mooreNeighbours(row, col)
        var newLife: Life = current
        val okNeighbors = Vector(0, 1, 2, 4)
        val birthNeighbors = Vector(0, 2, 8)
        
        if current(row, col) then // Lives
            if !okNeighbors.contains(nbrNeighbors) then newLife = current.updated(row, col, false) // Kill
        else // Is dead
            if birthNeighbors.contains(nbrNeighbors) then newLife = current.updated(row, col, true) // Birth

        newLife(row, col)

    /** Castles */
    def castles(row: Int, col: Int, current: Life): Boolean = 
        val nbrNeighbors = current.mooreNeighbours(row, col)
        var newLife: Life = current
        val okNeighbors = Vector(1, 3, 5, 6, 7, 8)
        val birthNeighbors = Vector(3, 6, 7, 8)
        
        if current(row, col) then // Lives
            if !okNeighbors.contains(nbrNeighbors) then newLife = current.updated(row, col, false) // Kill
        else // Is dead
            if birthNeighbors.contains(nbrNeighbors) then newLife = current.updated(row, col, true) // Birth

        newLife(row, col)

    
