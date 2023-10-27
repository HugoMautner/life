package life

import math.random

case class Life(cells: Matrix[Boolean]):

    /** Ger true om cellen på plats (row, col) är vid liv annars false.
    * Ger false om indexeringen är utanför universums gränser.
    */
    def apply(row: Int, col: Int): Boolean = 
        // check if out of universe bounds
        if row < 0 || row > LifeWindow.windowSize._2 || col < 0 || col > LifeWindow.windowSize._1 then false
        
        cells(row)(col)

    /** Sätter status på cellen på plats (row, col) till value. */
    def updated(row: Int, col: Int, value: Boolean): Life = 
        cells(row)(col) = value

    /** Växlar status på cellen på plats (row, col). */
    def toggled(row: Int, col: Int): Life = 
        cells(row)(col) = !cells(row)(col)

    /** Räknar antalet levande grannar till cellen i (row, col). */
    def nbrOfNeighbours(row: Int, col: Int): Int = 
        var nbrLivingNeighbours: Int = 0
        for r <- row -1 to row + 1 do 
            for c <- col -1 to col + 1 do
                if cells(r)(c) then nbrLivingNeighbours += 1
        nbrLivingNeighbours - 1

    /** Skapar en ny Life-instans med nästa generation av universum.
        * Detta sker genom att applicera funktionen \code{rule} på cellerna.
        */
    def evolved(rule: (Int, Int, Life) => Boolean = Life.defaultRule):Life =
        var nextGeneration = Life.empty(cells.dim)
        cells.foreachIndex( (r,c) =>
        Life.defaultRule(rule)
        )
        nextGeneration

    /** Radseparerad text där 0 är levande cell och - är död cell. */
    override def toString = 
        ???

object Life:
    /** Skapar ett universum med döda celler. */
    def empty(dim: (Int, Int)): Life = 
        Life(Matrix.fill(dim)(false))

    /** Skapar ett unviversum med slumpmässigt liv. */
    def random(dim: (Int, Int)): Life = 
        import scala.util.Random.nextBoolean
        val newLife = Life(Matrix.fill(dim)(false))


    /** Implementerar reglerna enligt Conways Game of Life. */
    def defaultRule(row: Int, col: Int, current: Life): Boolean = 
        if current.cells(row)(col) then 
            if current.nbrOfNeighbours(row, col) > 3 || 
                current.nbrOfNeighbours(row, col) < 2 then current.toggled(row, col)
        else 
            if current.nbrOfNeighbours(row)(col) == 3 then current.toggled(row, col)

        current.cells(row)(col)
