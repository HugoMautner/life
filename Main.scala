package life

object Main:
    val help = """
        Welcome to GAME OF LIFE!

        Click on cell to toggle.
        Press ENTER for next generation.
        Press SPACE to toggle play/stop.
        Press Up-arrow to increase simulation speed.
        Press Down-arrow to decrease simulation speed.
        Press R to create random life.
        Press BACKSPACE to clear life.
        Press Q to exit.
    """

    val dim = (95, 150)

    def main(args: Array[String]): Unit =
        println(help)
        val lifeWindow = new LifeWindow(dim._1, dim._2)
        lifeWindow.start()
        introprog.PixelWindow.exit()