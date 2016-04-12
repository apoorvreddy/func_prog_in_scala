object UtilFunctions {

    def compose[A, B, C](f: B => C, g: A => B): A => C =
        (a: A) => f(g(a))

    def main(args: Array[String]): Unit = {
        
        val f = (x: Int) => x + 2
        val g = (x: Int) => x * 10
        
        val x = compose(f, g)
        val y = compose(g, f)
        assert(x(2) == 22)
        assert(y(2) == 40)
        println("Tests passed")
    }

}
