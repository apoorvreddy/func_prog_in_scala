object UtilFunction {

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        a => (b => f(a, b))


    def main(args: Array[String]): Unit = {
        val f = (x: Int, y: Int) => x + y
        val g = curry(f)
        assert(g(10)(100) == 110)
        println("Test passed")
    }
}
