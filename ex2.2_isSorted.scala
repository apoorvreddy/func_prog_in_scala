object utilFunctions {

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        
        @annotation.tailrec
        def loop(n: Int, currentOrdered: Boolean): Boolean = {
            if (n+1 >= as.length) currentOrdered
            else if (currentOrdered == false) false           
            else loop(n+1, ordered(as(n), as(n+1)) & currentOrdered) 
        }

        if (as.length >= 1) loop(0, true)
        else false
    }

    def main(args: Array[String]): Unit = {
        
        assert(isSorted(Array(), (x: Int, y:Int) => x < y) == false)
        assert(isSorted(Array(7, 8, 9, 13, 20, 24, 30, 35, 36, 27, 39, 40, 41, 42, 43, 53), (x: Int, y:Int) => x <= y) == false)
        assert(isSorted(Array(9.2, 7.6, 5.4), (x: Double, y:Double) => x >= y) == true)
        println("All tests passed")
    }
}
