object Work extends App {


// +A makes List[Dog] a subtype of List[Animal] if Dog is a subtype of Animal
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)        
    }


    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
        }

    // ex3.2 tail for removing first element of a list
    def mytail[A](l: List[A]): List[A] = l match{
        case Nil => Nil
        case Cons(h, t) => t
        }

    // ex3.3 setHead. replaces first element with different value
    def setHead[A](l: List[A], h: A): List[A] = l match {
        case Nil => Cons(h, Nil)
        case Cons(h_old, t) => Cons(h, t)
        }

    // ex3.4 drop which removes first n elements from a list
    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
                case Nil => Nil
                case Cons(h, t) => 
                    if (n == 0) t
                    else drop(t, n-1)
        }


    // ex3.5 dropWhile remove elements which match a prefix
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
                case Nil => Nil
                case Cons(h, t) =>
                    if (f(h))
                        dropWhile(t, f)
                    else
                        Cons(h, dropWhile(t, f))
            }
    }
    
    override def main(args: Array[String]): Unit = {
    val y = List(1, 2, 3, 4)    
    val x = y match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
        }
    
    println(x)

      var z = List.mytail(y)
      println(z)

      z = List.setHead(z, 5)
      println(z)

      z = List.drop(y, 2)
      println(z)

      
      z = List(1, 2, 3, 4, 5, 2, 2, 1, 3, 3)
      z = List.dropWhile(z, (x: Int) => x == 2)
      println(z)

    }
}
