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

      val z = List.mytail(y)
      println(z)
    }
}
