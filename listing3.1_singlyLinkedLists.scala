object Work extends App {


// +A makes List[Dog] a subtype of List[Animal] if Dog is a subtype of Animal
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
def Cons2[A](tail: List[A], head: A): List[A] = {
    Cons(head, tail)
}
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
    
    // ex3.6 init that returns a List consisting of all but the last element
    def init[A](l: List[A]): List[A] = l match {
            case Nil => Nil
            case Cons(h, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))            
        }

    def sum2(ns: List[Int]) = 
        foldRight(ns, 0)(_ + _)

    def product2(ns: List[Int]) =
        foldRight(ns, 1.0)(_ * _)

    // ex3.9 length using foldRight
    def length[A](as: List[A]): Int =
        foldRight(as, 0)((x: A, y: Int) => 1 + y) 
    
    // ex3.10 foldLeft
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
                case Nil => z
                case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
        }


    // ex3.11 sum using foldLeft
    def sum3(ns: List[Int]) =
        foldLeft(ns, 0)(_ + _)

    // ex3.11 product using foldLeft
    def product3(ns: List[Int]) =
        foldLeft(ns, 0)(_ * _)
    
    // ex3.11 length using foldLeft
    def length2[A](ns: List[A]) =
        foldLeft(ns, 0)((x: Int, y: A) => 1 + x)
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

      z = List(1, 2, 3, 4, 5, 2, 2, 1, 3, 3, 5)
      z = List.init(z)
      println(z)


      z = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))
      println(z)

      println(List.length(z))
      z = List.foldLeft(List(1,2,3), Nil:List[Int])(Cons2(_, _))
      println(z)

      println(List.length2(z))
    }
}
