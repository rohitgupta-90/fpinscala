package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil =>  sys.error("tail of an empty list")
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("cannot replace null head")
      case Cons(_, t) => Cons(h,t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1) l
    else{
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n -1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs,f(z, x))(f)
    }
  }

  def sumLeft(l : List[Int]) : Int = {
    foldLeft(l, 0)( _ + _ )
  }

  def productLeft(l : List[Double]) : Double = {
    foldLeft(l, 1.0)( _ * _ )
  }

  def lenghthLeft[A](l : List[A]) : Int = {
    foldLeft(l, 1)((acc, _) => acc + 1 )
  }

  def reverse[A](l : List[A]) : List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))



  def foldRightViaFoldLeft[A,B](l : List[A], z : B)(f : (A, B) => B) : B = {
    foldLeft(reverse(l),z)((b,a)=> f(a,b))
  }


//  def foldRightViaFoldLeft_1[A,B](l : List[A], z : B)(f : (A, B) => B) : B = ???
//  def foldRightViaFoldLeft[A,B](l : List[A], z : B)(f : (B, A) => B) : B = ???


  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((a, b) => Cons(a, b))
  }

  def appendViaFoldLeft[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l1, l2)((a, b) => Cons(b, a))
  }


  def concat[A](l : List[List[A]]) : List[A] = {
    foldLeft(l , List[A]())(appendViaFoldLeft)
  }


  def add(l : List[Int]) : List[Int] = {
    foldRight(l, List[Int]())((a, acc) => Cons(a+1, acc))
  }


  def doubleToString(l : List[Double]) : List[String] =
    foldRight(l, List[String]())((a, acc) => Cons(a.toString , acc) )


  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a,acc) => Cons(f(a), acc))


  def filter[A](l : List[A] )(f: A => Boolean) : List[A] = {
    foldRight(l, List[A]())((a,acc) => if(f(a)) Cons(a, acc) else acc)
  }

  def removeOdds(l : List[Int]) : List[Int] = filter(l)(x => x%2 == 0)

  def flatMap[A, B](l : List[A])(f : A => List[B]) : List[B] =
    foldRight(l, List[B]())((a, acc) => append(acc, f(a)))

  def flatMapWithConcat[A, B](l : List[A])(f : A => List[B]) : List[B] = {
    concat(map(l)(f))
  }


//  def filterWithFlatMap[A](l : List[A], f : A => Boolean) : List[A] = {
//    flatMap(l)(a => if(f(a)))
//  }

  def addPairs(l1 : List[Int], l2 : List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2 , addPairs(t1,t2))
    }
  }

  def zipWith[A,B,C](l1: List[A], l2 : List[B])(f:(A,B) => C) : List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }

  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def subSeq(sup: List[A], sub : List[A]) : Boolean = {
      (sup, sub) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case(Cons(h1,t1), Cons(h2,t2)) => if(h1 == h2) subSeq(t1, t2) else false
      }
    }

    (sup,sub) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) => subSeq(sup,sub) || hasSubsequence(t1,sub)
    }
  }

}
