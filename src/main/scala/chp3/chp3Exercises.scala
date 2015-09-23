package chp3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
   * 3.2 Implement tail
   */
  def tail[A](input: List[A]): List[A] = input match {
    case Nil => Nil //if we get an empty list, return back an empty list
    case Cons(a, b) => b
  }

  /**
   * 3.3 Implement setHead. This function replaces the head element of a list with a different element
   */
  def setHead[A](input: List[A], replacement: A): List[A] = input match {
    case Nil => Nil
    case Cons(a, b) => Cons(replacement, b)
  }

  /**
   * 3.4 Remove the first n elements from a list
   */
  @tailrec
  def drop[A](l: List[A], n:Int): List[A] = {
    if (n == 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(a, b) => drop(b, n-1)
      }
    }
  }

  /**
   * 3.5 removes elements from the List prefix as long as they match a predicate
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(a, b) if f(a) => dropWhile(b, f)
      case _ => l
    }
  }

  /**
   * 3.6 returns a List consisting of all but the last element of a List
   *
   * Q: Why can't this be constant time?
   * A: because we have to traverse the entire list
   */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(a, b) => Cons(a, init(b))
    }
  }

  /**
   * 3.7
   * Q: Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
   * A: No. The implementation for foldRight doesn't have a check to see if value is 0, because it should be able to handle arbitrary values
   */

  /**
   * 3.8
   * What happens when you pass Nil and Cons to foldRight?
   */
  def exercise3_8() = {
    val result = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    //prints out Cons(1,Cons(2,Cons(3,Nil)))
    println(result)
  }

  /**
   * 3.9
   * Compute the length of a list using foldRight
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)( (_, b) => b + 1 )
  }

  /**
   * 3.10 write a tail recursive foldLeft
   */
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  /**
   * 3.11
   * Write Sum, Product, and Length with foldLeft
   */
  def sum2(l: List[Int]): Int = foldLeft(l, 0)( (a,b) => a+b)

  def product2(l: List[Double]): Double = foldLeft(l, 0.0)( (a,b) => a * b)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)( (a,_) => a+1)

  /**
   * 3.12
   * reverse a list using a fold
   *
   * This one took a bit of thinking to get
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])( (acc,b) => Cons(b,acc))
  //Given: reverse(List(1,2,3,4,5))
//  acc: Nil; b: 1
//  acc: Cons(1,Nil); b: 2
//  acc: Cons(2,Cons(1,Nil)); b: 3
//  acc: Cons(3,Cons(2,Cons(1,Nil))); b: 4
//  acc: Cons(4,Cons(3,Cons(2,Cons(1,Nil)))); b: 5


  /**
   * 3.13
   * Write a foldLeft in terms of FoldRight
   * Write a foldRight in terms of foldLeft
   */
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

  /**
   * 3.14 Implement append using foldLeft or foldRight
   *
   * Append will append the element to the end of a list
   */
  def append[A](l: List[A], element: A): List[A] = foldRight(l, Cons(element, Nil):List[A])( (a,acc) => Cons(a,acc))
  //Given append(List(1,2,3), 4)
//  acc: Cons(4,Nil) a: 3
//  acc: Cons(3,Cons(4,Nil)) a: 2
//  acc: Cons(2,Cons(3,Cons(4,Nil))) a: 1


  /**
   * 3.15
   * Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists.
   */
  def concat[A](l: List[List[A]]):List[A] = ???


  /**
   * 3.16
   * Transforms a list of integers by adding 1 to each element
   */
  def incrementListValue(l: List[Int]): List[Int] = {
    foldRight(l, List():List[Int])( (a,acc) => Cons(a+1, acc))
  }

  /**
   * 3.17
   * A function that transforms List[Double] into a String
   */
  def ListDoubleToString(l: List[Double]): List[String] = foldRight(l, List():List[String])( (a, acc) => Cons(a.toString, acc))


  /**
   * 3.18
   * Write Map
   */
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, List():List[B])( (a, acc) => Cons(f(a), acc))

  /**
   * 3.19
   * Filter to remove elements from a list unless they satisfy a given predicate
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List():List[A])( (a, acc) =>  if ( f(a) ) Cons(a, acc) else acc  )

  /**
   * Using 3.19 to remove all odd numbers
   */
  def removeOdds(l: List[Int]): List[Int] = List.filter(l)( a => if ( (a % 2) == 0) true else false  )


  /**
   * 3.20
   * write flatMap
   * eg> For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
   */
  //TODO: finish 3.15. This does not work atm
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List():List[B])( (a, acc) =>  f(a) )



}

object Tests {
  import chp3.List._

  //test for 3.10
  def testFoldLeft() = {
    val numbers: List[Int] = List(1,2,3,4)

    val result = foldLeft(numbers, 0)( (a,b) => a + b)

    println(result)
    assert(result == 10)
  }

  //test for 3.19


}