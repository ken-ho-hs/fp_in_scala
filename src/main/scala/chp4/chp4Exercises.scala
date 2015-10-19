package chp4

//sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(value) => Some(f(value))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(value) => f(value)
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(value) => value
      case None => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(_) => this
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(value) => if (f(value)) this else None
      case _ => None
    }
  }


  def variance(xs: Seq[Double]): Option[Double] = ???

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}



case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    ???
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = ???
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}



object Chp4 {

  def mapTest = {
    val someInt: Option[Int] = Some(123)

    val result = someInt.map( value => value + 1 )

    //expect this to be 124
    println(result)
  }

  /**
   * Exercise 4.3
   * Write a generic function map2 that combines two Option values using a binary function.
   * If either Option value is None, then the return value is too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a,b) match {
      case (Some(aValue), Some(bValue)) => Some(f(aValue,bValue))
      case _ => None
    }
  }

  /**
   * Exercise 4.4
   * Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original list.
   * If the original list contains None even once, the result of the function should be None;
   * otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    a match {
      case x :: xs =>  x.flatMap { //if x is None, this flatMap will return None

          head => sequence(xs).map( res => head :: res  )
      }
//      case x :: xs =>  x.map( xx => sequence(xs).flatMap( xxs =>  Some(xx :: xxs)  ) )
      case Nil => None
    }


  }


  /**
   * Exercise 4.5
   */

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???


  /**
   * Exercise 4.6
   */


}