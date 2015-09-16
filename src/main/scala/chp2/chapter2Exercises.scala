import scala.annotation.tailrec

object chp2 {

	/**
	* ex 2.1
	* Fn = Fn-1 + Fn-2
	**/
	def fib(n: Int): Int = {
		n match {
			case 0 => 0 
			case 1 => 1
			case _ => fib(n-1) + fib(n-2)
		}
	}

	def fibTailRecursive(n: Int): Int = {

		???
	}

	/**
	* ex 2.2
	* 
	**/
	@tailrec
	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		if (as.length < 2) {
			true //is it sorted if its empty or has 1 element?
		} else {
			ordered(as(0), as(1)) && isSorted(as.tail, ordered)
//			isSorted(as.tail, ordered) && ordered(as(0), as(1))
		}
	}

	/**
	 * ex 2.3
	 */
	def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
		???
	}

	/**
	 * ex 2.4
	 */
	def uncurry[A,B,C](f: A => B => C): (A, B) => C = ???


	/**
	 * ex 2.5
	 */
	def compose[A,B,C](f: B => C, g: A => B): A => C = ???




}