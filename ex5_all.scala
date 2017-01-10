package fp

package object laziness {
  def if_thunk[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  if_thunk(1 < 22,
    () => println("a"),
    () => println("b"))

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  if2(false, sys.error("fail"), 3)

  //lazy List
  sealed trait Stream[+A] {
    def headOption0: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    //ex5_1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    //ex5_2
    def take0(n: Int): Stream[A] = {
      if (n == 0) Stream.empty else this match {
        case Empty => this
        case Cons(h, t) => Stream.cons(h(), t().take0(n - 1))
      }
    }

    def drop(n: Int): Stream[A] = {
      if (n == 0) this else this match {
        case Empty => this
        case Cons(h, t) => t().drop(n - 1)
      }
    }

    //ex5_3
    def takeWhile0(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile0(p))
      case _ => Stream.empty
    }

    def exists0(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    
    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    //ex5_4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
    

    //ex5_5
    def takeWhile1(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) Stream.cons(a, b) else Stream.empty[A])
    
    //ex5_6
    def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))
    
    //ex5_7 
    def map0[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
    
    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)
    
    def append[B>:A](x: => Stream[B]): Stream[B] = foldRight(x)((a, b) => Stream.cons(a, b))
    
    //ex5_13
    def map[B](f: A => B): Stream[B] = unfold(this)(x => x match {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
      })
      
    def take(n: Int): Stream[A] = unfold((this, n))(x => x._1 match {
      case Cons(h, t) if x._2 > 0 => Some(h(), (t(), x._2 - 1))
      case _ => None
    })
    
    def takeWhile(p: A => Boolean): Stream[A] = unfold(this)(x => x match {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    })
    
    def zipWith[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b))(_ match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h, t), Cons(hb, tb)) => Some(f(h(), hb()), (t(), tb()))
    })
    
    def zipAll[B](b: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, b))(_ match {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    })

    //ex5_14
    def startsWith[A](s: Stream[A]): Boolean = {
      zipAll(s).forAll(_ match {
        case (None, _) => false
        case (_, None) => true
        case (Some(a), Some(b)) => a == b
      })
    }
     
    def tails: Stream[Stream[A]] = unfold(this)(x => x match {
      case Empty => None
      case Cons(h, t) => Some(x, t())
    })
    
    //ex5_15
    def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_ startsWith s)
    
    //ex5_16
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
      foldRight(Stream.cons(z, Empty))((a, s) => s match {
        case Cons(h, _) => Stream.cons(f(a, h()), s)
        case _ => Empty
      })
    }
    
  }
  

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
  
  //ex5_8
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }
  
  //ex5_9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }
  
  //ex5_10
  def fibs(z: (Int, Int)): Stream[Int] = {
    Stream.cons(z._1, fibs((z._2, z._1 + z._2)))
  }
  
  //ex5_11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z).map(x => Stream.cons(x._1, unfold(x._2)(f))).getOrElse(Stream.empty)
  }
  
  
  //ex5_12
  def ones1: Stream[Int] = unfold(1)(n => Some((n, n)))
  def constant1[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))
  def from1(n: Int): Stream[Int] = unfold(n)(n => Some((n, n+1)))
  def fibs1: Stream[Int] = unfold((1,1))(x => Some((x._1, (x._2, x._1 + x._2))))
}
