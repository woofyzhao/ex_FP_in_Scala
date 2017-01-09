object option {

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
                                                  //> lift: [A, B](f: A => B)option.Option[A] => option.Option[B]
  //ex4_2
  def variance(xs: Seq[Double]): Option[Double] = {
   mean(xs).flatMap(m => mean(xs.map(a => math.pow((a - m), 2))))
  }                                               //> variance: (xs: Seq[Double])option.Option[Double]
  
  //ex4_3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }                                               //> map2: [A, B, C](a: option.Option[A], b: option.Option[B])(f: (A, B) => C)op
                                                  //| tion.Option[C]

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }                                               //> map2_1: [A, B, C](a: option.Option[A], b: option.Option[B])(f: (A, B) => C)
                                                  //| option.Option[C]

  //ex4_4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])(map2(_, _)(_ :: _))
  }                                               //> sequence: [A](a: List[option.Option[A]])option.Option[List[A]]

  //ex4_5  using map
  def traverse0[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f))
                                                  //> traverse0: [A, B](a: List[A])(f: A => option.Option[B])option.Option[List[B
                                                  //| ]]

  //ex4_5 one pass using fold right
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)(_ :: _))
  }                                               //> traverse: [A, B](a: List[A])(f: A => option.Option[B])option.Option[List[B]
                                                  //| ]
  //sequence using traverse
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
                                                  //> sequence1: [A](a: List[option.Option[A]])option.Option[List[A]]

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }           //> Try: [A](a: => A)option.Option[A]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)                 //> mean: (xs: Seq[Double])option.Option[Double]




  val s = List(Some(1), Some(321), Some(444), Some("a"))
                                                  //> s  : List[option.Some[Any]] = List(Some(1), Some(321), Some(444), Some(a))
  sequence(s)                                     //> res0: option.Option[List[Any]] = Some(List(1, 321, 444, a))
  sequence1(s)                                    //> res1: option.Option[List[Any]] = Some(List(1, 321, 444, a))
  traverse0(List("234", "99", "xb"))(i => Try(i.toInt))
                                                  //> res2: option.Option[List[Int]] = None
  traverse(List("234", "99", "0", "99"))(i => Try(i.toInt))
                                                  //> res3: option.Option[List[Int]] = Some(List(234, 99, 0, 99))
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}
