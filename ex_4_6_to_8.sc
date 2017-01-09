object either {

  //ex4_6
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(x) => Right(f(x))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => f(x)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case _ => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      flatMap(x => b.map(y => f(x, y)))
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }        //> Try: [A](a: => A)either.Either[Exception,A]

  //ex4_7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
                                                  //> sequence: [E, A](es: List[either.Either[E,A]])either.Either[E,List[A]]

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
  	as.foldRight(Right(Nil): Either[E, List[B]])(f(_).map2(_)(_ :: _))
  }                                               //> traverse: [E, A, B](as: List[A])(f: A => either.Either[E,B])either.Either[E
                                                  //| ,List[B]]
                                                  
  //ex4_8: represent as List of Errors ?
                                                  
	val as = List(Right(1), Left("error1"), Right(2), Left("error2"))
                                                  //> as  : List[Product with Serializable with either.Either[String,Int]] = List
                                                  //| (Right(1), Left(error1), Right(2), Left(error2))
	val bs = List(Right(12), Right(2))        //> bs  : List[either.Right[Int]] = List(Right(12), Right(2))

	sequence(as)                              //> res0: either.Either[String,List[Int]] = Left(error1)
	sequence(bs)                              //> res1: either.Either[Nothing,List[Int]] = Right(List(12, 2))
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}
