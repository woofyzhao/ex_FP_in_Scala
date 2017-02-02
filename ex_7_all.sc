import java.util.concurrent._

object parallel {

  type Par[A] = ExecutorService => Future[A]

  /**
   * class ExecutorService {
   *   def submit[A](a: Callable[A]): Future[A]
   * }
   *
   * trait Callable[A] { def call: A }
   *
   * trait Future[A] {
   *   def get: A
   *   def get(timeout: Long, unit: TimeUnit): A
   *   def cancel(evenIfRunning: Boolean): Boolean
   *   def isDone: Boolean
   *   def isCancelled: Boolean
   * }
   */

  object Par {

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
      es.submit(new Callable[A] {
        def call = a(es).get
      })
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      (es: ExecutorService) =>
        {
          val fa = a(es)
          val fb = b(es)
          UnitFuture(f(fa.get, fb.get))
        }
    }

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
      map2(a, map2(b, c)((x, y) => (x, y)))((x, y) => f(x, y._1, y._2))
    }

    //ex7.3
    /*
		 * Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
		 * We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
		 */
    case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {

      @volatile var cache: Option[C] = None

      def isDone = cache.isDefined
      def isCancelled = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      def get = compute(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime;
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }

    def map2_with_timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      (es: ExecutorService) =>
        {
          val (fa, fb) = (a(es), b(es))
          Map2Future(fa, fb, f)
        }
    }

    //ex7.4
    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    def sortPar0(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    def mapf[A, B](f: A => B): Par[A] => Par[B] = pa => {
      map(pa)(f)
    }

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

    //ex7.5
    //not what they want..
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = (es: ExecutorService) => {
      UnitFuture(ps.map(p => p(es)).map(_.get))
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = map(parMap(as)(a => if (f(a)) List(a) else Nil))(_.flatten)

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    //ex7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = (es: ExecutorService) => {
      run(es)(choices(run(es)(n).get))
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

    //ex7.13
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = (es: ExecutorService) => {
      choices(pa(es).get)(es)
    }

    def choiceN2[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(pn)(n => choices(n))

    //ex7.14

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = ???

    //ex7.15

    def join[A](a: Par[Par[A]]): Par[A] =
      es => run(es)(run(es)(a).get())

    def join0[A](a: Par[Par[A]]): Par[A] = {
      flatMap(a)(p => p)
    }

    def flatMap0[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join0(map(a)(f))

    def map2_0[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      flatMap0(pa)(a => flatMap0(pb)(b => unit(f(a, b))))

  }

  object Par2 {
    ???
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }                                             //> sum: (ints: IndexedSeq[Int])parallel.Par[Int]

  val a = Par.lazyUnit(42 + 1)                    //> a  : parallel.Par[Int] = <function1>
  val b = Par.lazyUnit(43 + 1)                    //> b  : parallel.Par[Int] = <function1>
  val S = Executors.newFixedThreadPool(3)         //> S  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPool
                                                  //| Executor@5b464ce8[Running, pool size = 0, active threads = 0, queued tasks 
                                                  //| = 0, completed tasks = 0]
  //println(Par.equal(S)(a, Par.fork(a)))
  //Par.run(S)(Par.fork(Par.map2(a, b)(_ + _))).get
  //Par.run(S)(Par.fork(Par.fork(a))).get

  //println("Welcome to the Scala worksheet")
}
