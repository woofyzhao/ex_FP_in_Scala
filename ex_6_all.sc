object random {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }                                               //> randomPair: (rng: random.RNG)((Int, Int), random.RNG)

  //ex6_1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    if (n >= 0) (n, r) else nonNegativeInt(r)
  }                                               //> nonNegativeInt: (rng: random.RNG)(Int, random.RNG)

  //ex6_2
  def double1(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, r)
  }                                               //> double1: (rng: random.RNG)(Double, random.RNG)

  //ex6_3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i, d), r1)
  }                                               //> intDouble: (rng: random.RNG)((Int, Double), random.RNG)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }                                               //> doubleInt: (rng: random.RNG)((Double, Int), random.RNG)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }                                               //> double3: (rng: random.RNG)((Double, Double, Double), random.RNG)

  //ex6_4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng) else {
      val (i, r) = rng.nextInt
      val (list, rn) = ints(count - 1)(r)
      (i :: list, rn)
    }
  }                                               //> ints: (count: Int)(rng: random.RNG)(List[Int], random.RNG)

  type Rand[+A] = RNG => (A, RNG)
  def int: Rand[Int] = _.nextInt                  //> int: => random.Rand[Int]
  def unit[A](a: A): Rand[A] = rng => (a, rng)    //> unit: [A](a: A)random.Rand[A]
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }                                             //> map: [A, B](s: random.Rand[A])(f: A => B)random.Rand[B]
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
                                                  //> nonNegativeEven: => random.Rand[Int]

  //ex6_5
  def double(rng: RNG): (Double, RNG) = map(nonNegativeInt)(x => x.toDouble / Int.MaxValue)(rng)
                                                  //> double: (rng: random.RNG)(Double, random.RNG)

  //ex6_6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    r =>
      {
        val (a, r1) = ra(r)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }
  }                                               //> map2: [A, B, C](ra: random.Rand[A], rb: random.Rand[B])(f: (A, B) => C)rand
                                                  //| om.Rand[C]

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
                                                  //> both: [A, B](ra: random.Rand[A], rb: random.Rand[B])random.Rand[(A, B)]
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
                                                  //> randIntDouble  : random.Rand[(Int, Double)] = <function1>
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
                                                  //> randDoubleInt  : random.Rand[(Double, Int)] = <function1>

  //ex6_7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    r =>
      {
        fs match {
          case x :: xs => {
            val (a, r1) = x(r)
            val (list, rn) = sequence(xs)(r1)
            (a :: list, rn)
          }
          case _ => (Nil, r)
        }
      }
  }                                               //> sequence: [A](fs: List[random.Rand[A]])random.Rand[List[A]]

  def nonNegativeLessThan0(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan0(n)(rng2)
  }                                               //> nonNegativeLessThan0: (n: Int)random.Rand[Int]

  //ex6_8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    r =>
      {
        val (a, r1) = f(r)
        g(a)(r1)
      }
  }                                               //> flatMap: [A, B](f: random.Rand[A])(g: A => random.Rand[B])random.Rand[B]

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(x => {
    val mod = x % n
    if (x + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })                                              //> nonNegativeLessThan: (n: Int)random.Rand[Int]

  //ex6_9
  def map_1[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
                                                  //> map_1: [A, B](s: random.Rand[A])(f: A => B)random.Rand[B]
  def map2_1[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))
                                                  //> map2_1: [A, B, C](ra: random.Rand[A], rb: random.Rand[B])(f: (A, B) => C)ra
                                                  //| ndom.Rand[C]

  //ex6_10
  case class State[S, +A](run: S => (A, S)) {
    //def unit[B>:A](a:B): State[S,B] = State(s => (a, s))

    def apply(s: S): (A, S) = run(s)

    def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      g(a)(s1)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State1.unit(f(a)))

    def map2[B, C](g: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => g.map(b => f(a, b)))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  object State1 {
    def unit[A, S](a: A): State[S, A] = State(s => (a, s))

    def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
      fs match {
        case Nil => (Nil, s)
        case x :: xs => {
          val (a, s1) = x(s)
          val (list, sn) = sequence(xs)(s1)
          (a :: list, sn)
        }
      }
    })
  }

  type RND[+A] = State[RNG, A]
  val int1: RND[Int] = State(s => s.nextInt)      //> int1  : random.RND[Int] = State(<function1>)
  def ints1(count: Int): RND[List[Int]] = State1.sequence(List.fill(count)(int1))
                                                  //> ints1: (count: Int)random.RND[List[Int]]

  val ns: RND[List[Int]] =
    int1.flatMap(x =>
      int1.flatMap(y =>
        ints1(x).map(xs =>
          xs.map(_ % y))))                        //> ns  : random.RND[List[Int]] = State(<function1>)

  val ns1: RND[List[Int]] = for {
    x <- int1
    y <- int1
    xs <- ints1(x)
  } yield xs.map(_ % y)                           //> ns1  : random.RND[List[Int]] = State(<function1>)

  //ex6_11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(s => {
    s match {
      case Machine(locked, candies, coins) => {
        if (candies == 0) ((candies, coins), s) else
          inputs match {
            case Nil => ((candies, coins), s)
            case Coin :: xs => {
              if (locked) simulateMachine(xs)(Machine(false, candies, coins + 1))
              else simulateMachine(xs)(s)
            }
            case Turn :: xs => {
              if (!locked) simulateMachine(xs)(Machine(true, candies - 1, coins))
              else simulateMachine(xs)(s)
            }
          }
      }
    }
  })                                              //> simulateMachine: (inputs: List[random.Input])random.State[random.Machine,(I
                                                  //| nt, Int)]
  
  

  val r = SimpleRNG(10990)                        //> r  : random.SimpleRNG = SimpleRNG(10990)
  r.nextInt                                       //> res0: (Int, random.RNG) = (-66576884,SimpleRNG(277111794047841))
  randomPair(r)                                   //> res1: ((Int, Int), random.RNG) = ((-66576884,1447036716),SimpleRNG(94832998
                                                  //| 272600))
  val (n, r1) = nonNegativeInt(r)                 //> n  : Int = 1447036716
                                                  //| r1  : random.RNG = SimpleRNG(94832998272600)
  double(r)                                       //> res2: (Double, random.RNG) = (0.6738289802678996,SimpleRNG(94832998272600))
                                                  //| 
  intDouble(r)                                    //> res3: ((Int, Double), random.RNG) = ((-66576884,0.6738289802678996),SimpleR
                                                  //| NG(94832998272600))
  doubleInt(r)                                    //> res4: ((Double, Int), random.RNG) = ((0.6738289802678996,-66576884),SimpleR
                                                  //| NG(94832998272600))
  double3(r)                                      //> res5: ((Double, Double, Double), random.RNG) = ((0.6738289802678996,0.29438
                                                  //| 86058844573,0.6604286961538851),SimpleRNG(92947075901660))

  ints(5)(r)                                      //> res6: (List[Int], random.RNG) = (List(-66576884, 1447036716, -1079256938, 6
                                                  //| 32194717, -876430233),SimpleRNG(224037245001077))
  sequence(List.fill(5)(both(int, double)))(r)    //> res7: (List[(Int, Double)], random.RNG) = (List((-66576884,0.67382898026789
                                                  //| 96), (-1079256938,0.2943886058844573), (-876430233,0.6604286961538851), (-7
                                                  //| 10068556,0.8777587045346195), (-377955024,0.9829864813866962)),SimpleRNG(13
                                                  //| 8343048423328))
  nonNegativeEven(r)                              //> res8: (Int, random.RNG) = (1447036716,SimpleRNG(94832998272600))

  nonNegativeLessThan0(16)(r)                     //> res9: (Int, random.RNG) = (12,SimpleRNG(94832998272600))
  nonNegativeLessThan(16)(r)                      //> res10: (Int, random.RNG) = (12,SimpleRNG(94832998272600))

  sequence(List.fill(10)(nonNegativeLessThan0(114765284)))(r)
                                                  //> res11: (List[Int], random.RNG) = (List(69853308, 58368297, 41076417, 487279
                                                  //| 20, 94828412, 14505460, 78587846, 63395989, 72538739, 94099246),SimpleRNG(9
                                                  //| 6421980037608))
  sequence(List.fill(10)(nonNegativeLessThan(114765284)))(r)
                                                  //> res12: (List[Int], random.RNG) = (List(69853308, 58368297, 41076417, 487279
                                                  //| 20, 94828412, 14505460, 78587846, 63395989, 72538739, 94099246),SimpleRNG(9
                                                  //| 6421980037608))
  int1(r)                                         //> res13: (Int, random.RNG) = (-66576884,SimpleRNG(277111794047841))

	val m = Machine(true, 5, 10)              //> m  : random.Machine = Machine(true,5,10)
	simulateMachine(List(Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn))(m)
                                                  //> res14: ((Int, Int), random.Machine) = ((1,14),Machine(true,1,14))
	
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}
