object list {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //ex3_2
    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    //ex3_3
    def setHead[A](list: List[A], head: A): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => Cons(head, xs)
    }

    //ex3_4
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (n == 0) l else drop(xs, n - 1)
    }

    //ex3_5
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (!f(x)) l else dropWhile(xs)(f)
    }

    //ex3_6
    def init[A](l: List[A]): List[A] = l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case _ => l
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    //ex3_9
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => (acc + 1))

    //ex3_10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    //ex3_11
    def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
    def product3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
    def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => (acc + 1))

    //ex3_12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((x, y) => Cons(y, x))

    //ex3_13
    def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      foldRight(reverse(as), z)((x, y) => f(y, x))
    }

    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(as), z)((x, y) => f(y, x))
    }
    
    //ex3_14
    def append1[A](a: List[A], b: List[A]): List[A] = {
    	foldRight(a, b)(Cons(_, _))
    }
    
    def append2[A](a: List[A], b: List[A]): List[A] = {
    	foldLeft(reverse(a), b)((x, y) => Cons(y, x))
    }
    
    //ex3_15
    def concate[A](l: List[List[A]]): List[A] = {
    	foldLeft(l, List[A]())(append1)
    }
    
    //ex3_16
    def addone(l: List[Int]): List[Int] = {
    	foldRight(l, List[Int]())((x, y) => Cons(x+1, y))
		}
		
		//ex3_17
		def d2str(l: List[Double]): List[String] = {
			foldRight(l, List[String]())((x, y) => Cons(x.toString, y))
		}
		
		//ex3_18
		def map[A,B](as: List[A])(f: A => B): List[B] = {
			foldRight(as, List[B]())((x, y) => Cons(f(x), y))
		}
		
		//ex3_19
		def filter[A](as: List[A])(f: A => Boolean): List[A] = {
			foldRight(as, List[A]())((x, y) => if (f(x)) Cons(x, y) else y)
		}
		
		//ex3_20
		def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
			foldRight(as, List[B]())((x, y) => append1(f(x), y))
		}
		
		//ex3_21
		def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
			flatMap(as)(x => if (f(x)) List(x) else Nil)
		}
		
		//ex3_22
		def ladd(a: List[Int], b: List[Int]): List[Int] = {
			(a, b) match {
				case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, ladd(xs, ys))
				case (Nil, _) => b
				case (_, Nil) => a
			}
		}
		
		//ex3_23
		def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = {
			(a, b) match {
				case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
				case (Nil, _) => b
				case (_, Nil) => a
			}
		}
		
		//ex3_24
		def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
			(l, sub) match {
				case (Cons(x, xs), Cons(y, ys)) => ((x == y) && hasSubsequence(xs, ys)) || hasSubsequence(xs, sub)
				case (_, Nil) => true
				case (Nil, _) => false
			}
		}
		
				
  }

  //ex3_1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => { println("match this!"); x + y }
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }                                               //> match this!
                                                  //| x  : Int = 3

  val xs = List(1, 3, 17, 9, 88)                  //> xs  : list.List[Int] = Cons(1,Cons(3,Cons(17,Cons(9,Cons(88,Nil)))))
  List.tail(xs)                                   //> res0: list.List[Int] = Cons(3,Cons(17,Cons(9,Cons(88,Nil))))
  List.setHead(xs, 'x')                           //> res1: list.List[AnyVal{def getClass(): Class[_ >: Char with Int <: AnyVal]}
                                                  //| ] = Cons(x,Cons(3,Cons(17,Cons(9,Cons(88,Nil)))))
  List.drop(Nil, 100)                             //> res2: list.List[Nothing] = Nil
  List.drop(xs, 2)                                //> res3: list.List[Int] = Cons(17,Cons(9,Cons(88,Nil)))
  List.dropWhile(xs)(_ <= 7)                      //> res4: list.List[Int] = Cons(17,Cons(9,Cons(88,Nil)))
  List.init(xs)                                   //> res5: list.List[Int] = Cons(1,Cons(3,Cons(17,Cons(9,Nil))))

  List.foldRight(List(1, 2, 44), Nil: List[Int])(Cons(_, _))
                                                  //> res6: list.List[Int] = Cons(1,Cons(2,Cons(44,Nil)))
  List.sum3(xs)                                   //> res7: Int = 118
  List.reverse(xs)                                //> res8: list.List[Int] = Cons(88,Cons(9,Cons(17,Cons(3,Cons(1,Nil)))))
  List.foldLeft2(xs, 0)(_ + _)                    //> res9: Int = 118
  List.foldRight2(xs, 0)(_ + _)                   //> res10: Int = 118
  List.append1(xs, List('a', 'b', 'c'))           //> res11: list.List[AnyVal{def getClass(): Class[_ >: Char with Int <: AnyVal]
                                                  //| }] = Cons(1,Cons(3,Cons(17,Cons(9,Cons(88,Cons(a,Cons(b,Cons(c,Nil))))))))
  List.append2(xs, List(999, 888))                //> res12: list.List[Int] = Cons(1,Cons(3,Cons(17,Cons(9,Cons(88,Cons(999,Cons(
                                                  //| 888,Nil)))))))
  List.concate(List(xs, List('x', 'y', 'z'), List(30, 99)))
                                                  //> res13: list.List[AnyVal] = Cons(1,Cons(3,Cons(17,Cons(9,Cons(88,Cons(x,Cons
                                                  //| (y,Cons(z,Cons(30,Cons(99,Nil))))))))))
  List.addone(xs)                                 //> res14: list.List[Int] = Cons(2,Cons(4,Cons(18,Cons(10,Cons(89,Nil)))))
  val dl = List.d2str(List(4.0, 8294, 0.001, 24.2))
                                                  //> dl  : list.List[String] = Cons(4.0,Cons(8294.0,Cons(0.001,Cons(24.2,Nil))))
                                                  //| 
  List.map(xs)(x=>x*x)                            //> res15: list.List[Int] = Cons(1,Cons(9,Cons(289,Cons(81,Cons(7744,Nil)))))
  List.filter(xs)(_ % 2 == 0)                     //> res16: list.List[Int] = Cons(88,Nil)
  List.flatMap(xs)(x => List(x, x, x))            //> res17: list.List[Int] = Cons(1,Cons(1,Cons(1,Cons(3,Cons(3,Cons(3,Cons(17,C
                                                  //| ons(17,Cons(17,Cons(9,Cons(9,Cons(9,Cons(88,Cons(88,Cons(88,Nil))))))))))))
                                                  //| )))
  List.filter2(xs)(_ % 2 == 0)                    //> res18: list.List[Int] = Cons(88,Nil)
  List.ladd(List(1,2,3), List(100,200,300, 999))  //> res19: list.List[Int] = Cons(101,Cons(202,Cons(303,Cons(999,Nil))))
  List.zipWith(List(1,2,3), List(100,200,300,999))(_ * _)
                                                  //> res20: list.List[Int] = Cons(100,Cons(400,Cons(900,Cons(999,Nil))))
  List.hasSubsequence(List(1,2,3,4,5), List(2,4)) //> res21: Boolean = true
  List.hasSubsequence(List(1,2,3,4,5), List(5,3)) //> res22: Boolean = false
}
