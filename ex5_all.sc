object lz {
	import fp.laziness._
		
	lazy val as = Stream(-9,1,2,3)            //> as: => fp.laziness.Stream[Int]
	as.takeWhile(_ < 3).toList                //> a
                                                  //| res0: List[Int] = List(-9, 1, 2)
  as.headOption                                   //> res1: Option[Int] = Some(-9)
  
  //first order loop!!
	Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList
                                                  //> res2: List[Int] = List(12, 14)
  //infinite!!
  lazy val ones: Stream[Int] = constant(1)        //> ones: => fp.laziness.Stream[Int]
  ones.take(10).toList                            //> res3: List[Int] = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  ones.exists(_ % 2 != 0)                         //> res4: Boolean = true
  ones.map(_ + 1).exists(_ % 2 == 0)              //> res5: Boolean = true
  ones.takeWhile(_ == 1)                          //> res6: fp.laziness.Stream[Int] = Cons(<function0>,<function0>)
  ones.forAll(_ != 1)                             //> res7: Boolean = false
  
  from(100).take(5).toList                        //> res8: List[Int] = List(100, 101, 102, 103, 104)
  fibs((1, 1)).take(20).toList                    //> res9: List[Int] = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 
                                                  //| 610, 987, 1597, 2584, 4181, 6765)
                                                  
  ones1.take(9).toList                            //> res10: List[Int] = List(1, 1, 1, 1, 1, 1, 1, 1, 1)
  constant1(88).take(5).toList                    //> res11: List[Int] = List(88, 88, 88, 88, 88)
  from1(1000).take(12).toList                     //> res12: List[Int] = List(1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008
                                                  //| , 1009, 1010, 1011)
  fibs1.take(15).takeWhile(_ < 60).toList         //> res13: List[Int] = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  
  fibs1.zipWith(ones)(_ + 100 * _).take(10).toList//> res14: List[Int] = List(101, 101, 102, 103, 105, 108, 113, 121, 134, 155)
  
  fibs1.zipAll(from1(100).take(10)).take(20).toList
                                                  //> res15: List[(Option[Int], Option[Int])] = List((Some(1),Some(100)), (Some(1)
                                                  //| ,Some(101)), (Some(2),Some(102)), (Some(3),Some(103)), (Some(5),Some(104)), 
                                                  //| (Some(8),Some(105)), (Some(13),Some(106)), (Some(21),Some(107)), (Some(34),S
                                                  //| ome(108)), (Some(55),Some(109)), (Some(89),None), (Some(144),None), (Some(23
                                                  //| 3),None), (Some(377),None), (Some(610),None), (Some(987),None), (Some(1597),
                                                  //| None), (Some(2584),None), (Some(4181),None), (Some(6765),None))
  
  Stream(1,3,4,5).startsWith(Stream(1,3,4))       //> res16: Boolean = true
  fibs1.tails.map(x => x.take(10).toList.toString).take(10).toList mkString "\n"
                                                  //> res17: String = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
                                                  //| List(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
                                                  //| List(2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
                                                  //| List(3, 5, 8, 13, 21, 34, 55, 89, 144, 233)
                                                  //| List(5, 8, 13, 21, 34, 55, 89, 144, 233, 377)
                                                  //| List(8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
                                                  //| List(13, 21, 34, 55, 89, 144, 233, 377, 610, 987)
                                                  //| List(21, 34, 55, 89, 144, 233, 377, 610, 987, 1597)
                                                  //| List(34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584)
                                                  //| List(55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)
  Stream(1,2,3).scanRight(0)(_ + _).toList        //> res18: List[Int] = List(6, 5, 3, 0)
                      
  
  Stream(1,3,5,7,12).hasSubsequence(Stream(7,12)) //> res19: Boolean = true
  
  Stream(1,2,3,4).scanRight(1)(_ * _).toList      //> res20: List[Int] = List(24, 24, 12, 4, 1)
}
