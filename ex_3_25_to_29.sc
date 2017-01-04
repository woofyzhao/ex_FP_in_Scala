object tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  
  //ex3_25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }                                               //> size: [A](tree: tree.Tree[A])Int
  
  //ex3_26
  def tmax(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(l, r) => tmax(l) max tmax(r)
    }
  }                                               //> tmax: (tree: tree.Tree[Int])Int
  
  //ex3_27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }                                               //> depth: [A](tree: tree.Tree[A])Int
  
  //ex3_28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }                                               //> map: [A, B](tree: tree.Tree[A])(f: A => B)tree.Tree[B]
  
  //ex3_29
  def fold[A,B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = {
    tree match {
      case Leaf(x) => lf(x)
      case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
    }
  }                                               //> fold: [A, B](tree: tree.Tree[A])(lf: A => B)(bf: (B, B) => B)B
  
  def size1[A](tree: Tree[A]): Int = fold(tree)(x=>1)(1 + _ + _)
                                                  //> size1: [A](tree: tree.Tree[A])Int
  def tmax1(tree: Tree[Int]): Int = fold(tree)(x=>x)(_ max _)
                                                  //> tmax1: (tree: tree.Tree[Int])Int
  def depth1[A](tree: Tree[A]): Int = fold(tree)(x=>1)(1 + _ max _)
                                                  //> depth1: [A](tree: tree.Tree[A])Int
  def map1[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(x=>(Leaf(f(x)).asInstanceOf[Tree[B]]))((x, y) => Branch(x,y))
                                                  //> map1: [A, B](tree: tree.Tree[A])(f: A => B)tree.Tree[B]
  
  val tree = Branch(Branch(Leaf(1), Leaf(23)), Branch(Leaf(7), Leaf(8)))
                                                  //> tree  : tree.Branch[Int] = Branch(Branch(Leaf(1),Leaf(23)),Branch(Leaf(7),L
                                                  //| eaf(8)))
  size(tree)                                      //> res0: Int = 7
  tmax(tree)                                      //> res1: Int = 23
  depth(tree)                                     //> res2: Int = 3
  map(tree)(x=>x*x)                               //> res3: tree.Tree[Int] = Branch(Branch(Leaf(1),Leaf(529)),Branch(Leaf(49),Lea
                                                  //| f(64)))
  size1(tree)                                     //> res4: Int = 7
  tmax1(tree)                                     //> res5: Int = 23
  depth1(tree)                                    //> res6: Int = 3
  map1(tree)(x=>x*x)                              //> res7: tree.Tree[Int] = Branch(Branch(Leaf(1),Leaf(529)),Branch(Leaf(49),Lea
                                                  //| f(64)))
                                                  
  
}
