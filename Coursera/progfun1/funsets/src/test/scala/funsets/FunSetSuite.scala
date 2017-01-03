package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    
    //[-100, 200]
    val s4 = (x: Int) => (x >= -100 && x <= 200)
    //[-100, 500]
    val s5 = (x: Int) => (x >= -100 && x <= 500)
    //[300, 777]
    val s6 = (x: Int) => (x >= 300 && x <= 777)
    
    val s7 = (x: Int) => (x == 2 || x == 4 || x == 8 || x == 16 || x == 32)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect of two sets") {
    new TestSets {
      assert(contains(intersect(s2, s4), 2), "Instersect 1")
      assert(!contains(intersect(s4, s6), 200), "Instersect 2")
      assert(contains(intersect(s5, s6), 400), "Instersect 3")
    }
  }
  
  test("diff of two sets") {
    new TestSets {
      assert(!contains(diff(s5, s4), 200), "Diff 1")
      assert(contains(diff(s5, s4), 201), "Diff 2")
    }
  }

  test("filter") {
    new TestSets {
      assert(!contains(filter(s4, (x:Int) => x > 200), 200), "Filter 1")
      assert(contains(filter(s5, (x:Int) => x > 499), 500), "Filter 2")
    }
  }
  
  test("forall") {
    new TestSets {
      assert(forall(s7, (x:Int) => (x & (x - 1)) == 0))
      assert(!forall(s7, (x:Int) => (x != 32)))
    }
  }
  
  test("exists") {
    new TestSets {
      assert(exists(s7, (x:Int) => (x * 2 == 64)))
    }
  }
  
 test("map") {
   new TestSets {
     val p = map(s7, (x:Int) => x / 2)
     assert(contains(p, 1) && contains(p, 2) && contains(p, 4) && contains(p, 8) && contains(p, 16))
   }
 }

}
