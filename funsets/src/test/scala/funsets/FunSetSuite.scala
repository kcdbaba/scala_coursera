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
    val emptySet = (x: Int) => false
    val universalSet = (x: Int) => true
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
      assert(!contains(s1, 2), "Singleton")

      // Test empty/universal set
      assert(!contains(emptySet, 1), "Contains empty 1")
      for (i <- (-bound to bound)) assert(contains(universalSet, i), "Contains universal " + i.toString)
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

  test("union with empty set contains elements of other set") {
    new TestSets {
      val s = union(s1, emptySet)
      assert(contains(s, 1), "Union empty 1")
    }
  }

  test("union with universal set contains all integers") {
    new TestSets {
      val s = union(s1, universalSet)
      for (i <- (-bound to bound)) assert(contains(s, i), "Union universal " + i.toString)
    }
  }

  test("intersect contains common elements of each set") {
    new TestSets {
      val s = intersect(union(s1, s2), s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
    }
  }

  test("intersect with empty set does not contain elements of other set") {
    new TestSets {
      val s = intersect(s1, emptySet)
      assert(!contains(s, 1), "Intersect empty 1")
    }
  }

  test("intersect with universal set contains elements of other set") {
    new TestSets {
      val s = intersect(s1, universalSet)
      assert(contains(s, 1), "Intersect universal 1")
    }
  }

  test("diff contains elements present in first set, but not in the second") {
    new TestSets {
      val s = diff(union(s1, s2), s1)
      assert(!contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
    }
  }

  test("diff of a set against empty set contains elements of first set") {
    new TestSets {
      val s = diff(s1, emptySet)
      assert(contains(s, 1), "Diff empty 1")
    }
  }

  test("diff of empty set against a set does not contain elements of second set") {
    new TestSets {
      val s = diff(emptySet, s1)
      assert(!contains(s, 1), "Diff empty 2")
    }
  }

  test("diff of a set against universal set does not contain elements of first set") {
    new TestSets {
      val s = diff(s1, universalSet)
      assert(!contains(s, 1), "Diff universal 1")
    }
  }

  test("diff of universal set against a set does not contain elements of second set") {
    new TestSets {
      val s = diff(universalSet, s1)
      assert(!contains(s, 1), "Diff universal 2")
    }
  }

  test("filter contains elements of set that satisfy predicate") {
    new TestSets {
      val s = filter(union(s1, s2), x => x == 1 || x == 3)
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
    }
  }

  test("filter of empty set is empty") {
    new TestSets {
      val s = filter(emptySet, x => x == 1)
      assert(!contains(s, 1), "Filter empty 1")
    }
  }

  test("forall satisfies predicate for all elements of a set") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(forall(s, x => List(1, 2, 3).contains(x)), "Forall 1")
      assert(!forall(s, x => List(1, 2).contains(x)), "Forall 2")

      // Test empty/universal set
      assert(forall(emptySet, _ => true), "Forall is always true for empty set 1")
      assert(forall(emptySet, _ => false), "Forall is always true for empty set 2")
      assert(forall(universalSet, _ => true), "Forall matches predicate for universal set 1")
      assert(!forall(universalSet, _ => false), "Forall matches predicate for universal set 2")
    }
  }

  test("exists satisfies predicate for at least one element of a set") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      for (i <- List(1,2,3)) assert(exists(s, x => x == i), "Exists " + i.toString)
      assert(!exists(s, x => x == 4), "Exists 4")
      assert(exists(s, x=> true), "Exists 5")
      assert(!exists(s, _ => false), "Exists 6")

      // Test empty/universal set
      assert(!exists(emptySet, _ => true), "Exists is always false for empty set 1")
      assert(!exists(emptySet, _ => false), "Exists is always false for  empty set 2")
      assert(exists(universalSet, _ => true), "Exists matches predicate for universal set 1")
      assert(!exists(universalSet, _ => false), "Exists matches predicate for universal set 2")
    }
  }

  test("map transforms a set by applying all elements to function") {
    new TestSets {
      val s = map(union(s1, s3), x => 2 * x)
      for (m <- List(2, 6)) assert(contains(s, m), "Map " + m.toString)
      for (m <- List(1, 3)) assert(!contains(s, m), "Map " + m.toString)
    }
  }
}
