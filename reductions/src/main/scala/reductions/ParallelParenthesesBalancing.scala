package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def bal(chars: Array[Char], acc: Int): Int = chars match {
      case Array() => acc
      case Array('(', _*) => bal(chars.tail, acc+1)
      case Array(')', _*) => acc match {
        case x if x > 0 => bal(chars.tail, acc-1)
        case _ => -1
      }
      case _ => bal(chars.tail, acc)
    }
    bal(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else {
        val (newArg1, newArg2) = chars(idx) match {
          case '(' => (arg1, arg2 + 1)
          case ')' => arg2 match {
            case 0 => (arg1 - 1, arg2)
            case _ => (arg1, arg2 - 1)
          }
          case _ => (arg1, arg2)
        }
        traverse(idx+1, until, newArg1, newArg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until)/2
        val ((left1, left2), (right1, right2)) = parallel(reduce(from, mid), reduce(mid, until))
        (left1 + Math.min(left2 + right1, 0), Math.max(0, left2 + right1) + right2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
