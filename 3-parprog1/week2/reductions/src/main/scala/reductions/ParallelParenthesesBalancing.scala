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


  object Balance {
    def empty = Balance(0, Neutral)

    sealed trait Side
    object Left extends Side
    object Right extends Side
    object Neutral extends Side

  }

  case class Balance(accumlator: Int, startsWith: Balance.Side) {
    import Balance._

    def + (other: Balance): Balance = copy(
      accumlator + other.accumlator,
      if (startsWith == Neutral) other.startsWith else startsWith
    )

    def + (char: Char): Balance = char match {
      case '(' => this + Balance(1, Left)
      case ')' => this + Balance(-1, Right)
      case _ => this
    }

    def isBalanced: Boolean = accumlator == 0 && startsWith != Right

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def inner(idx: Int, balance: Balance): Balance = {
      if (idx >= chars.length) balance else inner(idx + 1, balance + chars(idx))
    }
    inner(0, Balance.empty).isBalanced
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, balance: Balance): Balance = {
      if (idx >= until) balance else traverse(idx + 1, until, balance + chars(idx))
    }

    def reduce(from: Int, until: Int): Balance = {
      if (until <= from || until - from <= threshold){
        traverse(from, until, Balance.empty)
      } else {
        val midPoint = (until - from) / 2 + from
        val (l,r) = parallel(reduce(from, midPoint - 1), reduce(midPoint, until))
        l + r
      }
    }

    reduce(0, chars.length).isBalanced
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
