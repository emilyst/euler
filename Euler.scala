import java.time.Duration
import java.time.Instant
import scala.io.Source

object Euler {
  def main(args: Array[String]): Unit = {
    Problem99.solve()
  }
}

/**
 * Project Euler Problem 99
 *
 * > Comparing two numbers written in index form like 2^11 and 3^7 is
 * > not difficult, as any calculator would confirm that 2^11 = 2048
 * > < 3^7 = 2187.
 * >
 * > However, confirming that 632382^518061 > 519432^525806 would be
 * > much more difficult, as both numbers contain over three million
 * > digits.
 * >
 * > Using base_exp.txt (right click and 'Save Link/Target As...'),
 * > a 22K text file containing one thousand lines with a base/exponent
 * > pair on each line, determine which line number has the greatest
 * > numerical value.
 * >
 * > NOTE: The first two lines in the file represent the numbers in the
 * > example given above.
 *
 * Being out of practice, I looked at some solutions for this, and I was
 * initially confused, as they often referred to a `log` function
 * without reference to which logarithm was in use. I thought it
 * mattered.
 *
 * The solutions to this problem were hinting that by using a logarithm,
 * the power identity of logarithms allowed me to change an
 * exponentiation to a multiplication, a much simpler and quicker
 * operation. I was momentarily confused by how this helped until
 * I realized that the power identity held the logarithm base constant.
 * So it didn't matter whether I were using the natural, decimal, or
 * binary logarithm.
 *
 * Therefore, according to the power identity of logarithms, the
 * following expressions are identical.
 *
 *     log(2^4)
 *     4 * log(2)
 *
 * It doesn't matter if it's log-base-10 or log-base-e or whatever
 * else.
 *
 * Once I cleared up my own misconception about the (ir)relevance of the
 * logarithmic base, it was easy to proceed.
 */
object Problem99 {
  def solve(): Unit = {
    val before = Instant.now

    val (_, lineNumber) = Source
      .fromFile("resources/p099_base_exp.txt")
      .getLines
      .zipWithIndex  // add in line numbers
      .map { case (line, lineNumber) =>
        val Array(mantissa, exponent) = line.split(",").take(2)
        (exponent.toDouble * math.log(mantissa.toDouble), lineNumber)
      }
      .maxBy { case (result, lineNumber) =>
        result
      }

    val after = Instant.now

    println(s"Problem 99: Largest number is on line '${lineNumber + 1}'")
    println(s"Problem 99: Solution found in '${Duration.between(before, after).toMillis}' milliseconds")
  }
}
