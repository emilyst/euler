import org.scalatest.FunSuite
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Minutes
import org.scalatest.time.Span

import scala.io.Source

class EulerSpec extends FunSuite with TimeLimitedTests {
  val timeLimit = Span(1, Minutes)  // enforce one-minute rule

  /**
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
  test("Project Euler Problem 99") {
    val (_, lineNumber) = Source
      .fromResource("p099_base_exp.txt")
      .getLines
      .zipWithIndex  // add in line numbers
      .map { case (line, lineNumber) =>
        val Array(mantissa, exponent) = line.split(",").take(2)
        (exponent.toDouble * math.log(mantissa.toDouble), lineNumber)
      }
      .maxBy { case (result, lineNumber) =>
        result
      }

    println(s"Problem 99: Largest number is on line '${lineNumber + 1}'")
  }

  /**
   * > Find the unique positive integer whose square has the form
   * > 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.
   *
   * All right. That's a nineteen-digit number whose square root is an
   * integer. Can't be too many of those, right? My first impression is
   * actually that I can brute-force the solution by squaring all
   * numbers starting from, oh, let's say, 1000000000 (which would put
   * us in the neighborhood, at 1e18), but we're still an order of
   * magnitude off, and there's actually a lot of numbers up here.
   *
   * I have a suspicion that the ending in zero is significant.
   * I imagine only certain numbers can be squared such that the final
   * digit is zero. That limits the brute-forcing space significantly if
   * I identify those numbers.
   *
   * On thinking about it, I suspect that only numbers ending in zero
   * can have squares ending in zero. But those squares always end in
   * zero-zero. So this mystery number must end in 900. Which means that
   * the root must end in 30 or 70. We can eliminate other
   * possibilities, so we've dramatically reduced the search space.
   *
   * I suspect that similar patterns extend up to the higher digits. I'm
   * tired and have a headache and don't want to think about it. I'm
   * going to look up the solution now.
   *
   * Having looked it up, it seems that there is no more clever solution
   * than what I have found, so I have just to implement it. I will do
   * this soon.
   */
  test("Project Euler Problem 206") {
    // do {

    // } while
  }
}
