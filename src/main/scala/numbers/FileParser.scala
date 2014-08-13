package numbers

import scala.io.Source
import numbers.Numbers.RenderedAccountNumber

object FileParser{
  val rows_per_line = 4
  def trimVerticalPadding(lines:Seq[String]):RenderedAccountNumber = lines.take(Numbers.character_height)

  def parse(source:Source):Seq[Account] = {
    val lines = source.getLines()
    lines.grouped(rows_per_line).map{xs =>
      Account.parse(trimVerticalPadding(xs))
    }.flatten.toList
  }
}
