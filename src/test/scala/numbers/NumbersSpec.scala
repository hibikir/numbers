package numbers

import org.scalatest.FunSpec
import scala.io.Source


class NumbersSpec extends FunSpec{

  //it("produces some tape"){
  //  val renderedString = Numbers.renderString("1234567890")
  //  assert(renderedString.split('\n').size == 3)
  //}

  it("parses a digit"){
    assert(Numbers.parseDigit(List(" _ ",
                                   "| |",
                                   "|_|")) =='0')
  }

  it("parses a row"){
    assert(Numbers.parseLine(List(" _  _  _  _  _  _  _  _  _ ",
                                  "| || |  || || |  || || |  |",
                                  "|_||_|  ||_||_|  ||_||_|  |")) == "007007007")
  }

  it("parses a file"){
    val file = Source.fromURL(getClass.getResource("/testfile.txt"))
    val parsed = FileParser.parse(file)
    file.close()
    assert(parsed.size == 14)
    assert(parsed(0).isValid)
    assert(!parsed(13).isValid)
  }
}
