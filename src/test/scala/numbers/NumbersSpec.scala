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
  
  it("calculates checksums correctly"){
    assert(Account("457508000").isValid)
    assert(!Account("664371495").isValid)
  }
  
  it("outputs report lines"){
    assert(Account("457508000").tabulatedString == "457508000\t")
    assert(Account("664371495").tabulatedString == "664371495\tERR")
    assert(Account("45750?000").tabulatedString == "45750?000\tILL")
  }
  
  it("outputs the given file"){
    val file = Source.fromURL(getClass.getResource("/testfile.txt"))
    val parsed = FileParser.parse(file)
    file.close()
    println(parsed.map(_.tabulatedString).mkString("\n"))
  }
}
