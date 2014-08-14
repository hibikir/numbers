package numbers

import org.scalatest.FunSpec
import scala.io.Source


class NumbersSpec extends FunSpec {

  describe("Number utilities") {
    it("parses a digit") {
      assert(Numbers.parseDigit(List(
          " _ ",
          "| |",
          "|_|")) == '0')
    }

    it("parses a row") {
      assert(Numbers.parseLine(List(
          " _  _  _  _  _  _  _  _  _ ",
          "| || |  || || |  || || |  |",
          "|_||_|  ||_||_|  ||_||_|  |")) == Right("007007007"))
    }

  }

  describe("Account validation") {
    it("calculates checksums correctly") {
      assert(Account("457508000").isValid)
      assert(!Account("664371495").isValid)
    }

    it("outputs report lines") {
      assert(Account("457508000").tabulatedString == "457508000\t")
      assert(Account("664371495").tabulatedString == "664371495\tERR")
      assert(Account("45750?000").tabulatedString == "45750?000\tILL")
    }
  }

  describe("File Parsing") {
    it("parses a file") {
      val file = Source.fromURL(getClass.getResource("/testfile.txt"))
      val parsed = FileParser.parse(file)
      file.close()
      assert(parsed.size == 14)
      assert(parsed(0).isValid)
      assert(!parsed(13).isValid)
    }
  }

  describe("Manual tests for exercise validation") {
    it("Use case 3") {
      val file = Source.fromURL(getClass.getResource("/testfile.txt"))
      val parsed = FileParser.parse(file)
      file.close()
      println(parsed.map(_.tabulatedString).mkString("\n"))
    }
  }
}
