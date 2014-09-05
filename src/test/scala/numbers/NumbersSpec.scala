package numbers

import org.scalatest.FunSpec
import scala.io.Source
import AccountId._

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
          "|_||_|  ||_||_|  ||_||_|  |")).right.map(_.map(Numbers.parseDigit).mkString) == Right("007007007"))
    }

    
    it("finds possible mutations on a digit"){
      val digit = List(
        " _ ",
        "| |",
        " _|")
 
      assert(Numbers.laxMatches(digit) === Seq('0','9'))
      
    }
  }

  describe("Account validation") {
    it("calculates checksums correctly") {
      assert(AccountId("457508000").isValid)
      assert(!AccountId("664371495").isValid)
    }

    it("outputs report lines") {
      assert(Account("457508000",Seq(),Seq()).tabulatedString == "457508000\t")
      assert(Account("664371495",Seq(),Seq()).tabulatedString == "664371495\tERR")
      assert(Account("45750?000",Seq(),Seq()).tabulatedString == "45750?000\tILL")
    }
    
    it("figures out alternatives with correct checksums"){
      assert(AccountId("490067715").findSimilarAccounts().filter(_.isValid).toSet == Set(AccountId("490067115"), AccountId("490067719"), AccountId("490867715")))
    }

    it("when an invalid scan has alternatives, it picks the one with the right checksum"){
      assert(Account.parse(List(
        " _  _  _  _  _  _  _  _  _ ",
        "|_||_||_||_||_||_||_||_||_|",
        "|_  _| _| _| _| _| _| _| _|")).right.get.accountId === AccountId("899999999"))

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
