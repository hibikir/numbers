package numbers

import org.scalatest.FunSpec


class NumbersSpec extends FunSpec{

  it("produces some tape"){
    println(Numbers.renderString("1234567890"))
  }

  it("parses a digit"){
    println(Numbers.parseDigit(List(" _ ","| |","|_|")))
  }

  it("parses a row"){
    println(Numbers.parseDigit(List(" _ ","| |","|_|")))
  }
}
