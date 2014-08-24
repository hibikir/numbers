package numbers

import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.scalatest.path
import scala.util.Random
import numbers.Numbers.RenderedAccountNumber
import org.scalacheck.Gen


class NumbersPropertySpec extends path.FunSpec with GeneratorDrivenPropertyChecks with Checkers{

  val someInvalidDigits = List(
    List(" _ ",
         "|  ",
         "|_ "),
    List(" _ ",
         "| |",
         " _ "),
    List("   ",
         "| |",
         "  "),
    List("   ",
         " _ ",
         " _ "))
  val random = new Random()
  case class AccountTestParams(nDigits:Int,nInvalid:Int)
  
  val validTestParams =for(d <-Gen.choose(0,10);
                          i <-Gen.choose(0,d)) yield AccountTestParams(d,i)
  
  it("returns accounts"){
    forAll(validTestParams){ (params:AccountTestParams) =>
        val digits = for(i<-0.until( params.nDigits - params.nInvalid)) yield Numbers.allRenderedDigits(random.nextInt(10))
        val invalidDigits =for(i<-0.until(params.nInvalid)) yield Numbers.allRenderedDigits(random.nextInt(someInvalidDigits.size))
        val line:RenderedAccountNumber = Random.shuffle(digits++invalidDigits).transpose.map(_.mkString)
        
        val account = Account.parse(line)
        if(params.nDigits != 9){
          account.isLeft === true
        }
        else{
          val shouldBeLegible = params.nInvalid==0
          account.right.get.isLegible === shouldBeLegible
        }
      }   
  }
}
