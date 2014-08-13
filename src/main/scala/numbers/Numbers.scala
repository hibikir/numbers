package numbers

import numbers.Numbers.RenderedAccountNumber

object Numbers {

  type RenderedDigit = Seq[String]
  type RenderedAccountNumber = Seq[String]
  val character_width = 3
  val characters_per_line = 9
  val spriteSheet = """ x _     _  _     _  _  _  _  _ 
                        x| |  | _| _||_||_ |_   ||_||_|
                        x|_|  ||_  _|  | _||_|  ||_| _|""".stripMargin('x')

  val blank:RenderedDigit = List("   ","   ","   ")

  private val allRenderedDigits:Seq[RenderedDigit] = {
    val rows = spriteSheet.split("\n").toList
    toRenderedDigits(rows)
  }

  def parseLine(r:RenderedAccountNumber) :Option[String] =
    validateRecordLength(r) match {
      case Left(x) => println(x); None
      case Right(x) => Some(parseValidLine(x))
    }
  
  private def parseValidLine(r:RenderedAccountNumber) :String = {
    val chunkedLine = toRenderedDigits(r)
    chunkedLine.map(parseDigit).mkString
  }

  def parseDigit(r:RenderedDigit) :Char = {
    allRenderedDigits.indexOf(r) match {
      case -1 => 
        println(r.mkString("\n")+" did not match any numbers")
        '?'
      case n:Int => n.toString.charAt(0)
    }
  }

  private def splitByCharacter(row:String):List[String] = row.grouped(character_width).toList

  private def toRenderedDigits(s:RenderedAccountNumber) :Seq[RenderedDigit] = {
     s.map(splitByCharacter).transpose
  }
  
  def validateRecordLength(s:RenderedAccountNumber):Either[String,RenderedAccountNumber] = {
    s.find(_.size!=character_width * characters_per_line) match {
      case Some(x) => Left("Line of inappropriate size\n"+x)
      case _ => Right(s)
    }
  }
 }

case class Account(accountId: String){
  val isLegible = accountId.size == Numbers.characters_per_line && !accountId.contains('?')
  val isValid:Boolean = {
    if(!isLegible) false
    else{
      val checksum:Int = accountId.toCharArray.zip(1.to(9).reverse).map{
        case (char,pos) => 
          char.asDigit*pos
       }.sum
      checksum %11 ==0
    }
  }
  
  val tabulatedString:String  = {
    val suffix = if(!isLegible) "ILL" else if(!isValid) "ERR" else ""
    accountId + "\t"+ suffix
  }
}

object Account {
  def parse(r:RenderedAccountNumber) :Option[Account] = Numbers.parseLine(r).map(x=>Account(x))
}

