package numbers

import numbers.Numbers.RenderedAccountNumber

object Numbers {

  type RenderedDigit = Seq[String]
  type RenderedAccountNumber = Seq[String]
  val character_width = 3
  val character_height = 3
  val characters_per_line = 9
  private val spriteSheet = 
    """ x _     _  _     _  _  _  _  _ 
        x| |  | _| _||_||_ |_   ||_||_|
        x|_|  ||_  _|  | _||_|  ||_| _|""".stripMargin('x')

  val allRenderedDigits:Seq[RenderedDigit] = {
    val rows = spriteSheet.split("\n").toSeq
    toRenderedDigits(rows)
  }

  def parseLine(r:RenderedAccountNumber) :Either[String,String] =
    validateRecordLength(r).right.map(x=>parseValidLine(x))

  def parseLineToRenderedDigits(r:RenderedAccountNumber) :Either[String,Seq[RenderedDigit]] =
    validateRecordLength(r).right.map(x=>toRenderedDigits(x))


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

  private def splitByCharacter(row:String):Seq[String] = row.grouped(character_width).toSeq

  private def toRenderedDigits(s:RenderedAccountNumber) :Seq[RenderedDigit] = {
     s.map(splitByCharacter).transpose
  }
  
  private def validateRecordLength(s:RenderedAccountNumber):Either[String,RenderedAccountNumber] = {
    s.find(_.size!=character_width * characters_per_line) match {
      case Some(x) => Left("Line of inappropriate size\n"+x)
      case _ => Right(s)
    }
  }
  
  private def mutateCharacter(c:Char) :Seq[Char] = {
    c match {
      case ' ' => Seq('_','|')
      case '_' | '|' => Seq(' ')  
    }
  }
  
  private def mutateDigit(digit:RenderedDigit) :Seq[RenderedDigit]  = {
    for(h<-0 until character_height;
        w<-0 until character_width;
        ch<-mutateCharacter(digit(h)(w))) yield{
      val newRow = digit(h).patch(w,Seq(ch),1)
      digit.patch(h,Seq(newRow),1)
    }
  }
  
  def laxMatches(digit:RenderedDigit):Seq[Char] = fuzzyDigitMap(digit)
  
  private lazy val fuzzyDigitMap:Map[RenderedDigit,Seq[Char]] = {
    case class FuzzyTuple(digit:Char,renderedDigit:RenderedDigit)
    
    val fuzzyTuples = for(d<-allRenderedDigits;
                          fuzzy<-mutateDigit(d)) yield FuzzyTuple(allRenderedDigits.indexOf(d).toString.charAt(0),fuzzy)
    fuzzyTuples.groupBy(_.renderedDigit).mapValues(x=>x.map(_.digit))
  }
 }

case class Account(accountId: String){
  val isLegible = Account.isLegible(accountId)
  
  val isValid:Boolean = Account.isValid(accountId)
  
  val tabulatedString:String  = {
    val suffix = if(!isLegible) "ILL" else if(!isValid) "ERR" else ""
    accountId + "\t"+ suffix
  }
}

object Account {
  def parse(r:RenderedAccountNumber) :Either[String,Account] = Numbers.parseLine(r).right.map{x=>
    Account(x)
  }
  
  private def isLegible(accountId:String) = accountId.size == Numbers.characters_per_line && !accountId.contains('?')
  private def isValid(accountId:String) = {
    if(!isLegible(accountId)) false
    else{
      val checksum:Int = accountId.toCharArray.zip(1.to(9).reverse).map{
        case (char,pos) =>
          char.asDigit*pos
      }.sum
      checksum % 11 == 0
    }
  }
}

