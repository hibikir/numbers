package numbers

import numbers.Numbers.{RenderedDigit, RenderedAccountNumber}

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

  def parseLine(r:RenderedAccountNumber) :Either[String,Seq[RenderedDigit]] =
    validateRecordLength(r).right.map(x=>toRenderedDigits(x))

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
  def laxMatches(char:Char):Seq[Char] = fuzzyDigitMap(allRenderedDigits(char.asDigit))

  private lazy val fuzzyDigitMap:Map[RenderedDigit,Seq[Char]] = {
    case class FuzzyTuple(digit:Char,renderedDigit:RenderedDigit)
    
    val fuzzyTuples = for(d<-allRenderedDigits;
                          fuzzy<-mutateDigit(d)) yield FuzzyTuple(allRenderedDigits.indexOf(d).toString.charAt(0),fuzzy)
    fuzzyTuples.groupBy(_.renderedDigit).mapValues(x=>x.map(_.digit)).withDefaultValue(Seq())
  }
 }

case class AccountId(id:String){
  
  val isLegible = id.size == Numbers.characters_per_line && !id.contains('?')
  val mightBecomeLegible = id.size == Numbers.characters_per_line && id.count(_=='?')<=1
  
  val isValid = {
    if(!isLegible) false
    else{
      val checksum:Int = id.toCharArray.zip(1.to(9).reverse).map{
        case (char,pos) =>
          char.asDigit*pos
      }.sum
      checksum % 11 == 0
    }
  }
  
  def patch(idx:Int, c:Char) = new AccountId(id.patch(idx,Seq(c),1))

  def findSimilarAccounts():Seq[AccountId] =
    for(i<-0.until(id.length);
        d<-Numbers.laxMatches(id(i));
        acc<-Seq(patch(i,d)))
    yield acc
}

case object AccountId{
  implicit def String2AccountId(id:String) = AccountId(id)
}

class Account private(val accountId: AccountId,val digits:Seq[RenderedDigit],val alternatives:Seq[AccountId]){
  val isLegible = accountId.isLegible
  
  val isValid:Boolean = accountId.isValid
  
  val tabulatedString:String  = {
    val suffix = if(alternatives.nonEmpty) "AMB " +alternatives.mkString(" ")
                else if(!isLegible) "ILL" 
                else if(!isValid) "ERR" else ""
    accountId.id + "\t"+ suffix
  }
}

object Account {

  def apply(original: AccountId,digits:Seq[RenderedDigit],alternatives:Seq[AccountId] = Seq()) :Account = {
    if(alternatives.isEmpty) new Account(original,digits,Seq())
    else if(alternatives.size==1) new Account(alternatives.head,digits,Seq())
    else new Account(original,digits,alternatives)
  }
  
  def parse(r:RenderedAccountNumber) :Either[String,Account] = Numbers.parseLine(r).right.map{x=>
    
    val accountId:AccountId = x.map(Numbers.parseDigit).mkString
    
    if (accountId.isValid) Account(accountId,x) 
    else if(accountId.isLegible){
      Account(accountId,x,accountId.findSimilarAccounts().filter(_.isValid))
    }
    else if(accountId.mightBecomeLegible){
      val idx = accountId.id.indexOf('?')
      val alternates = Numbers.laxMatches(x(idx)).map(c=>accountId.patch(idx,c)).filter(_.isValid)
      Account(accountId,x,alternates)  
    }
    else Account(accountId,x)
  }
  
}

