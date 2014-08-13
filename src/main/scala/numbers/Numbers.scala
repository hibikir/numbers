package numbers

import scala.io.Source

object Numbers {

  type RenderedDigit = Seq[String]
  type RenderedStream = Seq[String]
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
  
  def parseDigit(r:RenderedDigit) :Char = {
    allRenderedDigits.indexOf(r) match {
      case -1 => 
        println(r.mkString("\n")+" did not match any numbers")
        '?'
      case n:Int => n.toString.charAt(0)
    }
  }

  private def splitByCharacter(row:String):List[String] = row.grouped(character_width).toList

  private def toRenderedDigits(s:RenderedStream) :Seq[RenderedDigit] = {
     s.map(splitByCharacter).transpose
  }
  
  def validateRecordLength(s:RenderedStream):Either[String,RenderedStream] = {
    s.find(_.size!=character_width * characters_per_line) match {
      case Some(x) => Left("Line of inappropriate size\n"+x)
      case _ => Right(s)
    }
  }
  
  def parseLine(r:RenderedStream) :String = 
    validateRecordLength(r) match {
      case Left(x) => println(x); ""
      case Right(x) => parseValidLine(x)
    }


  private def parseValidLine(r:RenderedStream) :String = {
    val chunkedLine = toRenderedDigits(r)
    chunkedLine.map(parseDigit).mkString
  }
  
  //def renderDigit(c:Char):Rendered = if(c.isDigit) renderedNumbers(c.asDigit) else blank
  //def renderString(s:String) = s.toCharArray.toList.map(c=>renderDigit(c)).transpose.map(xs=>xs.mkString("")).mkString("\n")
}

case class Account(accountId: String){
  val allCharactersValid = !accountId.contains('?')
  val hasValidChecksum:Boolean = {
    if(!allCharactersValid) false
    else{
      true
    }
  }
  val isValid = hasValidChecksum
}

object FileParser{
  def parse(source:Source):Seq[Account] = {
    val lines = source.getLines()
    lines.grouped(4).map{xs => Account(Numbers.parseLine(xs.take(3)))}.toList
  }
}