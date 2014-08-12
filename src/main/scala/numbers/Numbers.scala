package numbers


object Numbers {


  type Rendered = Seq[String]
  val character_width = 3
  val spriteSheet = """ x _     _  _     _  _  _  _  _
                        x| |  | _| _||_||_ |_   ||_||_|
                        x|_|  ||_  _|  | _||_|  ||_| _|""".stripMargin('x')

  val blank:Rendered = List("   ","   ","   ")

  private def splitByCharacter(row:String) = row.grouped(character_width).toList

  val renderedNumbers:List[Rendered] = {
    val rows = spriteSheet.split("\n").toList
    rows.map(splitByCharacter).transpose
  }

  def parseDigit(r:Rendered) :Char = {
    renderedNumbers.indexOf(r) match {
      case -1 => '?'
      case n:Int => n.toString.charAt(0)
    }
  }

  def parseLine(r:Rendered) :String = {
    r.map(splitByCharacter).transpose.map(parseDigit).mkString
  }

  def renderDigit(c:Char):Rendered = if(c.isDigit) renderedNumbers(c.asDigit) else blank

  def renderString(s:String) = s.toCharArray.toList.map(c=>renderDigit(c)).transpose.map(xs=>xs.mkString("")).mkString("\n")
}
