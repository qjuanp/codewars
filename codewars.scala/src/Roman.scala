object Roman extends App {

  class Pipe(
              val roman:String,
              val residue: Int
            ) {
      def apply(roman:String, residue:Int):Pipe = new Pipe(this.roman + roman, residue)
    }

  val M: Int = 1000
  val D: Int = 500
  val C: Int = 100
  val L: Int = 50
  val X: Int = 10
  val V: Int = 5
  val I: Int = 1

  def upperPart(number:Int, part:Int): Int = number - (number%part)
  def isPrePart(number:Int, part:Int, nextPart: Int): Boolean =
    ((number/part)*upperPart(number,nextPart)%part)==(4*nextPart)

  def isDownPart(number:Int, part:Int, nextPart: Int): Boolean =
    (upperPart(number,nextPart)%part)==(4*nextPart)

  val encodeM = (pipe:Pipe) =>
    pipe("M" * (pipe.residue/M), pipe.residue % M)

  val encodeD = (pipe:Pipe)=>
    if(isPrePart(pipe.residue, D, C))
      pipe("CM", pipe.residue % D % C)
    else if (isDownPart(pipe.residue, D, C))
      pipe("CD", pipe.residue % D % C)
    else
      pipe("D" * (pipe.residue/D), pipe.residue % D)

  val encodeC = (pipe:Pipe)=>
    pipe("C" * (pipe.residue/C), pipe.residue % C)

  val encodeL = (pipe:Pipe)=>
    if(isPrePart(pipe.residue, L, X))
      pipe("XC", pipe.residue % L % X)
    else if (isDownPart(pipe.residue, L, X))
      pipe("XL", pipe.residue % L % X)
    else
      pipe("L" * (pipe.residue/L), pipe.residue % L)

  val encodeX = (pipe:Pipe)=>
    pipe("X" * (pipe.residue/X), pipe.residue % X)

  val encodeV = (pipe:Pipe)=>
    if(isPrePart(pipe.residue, V, I))
      pipe("IX", pipe.residue % V % I)
    else if (isDownPart(pipe.residue, V, I))
      pipe("IV", pipe.residue % V % I)
    else
      pipe("V" * (pipe.residue/V), pipe.residue % V)

  val encodeI = (pipe:Pipe)=>
    pipe("I" * (pipe.residue/I), pipe.residue % I)


  val results = (pipe:Pipe)=> pipe.roman

  def encode(arabic: Int): String =
   (
        encodeM andThen
        encodeD andThen
        encodeC andThen
        encodeL andThen
        encodeX andThen
        encodeV andThen
        encodeI andThen
        results
      )(new Pipe("",arabic))
}