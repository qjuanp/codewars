object Roman extends App {

  class Pipe(
              val roman:String,
              val residue: Int,
              val next: (Pipe) => String
            )

  val M: Int = 1000
  val D: Int = 500
  val C: Int = 100
  val L: Int = 50
  val X: Int = 10
  val V: Int = 5
  val I: Int = 1

  def numpart(arabic: Int, part: Int):Int = arabic - (arabic%part)

  def prePart(part: Int) : Int =
    part match {
      case M => M-C
      case D => D-C
      case C => C-X
      case L => L-X
      case X => X-I
      case V => V-I
    }

  def isPrePart(number:Int, part:Int, nextPart: Int): Boolean =
    (number)%(part)==(4*nextPart)



  def encodeM(pipe: Pipe): String = pipe.next(new Pipe("M" * (pipe.residue/M), pipe.residue%M, encodeC))
  def encodeD(pipe: Pipe): String =
    if(isPrePart(pipe.residue,D,C))
      pipe.next(new Pipe(pipe.roman + "CM", pipe.residue%D%C, encodeL))
    else
      pipe.next(new Pipe(pipe.roman + "D" * (pipe.residue/D), pipe.residue%D, encodeL))
  def encodeC(pipe: Pipe): String = pipe.next(new Pipe(pipe.roman + "C" * (pipe.residue/C), pipe.residue%C, encodeX))
  def encodeL(pipe: Pipe): String = pipe.next(new Pipe(pipe.roman + "L" * (pipe.residue/L), pipe.residue%L, encodeV))
  def encodeX(pipe: Pipe): String = pipe.next(new Pipe(pipe.roman + "X" * (pipe.residue/X), pipe.residue%X, encodeI))
  def encodeV(pipe: Pipe): String = pipe.next(new Pipe(pipe.roman + "V" * (pipe.residue/V), pipe.residue%V, done))
  def encodeI(pipe: Pipe): String = pipe.next(new Pipe(pipe.roman + "I" * (pipe.residue/I), pipe.residue%I, (pipe:Pipe)=>{""}))
  def done(pipe: Pipe): String = pipe.roman

  def encodeFinal(arabic: Int): String = s"${arabic}"

  def encode(arabic: Int): String = encodeM(new Pipe("", arabic, encodeD))

  println(s"2564 => ${encode(2564)}")
  println(s"564 => ${encode(564)}")
  println(s"4 => ${encode(4)}")
  println(s"89 => ${encode(89)}") // "LXXXIX"
  println(s"900 => ${encode(900)}") // "CM"
  println(s"964 => ${encode(900)}") // "CM"
  println(s"90 => ${encode(90)}") // "XC"
  println(s"9 => ${encode(9)}") // "IX"
  println(s"4 => ${encode(4)}") // "IV"
}