object StringToNumber extends App {
    val start = 'a'.toInt - 1
    val end = 'z'.toInt
    def alphabetPosition(text: String): String =
        text
            .map(c=> c.toLower)
            .filter(c=> c >= start && c<=end)
            .map(c => (c.toInt - start).toString)
            .reduceOption((c:String,b:String)=>s"$c $b").getOrElse("")

    println(alphabetPosition("!"))
}
