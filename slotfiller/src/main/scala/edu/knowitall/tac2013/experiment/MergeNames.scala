package edu.knowitall.tac2013.experiment

object MergeNames {

  val names = Seq(
    "Steve",
    "Steve A.",
    "Steve A. Ballmer",
    "Steve Ballmer",
    "S. Ballmer",
    "Ballmer",
    
    "Bill",
    "Bill H.",
    "Bill H. Gates",
    "Bill Gates",
    "Gates",
    
    "William H. Gates",
    "William Henry Gates",
    "W. Henry Gates",
    
    "Steve Jobs",
    "Jobs",
    "Steve P. Jobs"
    
  )
  
  def main(args: Array[String]): Unit = {
    
    // map every token in a name to the name
    val tokenNamePairs = names.flatMap { name =>
      val tokens = name.split(" ")
      tokens.map(t => (t, name))
    }
    
    val tokenMap = tokenNamePairs.groupBy { case (token, name) => token } map { case (token, tokenNames) => (token, tokenNames.map(_._2)) }
    
    val tokenList = tokenMap.toSeq.sortBy(-_._2.size)
    
    tokenList foreach { case (token, names) =>
      val tokenSpaces = token.replaceAll(".", " ")
      println(token + "\t" + names.head)
      names.tail foreach { name =>
        println(tokenSpaces + "\t" + name)
      }
      println()
    }
  }
  
}