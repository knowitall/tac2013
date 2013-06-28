package edu.knowitall.tac2013.findSlotFillersApp

object KBPQueryExecutor {
  

    
  def main(args: Array[String]){

     assert(args.length == 2, 
         "there should be two arguments: path to KBP query Files, path for outputFile")
         
     val KBPQueryPath = args(0)
     val OutputPath = args(1)
     
     val KbpQuery = KBPQuery.parseKBPQuery(KBPQueryPath)
     println(KbpQuery.slotsToFill)
         
  }
         
     
}