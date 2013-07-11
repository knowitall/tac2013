package edu.knowitall.tac2013.app

object TipsterData {
  
  private val tipsterResourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/TipsterGazetteer.txt"
  
  private val tipsterURL = getClass().getResource(tipsterResourcePath)
  require(tipsterURL != null, "Could not find resource: " + tipsterResourcePath)

    
  val citySet =  scala.collection.mutable.Set[String]()
  val stateOrProvinceSet = scala.collection.mutable.Set[String]()
  val countrySet = scala.collection.mutable.Set[String]()

  // read in tipster lines with latin encoding so as not to get errors.
  scala.io.Source.fromFile(tipsterURL.getPath())(scala.io.Codec.ISO8859).getLines.foreach(line => {
      val pairs = line.split("\\)")
      val pairSplits = { for(p <- pairs) yield p.split("\\(")}
      for(nameAndLocationType <- pairSplits){
        if(nameAndLocationType.size ==2){
	        val name = nameAndLocationType(0).trim().toLowerCase()
	        val locationType = nameAndLocationType(1).split(" ")(0).trim()	        
	        locationType match {
	          case "CITY" => { if(!citySet.contains(name)) {
	        	  					citySet.add(name)
	          				}
	          }
	          case "COUNTRY" => { if(!countrySet.contains(name)) {
	        	  					countrySet.add(name)
	        	  				}
	          }
	          case "PROVINCE" => { if(!stateOrProvinceSet.contains(name)){
	        	  				stateOrProvinceSet.add(name)
	          					}
	          }
	          case _ => {}
	        }
        }
      }
      
      
    })
  
  
  lazy val cities = citySet.toSet
  lazy val countries = countrySet.toSet
  lazy val stateOrProvinces = stateOrProvinceSet.toSet
  
  
}