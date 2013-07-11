package edu.knowitall.tac2013.findSlotFillersApp

case class NellData(val name: String, val cityProbability: Option[Double],
    val stateOrProvinceProbability: Option[Double], val countryProbability: Option[Double]){
}


object NellData {
  
  private val nellResourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/caseInsensitiveNellLocations"
  
  private val nellURL = getClass().getResource(nellResourcePath)
  require(nellURL != null, "Could not find resource: " + nellResourcePath)

    
  val citySet =  scala.collection.mutable.Set[NellData]()
  val stateOrProvinceSet = scala.collection.mutable.Set[NellData]()
  val countrySet = scala.collection.mutable.Set[NellData]()
  private var nellMap = Map[String,NellData]()
  

  // read in tipster lines with latin encoding so as not to get errors.
  scala.io.Source.fromFile(nellURL.getPath())(scala.io.Codec.UTF8).getLines.foreach(line => {
      val lineData = line.split("\t")
      val name = lineData(0).toLowerCase().trim()
      val typeProbabilityPairs = lineData.tail
      
      //construct an instance of NELL Data
      var cityProbability: Option[Double] = None
      var countryProbability: Option[Double] = None
      var stateOrProvinceProbability: Option[Double] = None
      
      
      //read semantic types
      for(tpPair <- typeProbabilityPairs){
        val tpPairData = tpPair.split(" ")
        val semanticTypeName = tpPairData(0).trim().toLowerCase()
        val probability = tpPairData(1).toDouble
        
        if(semanticTypeName == "city"){
          cityProbability = Some(probability)
        }
        else if(semanticTypeName == "country"){
          countryProbability = Some(probability)          
        }
        else if(semanticTypeName == "stateorprovince"){
          stateOrProvinceProbability = Some(probability)          
        }
        else{
          
        }
        
      }
      
      
      val nellData = new NellData(name,cityProbability,stateOrProvinceProbability,countryProbability)
      nellMap = nellMap + (nellData.name -> nellData)
      
      if(nellData.cityProbability.isDefined){
        citySet.add(nellData)
      }
      if(nellData.stateOrProvinceProbability.isDefined){
        stateOrProvinceSet.add(nellData)
      }
      if(nellData.countryProbability.isDefined){
        countrySet.add(nellData)
      }
      
      
    })
  
  
  lazy val cities = citySet.toSet
  lazy val countries = countrySet.toSet
  lazy val stateOrProvinces = stateOrProvinceSet.toSet
  
  lazy val cityNameSet = cities.map(f => f.name)
  lazy val countryNameSet = countries.map(f => f.name)
  lazy val stateOrProvinceNameSet = stateOrProvinces.map(f => f.name)
  
  def getNellData(str: String): Option[NellData] = {
    if(nellMap.contains(str)) Some(nellMap(str)) else None
  }

  
  
}