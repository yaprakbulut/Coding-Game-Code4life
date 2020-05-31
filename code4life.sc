import math._
import scala.util._
import scala.io.StdIn._
import scala.util.control._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
**/

//Class definition for Sample
//This class has 4 variables sampleId, carriedBy, healthPoints, numberOfMolecules
class Sample(
    var sampleId: Int,                   //unique Id for the sample
    var carriedBy: Int,                  //0 if the sample is carried by you, 1 by the other robot, -1 if the sample is in the cloud.
    var healthPoints: Int,               //The number you get from this sample.
    var numberOfMolecules: Array[Int]    //an array for every cost
    ){  // String representation of sample object, return sampleId
        override def toString(): String = {  
        return ""+sampleId
    } 
    }
//Class definition for Player which is robot
class Players(
    var storage:Array[Int],        //number of molecules held by the player for each molecule type
    var target: String             // module where the player is
){}
    
object Player extends App {
    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }

    // game loop
    while(true) {
        var samples = ArrayBuffer[Sample]()         //An ArrayBuffer buffer holds sample array and size.
        var players = ArrayBuffer[Players]()        //An ArrayBuffer buffer holds players array and size.
        var molecules = ArrayBuffer[String]()       //molecules ArrayBuffer holds string molecule type
        for(i <- 0 until 2) {
            val Array(target, _eta, _score, _storageA, _storageB, _storageC, _storageD, _storageE, _expertiseA, _expertiseB, _expertiseC, _expertiseD, _expertiseE) = readLine split " "
            val eta = _eta.toInt
            val score = _score.toInt
            val storageA = _storageA.toInt
            val storageB = _storageB.toInt
            val storageC = _storageC.toInt
            val storageD = _storageD.toInt
            val storageE = _storageE.toInt
            val expertiseA = _expertiseA.toInt
            val expertiseB = _expertiseB.toInt
            val expertiseC = _expertiseC.toInt
            val expertiseD = _expertiseD.toInt
            val expertiseE = _expertiseE.toInt
            //Create new player object 
            val player = new Players(Array(storageA, storageB, storageC, storageD, storageE), target)
            players.append(player)
        }
        val Array(availableA, availableB, availableC, availableD, availableE) = (readLine split " ").map (_.toInt)
        val sampleCount = readLine.toInt
        for(i <- 0 until sampleCount) {
            val Array(_sampleId, _carriedBy, _rank, expertiseGain, _health, _costA, _costB, _costC, _costD, _costE) = readLine split " "
            val sampleId = _sampleId.toInt
            val carriedBy = _carriedBy.toInt
            val rank = _rank.toInt
            val health = _health.toInt
            val costA = _costA.toInt
            val costB = _costB.toInt
            val costC = _costC.toInt
            val costD = _costD.toInt
            val costE = _costE.toInt
            //numberOfMolecules holds costs of array
            var numberOfMolecules = Array(costA, costB, costC, costD, costE)
            val sample = new Sample(sampleId, carriedBy, health, numberOfMolecules) //create new sample object
            samples.append(sample) 
            //Add a single element(molecule type) to an ArrayBuffer
            molecules += "A"
            molecules += "B"
            molecules += "C"
            molecules += "D"
            molecules += "E"
        }
        /*
        Define gotoConnect function
        We can have the arguments where we want to go so in this case module, 
        and data which is how we want to connect, and target which is where we are.
        */
        def gotoConnect(module:String, data:String, location:String ){
            if(location == module){
                println("CONNECT " + data )
            }else{
                println("GOTO " + module)

            }
        }
      
        //Create object with name Config. There are variables with instances
      object Config{
        var myPlayer = players(0)
        var bestSample:Sample = null
        var maxHealth = 0
        }
        //samples iterate over and bestSample is found.
        for (sample <- samples) {
            if(sample.healthPoints > Config.maxHealth && sample.carriedBy != 1){
                Config.bestSample = sample
                Config.maxHealth = sample.healthPoints
            }
        }
        
      //If we are not carrying bestSample it goes to the diagnosis module and connect with Id of the bestSample.
      if(Config.bestSample.carriedBy != 0){
            gotoConnect("DIAGNOSIS", Config.bestSample.toString(), Config.myPlayer.target)        
        }else{
            var neededMolecule:String = null
          
          //Iterate over each molecule. There are 5 different types of molecule.
            breakable {                  
            for (i <- 0 until 5){         
                if(Config.myPlayer.storage(i) < Config.bestSample.numberOfMolecules(i)){
                    neededMolecule = molecules(i)
                    break            //We can break out of the loop
                }
            }
            }
          //If neededMolecule is not null, it goes to the molecules else it goes to the laboratory
            if(neededMolecule != null){
                gotoConnect("MOLECULES", neededMolecule.toString(), Config.myPlayer.target)
            }else{
                gotoConnect("LABORATORY", Config.bestSample.toString(), Config.myPlayer.target)
            }
        } 
  }
 }
  
