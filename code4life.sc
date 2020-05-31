import math._
import scala.util._
import scala.io.StdIn._
import scala.util.control._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
**/

class Sample(
    var sampleId: Int, 
    var carriedBy: Int, 
    var healthPoints: Int,
    var numberOfMolecules: Array[Int]
    ){
        override def toString(): String = { 
        return ""+sampleId
    } 
    }
class Players(
    var storage:Array[Int],
    var target: String
){}
    
object Player extends App {
    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }

    // game loop
    while(true) {
        var samples = ArrayBuffer[Sample]()
        var players = ArrayBuffer[Players]()
        var molecules = ArrayBuffer[String]()
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
            var numberOfMolecules = Array(costA, costB, costC, costD, costE)
            val sample = new Sample(sampleId, carriedBy, health, numberOfMolecules)
            samples.append(sample)
            molecules += "A"
            molecules += "B"
            molecules += "C"
            molecules += "D"
            molecules += "E"
        }
        def gotoConnect(module:String, data:String, location:String ){
            if(location == module){
                println("CONNECT " + data )
            }else{
                println("GOTO " + module)

            }
        }
      
      object Config{
        var myPlayer = players(0)
        var bestSample:Sample = null
        var maxHealth = 0
        }
        for (sample <- samples) {
            if(sample.healthPoints > Config.maxHealth && sample.carriedBy != 1){
                Config.bestSample = sample
                Config.maxHealth = sample.healthPoints
            }
        }
        if(Config.bestSample.carriedBy != 0){
            gotoConnect("DIAGNOSIS", Config.bestSample.toString(), Config.myPlayer.target)        
        }else{
            var neededMolecule:String = null
            breakable { 
            for (i <- 0 until 5){
                if(Config.myPlayer.storage(i) < Config.bestSample.numberOfMolecules(i)){
                    neededMolecule = molecules(i)
                    break
                }
            }
            }
            if(neededMolecule != null){
                gotoConnect("MOLECULES", neededMolecule.toString(), Config.myPlayer.target)
            }else{
                gotoConnect("LABORATORY", Config.bestSample.toString(), Config.myPlayer.target)
            }
        } 
  }
 }
  
