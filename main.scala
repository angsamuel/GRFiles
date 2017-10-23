import scala.io.Source
import java.io._
import java.io.File
import scala.collection.mutable.ListBuffer
import System.nanoTime
import sys.process._
import scala.util.control.Breaks._
import collection.mutable.HashMap
import scala.util.Random
//6.39
//7.2958

//morning star venus
var targetMap = HashMap[String, Int]()
//scala main.scala <inputFileName> <lpFileName> <outputFilename>
//Generate Board
//Solve
//Compare Outcome with that of non-stationary method
var size = 0;
var moveCost = 0;
var predictionCost = 0;
var attackerStart = "(0,0)"
var targets = new ListBuffer[Target]()
var attackMap = HashMap[(String, String), ListBuffer[(String, Double)]]()
class Target(posX:Integer, posY:Integer, probability:Double){
	val x = posX
	val y = posY
	val p = probability
	def GetPos(): String = {
		"(" + x.toString + "," + y.toString + ")"
	}
}


GrabValues(args(0));
GenerateLP(args(1));
RunSolver();
//GenerateAttackerStrategy()
//BuildAttackerMap()
//BuildDefenderMap()

if(args(3) == "--multiple"){
	var totalScore = 0.0
	var totalScoreDefault = 0.0
	for(i <- 0 to args(5).toInt){
		var playedGame = PlayGame(false)
		totalScore += playedGame._1
		totalScoreDefault += playedGame._2
	}
	println("Best Response Score: " + totalScore/args(5).toInt)
	println("Stationary Score: " + totalScoreDefault/args(5).toInt)
	println(targetMap)
}else if(args(3) == "--single"){
	PlayGame(true)
}
def PrintValues(){
	println("size: " + size)
	println("moveCost: " + moveCost)
	println("predictionCost: " + predictionCost)
	println("targets:" + targets)
}
 //grabs relevent data from input file
def GrabValues(inputFile:String){
	for(line <- Source.fromFile("./files/" + inputFile).getLines()){
		var lineArray = line.split(" ")
		lineArray(0) match {
			case "size:" => size = lineArray(1).toInt
			case "moveCost:" => moveCost = lineArray(1).toInt
			case "predictionCost:" => predictionCost = lineArray(1).toInt
			//println(lineArray(1))
			case "attackerStart:" =>attackerStart = lineArray(1)
			case "target:" => targets+= new Target(lineArray(1).split(",")(0).toInt, lineArray(1).split(",")(1).toInt, lineArray(2).toDouble)
			case _ => 
		}
	}
}

//generates a MI lp
def GenerateLP(lpFile:String){
 var lpWriter = new PrintWriter(new File("./files/" + lpFile))
 //maximize
 lpWriter.write("maximize\n")
 for(i <- 0 until targets.length){
		var m = targets(i).p.toString + "V" + targets(i).GetPos() + ";(" + attackerStart + ")"
		if(i < targets.length - 1){
			m += " + "
		}
		lpWriter.write(m)
	}
	lpWriter.write("\n")
	lpWriter.write("subject to\n")

	for(y <- 0 until size){
		for(x <- 0 until size){
			for(to <- targets){
				if(MatchTarget(to, x, y)){
					lpWriter.write(MakeTerm("V", to, x, y));
					lpWriter.write(" = 0\n")
				}
			}
			if(x > 0){
				WriteLines(x,y,x-1,y,lpWriter);
			}
			if(x < size-1){
				WriteLines(x,y,x+1,y,lpWriter);
			}
			if(y > 0){
				WriteLines(x,y,x,y-1,lpWriter);
			}
			if(y < size-1){
				WriteLines(x,y,x,y+1,lpWriter);
			}
		}
	}

	//limit probabilities to 100%
	for(y <- 0 until size){
		for(x <- 0 until size){
			var index = 0
			for(t <- targets){
				index = index + 1
				lpWriter.write(MakeTerm("F",t,x,y))
				if(index < targets.length){
					lpWriter.write(" + ")
				} 
			}
			lpWriter.write(" = 1\n")
		}
	}


	lpWriter.write("bounds\n")

	for(y <- 0 until size){
		for(x <- 0 until size){
			for(t <- targets){
				lpWriter.write(MakeTerm("F",t,x,y) + " >= 0\n")
			}
		}
	}

	for(y <- 0 until size){
		for(x <- 0 until size){
			for(t <- targets){
				lpWriter.write(MakeTerm("V",t,x,y) + " free\n")
			}
		}
	}
	lpWriter.write("integer\n")

	lpWriter.write("end")
	lpWriter.close()
}

def WriteLines(x:Integer, y:Integer, xn:Integer, yn:Integer, lpFile:PrintWriter){
	for(to <- targets){
		if(!MatchTarget(to, x, y)){
		//now w're looking at the F variables
			lpFile.write(MakeTerm("V", to, x, y));
			for(t <- targets){
				var coef = moveCost;
					if(MatchTarget(t, to)){
						coef += predictionCost;

					}
					if(MatchTarget(to, xn, yn)){
						coef -= 0;
					}
					if(coef > 0){
						lpFile.write(" - ")
					}else{
						coef = Math.abs(coef)
						lpFile.write(" + ")
					}
					lpFile.write(coef.toString + MakeTerm("F", t, x, y)) 
			}
			lpFile.write(" - " + MakeTerm("V", to, xn, yn) + " <= 0\n");
		}
	}
}


def MatchTarget(target:Target, x:Integer, y:Integer):Boolean = {
	(x == target.x && y == target.y)
}

def MatchTarget(target:Target, target2:Target):Boolean = {
	(target2.x == target.x && target2.y == target.y)
}

def MakeTerm(letter:String,target:Target,x:Integer,y:Integer):String = {
	letter + "(" + target.x.toString + "," + target.y.toString + ");(" + x.toString + "," + y.toString + ")"
}

def RunSolver(){
	if(args(4) == "-s"){
		var lpFilePath = "./files/" + args(1)
		var outputFilePath = "./files/" + args(2)
		var command = "glpsol --cpxlp " + lpFilePath + " --write " + outputFilePath + ""
		outputFilePath = "./files/ez" + args(2)
		var ezcommand = "glpsol --cpxlp " + lpFilePath + " -o " + outputFilePath + ""
		(command).!
		(ezcommand).!
	}else if(args(4) == "-i"){
	var lpFilePath = "./files/" + args(1)
		var outputFilePath = "./files/" + args(2)
		var command = "glpsol --cpxlp --interior " + lpFilePath + " --write " + outputFilePath + ""
		outputFilePath = "./files/ez" + args(2)
		var ezcommand = "glpsol --cpxlp --interior " + lpFilePath + " -o " + outputFilePath + ""
		(command).!
		(ezcommand).!
	}
}

def BuildAttackerMap(){
	//					 target position              move   liklihood 
	attackMap = HashMap[(String, String), ListBuffer[(String, Double)]]()
	//get list of attacker values
	val solvedLines = Source.fromFile("files/" + args(2)).getLines().toList.dropRight(2).drop(8)
	var attackerValues = List[Double]() //solvedLines.map(_.split(" ")(3).toDouble)
	if(args(4) == "-i"){
		attackerValues = solvedLines.map(_.split(" ")(3).toDouble)
	}else if(args(4) == "-s"){
		attackerValues = solvedLines.map(_.split(" ")(4).toDouble)
	}

	val lpLines = Source.fromFile("files/" + args(1)).getLines().toList.drop(3).filter(_.charAt(0) == 'V').filter(_.split(" ")(1) != "free")
/*
	for(line <- lpLines){
		println(line)
	}*/
	var lpLineTracker = 0
	for(lpLine <- lpLines){
		//println(lpLine)
		var lpList = lpLine.split(" ")
		if(lpList.size > 3){ //grab values and put them in the map
			var positionString = lpList(0).replace("V","").split(";")(1)

			var targetString = lpList(0).replace("V","").split(";")(0)

			var moveString = lpList(lpList.size - 3).replace("V","").split(";")(1)

			if(attackMap isDefinedAt (targetString, positionString)){
				attackMap((targetString,positionString)) += ((moveString, attackerValues(lpLineTracker)))
			}else{
				attackMap((targetString, positionString)) = ListBuffer((moveString, attackerValues(lpLineTracker)))
			}
			//println(position)
		}
		lpLineTracker+=1

	}
}

def GenerateAttackerStrategy():ListBuffer[String] = {
	BuildAttackerMap()
	var attackerPos = "(" + attackerStart + ")"
	//decide strategy
	var chosenTarget = "(" + targets(0).x + "," + targets(0).y + ")"
	var tselection = Random.nextDouble
	var tobservedProbability = 0.0
	for(t <- targets){ //assign target for attacker
		tobservedProbability += t.p
		if(tselection<=tobservedProbability){
			chosenTarget ="(" + t.x + "," + t.y + ")"
			tselection = 2
		}
	}
	if(targetMap isDefinedAt chosenTarget){
		targetMap(chosenTarget)+=1
	}else{
		targetMap(chosenTarget) = 1
	}

	var attackerStrategy = ListBuffer[String]()
	attackerStrategy += attackerPos


	var newAttackerPos = attackerPos

	while(chosenTarget != attackerPos){

		var totalprob = 0.0
		for(move <- attackMap((chosenTarget,attackerPos))){
			totalprob += move._2
		}

		var maxMoveWeight = 0.0
		//Thread.sleep(500)
		//println((chosenTarget, attackerPos) + " -> " + attackMap((chosenTarget,attackerPos)))
		var selection = Random.nextDouble * totalprob

		var observedProbability = 0.0
		//println()
		//println("selection: " + selection)
		//println("observedProbability: " + observedProbability)

		for(move <- attackMap((chosenTarget,attackerPos))){
			observedProbability += move._2
			//println("move._2: " + move._2)
			//println("observedProbability: " + observedProbability)
			if(selection <= observedProbability){
				maxMoveWeight = move._2
				newAttackerPos = move._1
				selection = 2
			}
		}
		attackerStrategy+=newAttackerPos
		attackerPos = newAttackerPos

		if(attackerStrategy.size > 500){
			println("warning, potential looping strategy")
			println(attackerStrategy)
			println(chosenTarget)
			println()
			System.exit(1)
		}
		//println(chosenTarget + " " + attackerPos)
	}
	//println(attackerStrategy)
	attackerStrategy+=chosenTarget
	attackerStrategy
}

def BuildTrackerMap():HashMap[String, Double] = {
	var trackerMap = HashMap[String, Double]()
	for(t <- targets){
		trackerMap(t.GetPos) = 1.0
	}
	trackerMap
}

def BuildDefenderMap():HashMap[String, ListBuffer[(String, Double)]] = {
						//pos                  target weight
	var defMap = HashMap[String, ListBuffer[(String, Double)]]()

	val outputLines = Source.fromFile("files/ez" + args(2)).getLines().toList
	var dashCount = 0
	for(line <- outputLines){
		var fValue = ""
		var activity = 0.0
		val splitLine = line.split(" ").filter(a=> a.size > 0)
		if(splitLine.size > 0){
			if(splitLine(0) == "------"){
				//println("found dash")
				dashCount+=1
			}else if(dashCount > 1){
				var collectValue = false
				var onST = false
				if(splitLine.size > 4){
					if(splitLine(1).take(1)=="F"){					
						if(splitLine(3) != "<"){
							fValue = splitLine(1) 
							if(args(4) == "-s"){
								activity = splitLine(3).toDouble
							}else if(args(4) == "-i"){
								activity = splitLine(2).toDouble
							}
						}
					}
				}
			}
		}
		if(fValue != ""){
			//add to defMap
			var target = fValue.replace("F","").split(";")(0)
			var position = fValue.replace("F","").split(";")(1)
			//println(position + "," + target)
			if(defMap isDefinedAt position){
				defMap(position) += ((target, activity))
			}else{
				defMap(position) = ListBuffer((target, activity))
			}
		}
	}
	defMap
}

def PlayGame(printInfo: Boolean):(Double,Double) = {
	println("\n\n\n\n\n\n\n\n\n\n\n")
	var attackerStrat = GenerateAttackerStrategy()
	//println(attackerStrat)
	val defenderMap = BuildDefenderMap()
	var defenderScore = 0.0
	var defenderScoreDefault = 0.0
	var guessesnum = 0
	var chosenTarget = attackerStrat(attackerStrat.size-1)
	attackerStrat = attackerStrat.dropRight(1)
	var BestResponseGuesses = new ListBuffer[String]()
	var StationaryGuesses = new ListBuffer[String]()
	var weightTracker = BuildTrackerMap()


	for(i <- 0 until attackerStrat.size-1){
		var maxValue = -10.0
		var maxGuess = "[NONE]"
		var maxValueDefault = -10.0
		var maxGuessDefault = "[NONE]"
		var attackerMoveInfo = new ListBuffer[(String, Double)]()
		if(i==0){
			//find max in defender strat
			if(printInfo){
				println("Attacker Move " + i + ":")
				println("Attacker Location:" + attackerStrat(i))
				println("Unaltered Defender Map: " + defenderMap(attackerStrat(i)))
			}
			for(option <- defenderMap(attackerStrat(i))){

				if(option._2 > maxValue){
					maxValue = option._2
					maxGuess = option._1

					maxValueDefault = option._2
					maxGuessDefault = option._1
				}
			}
			if(printInfo){
				println("	Stationary Defender Guess: " + maxGuess + " with " + maxValue)
				println("	Best Response Defender Guess: " + maxGuessDefault + " with " + maxValueDefault)
				println()
			}
		}else{
			if(printInfo){
				println("Attacker Move " + i + ":")
				println("Attacker Location: " + ":" + attackerStrat(i))
				println("Unaltered Defender Map: " + defenderMap(attackerStrat(i)))
				println("Attacker Move Map: (target, position) -> List(new position, liklihood)")
				for(t <- targets){
					if(attackMap isDefinedAt (t.GetPos(), attackerStrat(i-1)) ){
						println("	target: " + t.GetPos() + ", position " + attackerStrat(i-1) +": " + attackMap(t.GetPos(),attackerStrat(i-1)))
					}
				}
			}
					  //target //double
		    //					     target position              move   liklihood 
			attackerMoveInfo = new ListBuffer[(String, Double)]() //just for keeping track
			for(option <- defenderMap(attackerStrat(i))){
				//grab attacker liklihood of making move
				attackerMoveInfo += option
				//give target, position, get back a list of moves and their liklihoods
					if(option._1 != attackerStrat(i-1)){
						for(move <- attackMap(option._1,attackerStrat(i-1))){
							if(move._1 == attackerStrat(i)){
								weightTracker(option._1) = weightTracker(option._1)*move._2*10

								if(weightTracker(option._1) * option._2 > maxValue){ //used to be move._2
									//println("EXCUSE ME: " + move._2 + "*" + option._2 + " = " + (move._2 * option._2))
									//println("--" + option + " " + defenderMap(attackerStrat(i)))
									maxValue = move._2 * option._2
									maxGuess = option._1
								}
							}
						}
					}	
				//normalize le map
				var sum = 0.0	
				for(t <- targets){
					sum += weightTracker(t.GetPos())
				}
				for(t <- targets){
					weightTracker(t.GetPos()) = weightTracker(t.GetPos()) / sum
				}

				if(option._2 > maxValueDefault){
					maxValueDefault = option._2
					maxGuessDefault = option._1
				}

				if(maxValue == 0){
					maxGuess = maxGuessDefault; //stop accidental best choices
				}

			}
			if(printInfo){
				println("Attacker Move Info: " + attackerMoveInfo)
				println("-----------------------------------------")
				println("	Best Response Defender Guess: " + maxGuess + " with " + maxValue)
				println("	Stationary Defender Guess: " + maxGuessDefault + " with " + maxValueDefault)
				println()
			}

			
		}
			BestResponseGuesses += (maxGuess)
			StationaryGuesses += (maxGuessDefault)

		//score
		guessesnum += 1
		if(maxGuess == attackerStrat(attackerStrat.size-1)){
			defenderScore+=predictionCost
		}
		if(maxGuessDefault == attackerStrat(attackerStrat.size-1)){
			defenderScoreDefault+=predictionCost
		}
	}
	//println(defenderScore)
	if(printInfo){
		println("----------------RESULTS----------------")
		println()
		println("Target: " + chosenTarget)
		println("Attacker Strategy: " + attackerStrat)
		println()
		println("Best Response Defender Score " + defenderScore)
		println("Best Response Guesses: " + BestResponseGuesses)
		println()
		println("Stationary Defender: " + defenderScoreDefault)
		println("Stationary Guesses: " + StationaryGuesses )
		println()
		println("----------complete----------")
		println()
		println( "attack map at (8,6) (2,8) " + attackMap(("(8,6)", "(2,8)")))
		println(weightTracker)
	}

	(defenderScore,defenderScoreDefault)
}

/*
for default strategy, 
simulate an action from it
rewrite selection
*/