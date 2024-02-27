file://<WORKSPACE>/Drill.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

action parameters:
offset: 2806
uri: file://<WORKSPACE>/Drill.scala
text:
```scala
//> using scala "3.2.0"
//> using jar "stainless-library_2.13-0.9.8.1.jar"
//> using options "-Wconf:msg=pattern.*?specialized:s,msg=not.*?exhaustive:s"

import stainless.lang._
import stainless.collection._
import stainless.annotation._
 
object drill {
  val drillHeight: BigInt = 10
  val drillLength: BigInt = 5
  val downSpeed: BigInt = 3
  val drillSpeed: BigInt = 3
  val retractSpeed: BigInt = 3
  val upSpeed: BigInt = 3

  abstract sealed class Instruction
  case class Launch() extends Instruction
  case class Pause() extends  Instruction
  case class Resume() extends Instruction
  case class Abort() extends Instruction 
  case class Motor() extends Instruction

  abstract sealed class State
  case class Idle() extends State
  case class Down() extends State
  case class Drill() extends State
  case class Retract() extends State
  case class Up() extends State

  case class DrillState(
    currState: State,
    currHeight:BigInt,
    currDeployment: BigInt,
    targetHeight:BigInt,
    targetDeploymnet:BigInt,
    waiting:Boolean,
    motor_active:Boolean,
    aborted:Boolean
  ){
    

    //R01
    require((currState == Down()) ==> (currDeployment == 0 && currHeight == drillHeight))

    //R02
    require(!(aborted && waiting))

    //R03 
    require(targetDeploymnet >= 0 && targetDeploymnet <= drillLength)
    require(currDeployment >= 0 && currDeployment <= drillLength)
    
    //R04
    require(targetHeight >= 0 && targetHeight <= drillHeight)
    require(currHeight >= targetHeight && currHeight <= drillHeight)

    //R05
    require(((currState == Down() && currHeight == targetHeight) 
 (currState == Drill() && currDeployment == targetDeploymnet) 
 (currState == Retract() && currDeployment == 0) 
 (currState == Up() && currHeight == drillHeight))
    ==> !motor_active)

    //R06
    require(!(motor_active && waiting))

    //R07
    require(!(currState == Idle() && motor_active))

    //R08
    require((!(currState == Idle() || waiting 
 (currState == Down() && currHeight != targetHeight)
 (currState == Drill() && currDeployment != targetDeploymnet)
 (currState == Retract() && currDeployment != 0)
 (currState == Up() && currHeight != drillHeight)))
          ==> motor_active)
  }

    
    // TODO : there exists a path in a finite number of steps from Idle to itself
  def loop(s: DrillState, nb: BigInt, ins: List[Instruction]): (DrillState, BigInt) = {
    val v = action(s, ins.head)
    loop(v, nb + 1, ins.tail)
    (v.get, nb+1)
  }.ensuring((s, n) => s.currState == Idle() && n <= 5)

  def existPath(): Unit={
    val arr: List[Instruction] = List(Launch(), Launch(), Launch(), Launch(), Launch())
    val sys: DrillState = DrillState(Idle(),drillHeight, 0, 0, 5, fals,@@)
    val (s, n) = loop()
    assert()
  }
  /**/
  def isInitial(s:DrillState): Boolean = {
    s.currHeight == drillHeight && s.currDeployment == 0
    && s.currState == Idle()
    && !s.waiting && !s.motor_active && !s.aborted
  }

  def downMotor(s: DrillState): BigInt = {
    if(s.currHeight - downSpeed >= s.targetHeight) s.currHeight - downSpeed
    else s.targetHeight
  }

  def drillMotor(s: DrillState): BigInt = {
    if(s.currDeployment + drillSpeed <= s.targetDeploymnet) s.currDeployment + drillSpeed
    else s.targetDeploymnet
  }

  def retractMotor(s: DrillState): BigInt = {
    if(s.currDeployment - retractSpeed >= 0) s.currDeployment - retractSpeed
    else 0
  }

  def upMotor(s: DrillState): BigInt = {
    if(s.currHeight + upSpeed <= drillHeight) s.currHeight + upSpeed
    else drillHeight
  }

  def action(s: DrillState, instruction:Instruction): Option[DrillState] = {

    instruction match
        case Launch() => s.currState match
          case Idle() => Some(s.copy(currState = Down(), motor_active = true))
          case Down() => Some(s.copy(currState = Drill(), motor_active = true))
          case Drill() => Some(s.copy(currState = Retract(), motor_active = true))
          case Retract() => Some(s.copy(currState = Up(), motor_active = true))
          case Up() => Some(s.copy(currState = Idle(), motor_active = false))

        case Motor() if s.motor_active => s.currState match
          case Idle() => Some(s.copy())
          case Down() => {
            val newHeight: BigInt = downMotor(s)
            val newMotor: Boolean = newHeight == s.targetHeight
            Some(s.copy(currHeight = newHeight, motor_active = newMotor))
          }
          case Drill() => {
            val newDeployment: BigInt = drillMotor(s)
            val newMotor: Boolean = newDeployment == s.targetDeploymnet
            Some(s.copy(currDeployment = newDeployment, motor_active = newMotor))
          }
          case Retract() => {
            val newDeployment: BigInt = retractMotor(s)
            val newMotor: Boolean = newDeployment == 0
            Some(s.copy(currDeployment = newDeployment, motor_active = newMotor))
          }
          case Up() => {
            val newHeight: BigInt = upMotor(s)
            val newMotor: Boolean = newHeight == drillHeight
            Some(s.copy(currHeight = newHeight, motor_active = newMotor))
          }       
        
        case Pause() => Some(s.copy(motor_active = false, waiting = true))
        case Resume() => Some(s.copy(motor_active = true, waiting = false))
        case Abort() => {
          // TODO : Not already aborted
          //require(!s.aborted)
          Some(s.copy(currState = Retract(), aborted = true))
        }
          
        case _ => Some(s.copy())
  }

  @extern
  def main(args: Array[String]): Unit = {
    println("Test")
  }
}
```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:117)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:114)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:436)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:156)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:91)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:61)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:21)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:375)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0