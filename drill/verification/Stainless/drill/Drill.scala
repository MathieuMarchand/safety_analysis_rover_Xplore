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

  abstract sealed class Direction
  case class UP_DIR() extends Direction
  case class DOWN_DIR() extends Direction

  case class DrillState(
    currState: State,
    currHeight:BigInt,
    currDeployment: BigInt,
    targetHeight:BigInt,
    targetDeploymnet:BigInt,
    waiting:Boolean,
    motor_active:Boolean,
    aborted:Boolean,
    direction: Direction
  ){
    
    //R01
    require((currState == Idle()) ==> (currDeployment == 0 && currHeight == drillHeight))
    
    
    //R02
    require(!(aborted && waiting))
    
    //R03 
    require(targetDeploymnet >= 0 && targetDeploymnet <= drillLength)
    require(currDeployment >= 0 && currDeployment <= targetDeploymnet)
    
    //R04
    require(targetHeight >= 0 && targetHeight <= drillHeight)
    require(currHeight >= targetHeight && currHeight <= drillHeight)
    
    //R05
    require(
      ((currState == Down()    && currHeight == targetHeight) 
    || (currState == Drill()   && currDeployment == targetDeploymnet) 
    || (currState == Retract() && currDeployment == 0) 
    || (currState == Up()      && currHeight == drillHeight))
    ==> !motor_active)
    
    //R06
    require(!(motor_active && waiting))
    
    //R07
    require(!(currState == Idle() && motor_active))
     
    //R08
    
    require((
              currState != Idle() && !waiting &&
              ((currState == Down() && currHeight != targetHeight)
            || (currState == Drill() && currDeployment != targetDeploymnet)
            || (currState == Retract() && currDeployment != 0)
            || (currState == Up() && currHeight != drillHeight)))
          ==> motor_active)
  }


  def loop(s: DrillState, nb: BigInt, ins: List[Instruction]): (DrillState, BigInt) = {
    require(!ins.isEmpty && nb >= 0)
    val v = action(s, ins.head)

    v match
      case None() => return (s, nb+1)
      case _ => 
    val news = v.get

    ins match
      case Cons(x, xs) if xs.isEmpty => (news, nb+1)
      case Cons(x, xs) => loop(news, nb+1, xs)
  }

  //R10: All states can be reached
  def existPath(sys: DrillState): Unit={
    require(isInitial(sys))
    // To prove existance, need to construct an example
    val arr: List[Instruction] = List(Launch(), Launch(), Launch(), Launch(), Launch())

    val (s, n) = loop(sys, 0, arr)
    assert(s.currState == Idle())
    assert(n == 5)

  }

  def goodExecution(sys: DrillState): Boolean = {
    require(isInitial(sys))
    val arr: List[Instruction] = List(Launch())
    true
  }

  def isInitial(s:DrillState): Boolean = {
    s.currHeight == drillHeight && s.currDeployment == 0
    && s.currState == Idle()
    && !s.waiting && !s.motor_active && !s.aborted
    && s.direction == DOWN_DIR()
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

  def mod1(s: DrillState): BigInt = {
    s.direction match
      case DOWN_DIR() => downMotor(s)
      case UP_DIR() => upMotor(s)
  }

  def drilll(s: DrillState): BigInt = {
    s.direction match
      case DOWN_DIR() => drillMotor(s)
      case UP_DIR() => retractMotor(s)
  }

  def action(s: DrillState, instruction:Instruction): Option[DrillState] = {

    instruction match
        case Launch() => s.currState match
          case Idle() => Some(s.copy(currState = Down(), motor_active = true, direction = DOWN_DIR()))
          case Down() => Some(s.copy(currState = Drill(), motor_active = true, direction = DOWN_DIR()))
          case Drill() => Some(s.copy(currState = Retract(), motor_active = true, direction = UP_DIR()))
          case Retract() => Some(s.copy(motor_active = true, direction = UP_DIR()))//case Retract() => Some(s.copy(currState = Up(), motor_active = true, direction = UP_DIR()))
          case Up() => None()//case Up() => Some(s.copy(currState = Idle(), motor_active = false))

        case Motor() if s.motor_active => s.currState match
          case Idle() => Some(s.copy())
          case Down() => {
            val newHeight: BigInt = mod1(s)
            val newMotor: Boolean = !(newHeight == s.targetHeight)
            Some(s.copy(currHeight = newHeight, motor_active = newMotor))
          }
          case Drill() => {
            val newDeployment: BigInt = drilll(s)
            val newMotor: Boolean = !(newDeployment == s.targetDeploymnet)
            Some(s.copy(currDeployment = newDeployment, motor_active = newMotor))
          }
          case Retract() => {
            val newDeployment: BigInt = drilll(s)
            val newMotor: Boolean = !(newDeployment == 0)
            Some(s.copy(currDeployment = newDeployment, motor_active = newMotor))
          }
          case Up() => {
            val newHeight: BigInt = mod1(s)
            val newMotor: Boolean = !(newHeight == drillHeight)
            Some(s.copy(currHeight = newHeight, motor_active = newMotor))
          }       
        
        case Pause()  => Some(s.copy(motor_active = false, waiting = true))
        case Resume() => Some(s.copy(motor_active = true, waiting = false))
        case Abort()  => {  
          val n = s.copy(currState = Retract(), aborted = true)
          assert(n.currState == Retract() && n.aborted == true && n.motor_active == s.motor_active)
          Some(n)
        }
        
          
        case _ => Some(s.copy())
  }

  def isTraceLike(t: List[(DrillState, Instruction)]): Boolean = {
    t match
      case Cons((s1, i1), Cons((s2, i2), rest)) => {
        if(action(s1,i1) == Some(s2)) isTraceLike(Cons((s2, i2),rest))
        else false
      }
      case Cons((s1,i1), Nil()) => true
      case _ => false
  }

  // Definition of trace
  def isTrace(t: List[(DrillState, Instruction)]): Boolean = {
    t match
      case Cons((s,i), rest) => (isInitial(s) && isTraceLike(t))
      case _ => false
  }

  // R09 : Abort on an aborted drill should be ignored
  def doubleAbortNoEffect(t: List[(DrillState, Instruction)]): Boolean = {
    require(isTrace(t))
    require(t.size < 10)
    doubleAbortNoEffectRec(t)
  }.holds

  // 
  def doubleAbortNoEffectRec(t: List[(DrillState, Instruction)]): Boolean = {
    t match
      case Cons((s1,i1), Cons((s2, i2), rest)) => {
        if(s1.aborted && i1 == Abort()) s1 == s2 && doubleAbortNoEffectRec(Cons((s2, i2), rest))
        else doubleAbortNoEffectRec(Cons((s2, i2), rest))
      }
      case _ => true
  }

  // Verifies that equality is by content and not reference
  def eq(s: DrillState, i: Instruction): Boolean = {
    val s1 = action(s, i)
    val s2 = action(s, i)
    s1 == s2
  }.holds

  @extern
  def main(args: Array[String]): Unit = {
    println("Test")
  }
}