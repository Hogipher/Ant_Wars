package players.TheFlood

import scant._
import awannotation.AntWarsPlayer
import scala.math._

@AntWarsPlayer
class TheFloodQueen extends Queen {
    def takeTurn = {
        //if (time%100==0) {layEgg(new StalkerBlkr3)}
        //if (time%110==0) {layEgg(new StalkerBlkr1)}
        //if (time%120==0) {layEgg(new StalkerBlkr3)}
        //if (time%130==0) {layEgg(new StalkerBlkr2)}
        //if (time%50==0) {layEgg(new StalkerBlkr4)}
        //if (time%60==0) {layEgg(new StalkerBlkr2)}
        //if (time%70==0) {layEgg(new StalkerBlkr3)}
        if (time%50==0) {layEgg(new StalkerBlkr4)}
        //if (time%110==0) {layEgg(new StalkerBlkr2)}
        if (time%100==0) {layEgg(new StalkerBlkr3)}        
        if (countAnts == 1){layEgg(new StalkerBlkr4)}
        //else if (countAnts == 2){layEgg(new StalkerBlkr1)}
        else if (countAnts == 2) {layEgg(new StalkerBlkr2)}
        else if (countAnts == 3) {layEgg(new StalkerBlkr3)}
        else if (countAnts == 4) {layEgg(new RunnerNthrn)}
        else if (time<=50&&time>30) {layEgg(new RunnerMid)}
        //else if (countAnts == 5) {layEgg(new lilRunner)}        
        else if (time<=70&&time>50) {layEgg(new RunnerSthrn)}
        //else if (countAnts == 7) {layEgg(new ClickerFar)}
        //else if (countAnts == 8) {layEgg(new ClickerMid)}
        //else if (countAnts == 9) {layEgg(new ClickerClose)}
        //else if (countAnts == 5) {layEgg(new RunnerMid)}
        //else if (countAnts == 6) {layEgg(new RunnerSthrn)}
    }
}

abstract class EarlyStage extends Ant {
    capacity = 150
    var switchDir:Boolean = true
    var foodGuards:Int = 1
    var homingBeq:Boolean = false
    var holding:Boolean = false
    def scavange:Boolean = {
        if (lookForFood(East)==1) {move(East);icon='*';true}
        else if (lookForFood(North)==1) {move(North);icon='*';true}
        else if (lookForFood(South)==1) {move(South);icon='*';true}
        else if (lookForFood(West)==1) {move(West);icon='*';true}
        else false
    }
    def lkFrMsg360:Boolean = {
        if (lookForFood(East)==1) true
        else if (lookForFood(North)==1) true
        else if (lookForFood(South)==1) true
        else if (lookForFood(West)==1) true
        else false
    }
    def attack360 = {
        if (lookForEnemy(East)==true) {attack(East)}
        else if (lookForEnemy(North)==true) {attack(North)}
        else if (lookForEnemy(South)==true) {attack(South)}
        else if (lookForEnemy(West)==true) {attack(West)}
    }
    def moveDiagonal = {
        if(homingBeq){
            if (switchDir) {switchDir=false;move(South)}
            else {switchDir=true;move(West)}
        }
        else {
            if (switchDir) {switchDir=false;move(North)}
            else {switchDir=true;move(East)}
        }
    }
    def nearQueen:Boolean = {
        if (pos.x==1&&pos.y==1) true
        else if (pos.x==0&&pos.y==1) true
        else if (pos.x==1&&pos.y==0) true
        else if (pos.x==1&&pos.y== -1) true
        else if (pos.x==0&&pos.y== -1) true
        else false 
    }
    var gtCoordCnt:Int = 1
    def homing(origX:Int,origY:Int,goalX:Int,goalY:Int) = {
        val southWest:Boolean = origX < goalX && origY < goalY
        val northWest:Boolean = origX < goalX && origY > goalY
        val northEast:Boolean = origX > goalX && origY > goalY
        val xDist:Int = abs(origX-goalX)
        val yDist:Int = abs(origY-goalY)
        val factor:Int = if (xDist>yDist) 1+(xDist/yDist) else 1+(yDist/xDist)
        if (gtCoordCnt>factor)gtCoordCnt=0
        //println(("factor: ",factor))
        if (southWest){
            if (factor ==gtCoordCnt){
                gtCoordCnt=1
                move(North)
                }
            else {
                gtCoordCnt+=1
                move(East)
            }
        }
        else if (northEast){
            if (factor == gtCoordCnt){
                gtCoordCnt=1
                move(South)
            }
            else {
                gtCoordCnt+=1
                move(West)
            }
        }
        else if (northWest) {
            if (factor == gtCoordCnt){
                gtCoordCnt=1
                move(South)
            }
            else {
                gtCoordCnt+=1
                move(East)
            }
        }
        else {
            if (factor == gtCoordCnt){
                gtCoordCnt=1
                move(North)
            }
            else {
                gtCoordCnt+=1
                move(West)
            }
        }
    }
}

abstract class Stalker extends EarlyStage {
    //INFINATE_HEALTH_CHEAT
    icon = 'S'
    strength = 4
    health = 14
    var steps = 0
}
/*class StalkerBlkr1 extends Stalker {
    def takeTurn = {
        if (steps<=20) {attack360;move(East);steps+=1}
        else if (steps<=29) {attack360;move(North);steps+=1}
        else {attack360}
    }
}
class StalkerBlkr2 extends Stalker {
    def takeTurn = {
        if (steps<=20) {move(East);steps+=1}
        else if (steps<=31) {move(South);steps+=1}
        else {attack360}
    }
}
class StalkerBlkr3 extends Stalker {
    def takeTurn = {
        if (steps==17) {move(North);steps+=1}
        else if (steps<=21) {move(East);steps+=1}
        else if (steps<=23) {move(South);steps+=1}
        else {attack360}
    }
}*/
class StalkerBlkr4 extends Stalker {
    strength = 1
    def takeTurn = {
        if (pos.x>=35&&pos.y<= -1) attack360
        else if (pos.x<=35) move(East)
        else if (pos.y>= -1) move(South)
    }
}
class StalkerBlkr2 extends Stalker {
    strength = 1
    def takeTurn = {
        if (pos.x>=35&&pos.y<= 0) attack360
        else if (pos.x<=35) move(East)
        else if (pos.y>= 0) move(South)
    }
}
class StalkerBlkr3 extends Stalker {
    strength = 1
    def takeTurn = {
        if (pos.x>=35&&pos.y<= 1) attack360
        else if (pos.x==0&&pos.y>=1) move(East)
        else if (pos.y>= -1) move(South)
        else if (pos.x<=35) move(East)
    }
}

abstract class Runner extends EarlyStage {
    icon = 'R'
    capacity = 500
    var steps = 0 
}

class RunnerNthrn extends Runner {
    def takeTurn = {
        if (homingBeq == false) {
            //println((pos.x,pos.y))
            if (pos.x==20&&pos.y==11) move(South)
            if (pos.x>=20&&pos.y>=10) {homingBeq=true;pickup}
            else if (scavange) {pickup;homingBeq=true}//pick up along way
            else {homing(0,1,20,11)}
        }
        else {
            //println((pos.x,pos.y))
            if (nearQueen) {homingBeq=false;drop}
            else homing(20,10,0,1)
        }
    }
}
class lilRunner extends Runner {
    capacity = 150
    def takeTurn = {
        if (homingBeq == false) {
            //println((pos.x,pos.y))
            if (pos.x==20&&pos.y==11) move(South)
            if (pos.x>=20&&pos.y>=10) {homingBeq=true;pickup}
            else if (scavange) {pickup;homingBeq=true}//pick up along way
            else {homing(0,1,20,11)}
        }
        else {
            //println((pos.x,pos.y))
            if (nearQueen) {homingBeq=false;drop}
            else homing(20,10,0,1)
        }
    }
}
class RunnerMid extends Runner {
    def takeTurn = {
        if (homingBeq == false) {
            if (pos.y>1) move(South)
            if (pos.x>=20&&pos.y==1) {homingBeq=true;move(South);pickup}
            else if (scavange) {pickup;homingBeq=true}
            else move(East)
        }
        else {
            if (nearQueen) {homingBeq=false;move(North);drop}
            else move(West)
        }
    }
}
class RunnerSthrn extends Runner {
    def takeTurn = {
        if (homingBeq == false) {
            //println((pos.x,pos.y))
            if (pos.x==20) {move(South);homingBeq=true;pickup}
            else if (scavange) {pickup;homingBeq=true}//pick up along way
            else {
                if (pos.x==20&&pos.y== -9) move(South)
                homing(0,1,20,-9)
            }
        }
        else {
            //println((pos.x,pos.y))
            if (pos.x==20&&pos.y== -9) move(South)
            if (nearQueen) {homingBeq=false;drop}
            else homing(20,-9,0,1)
        }
    }
}

abstract class AdvancedStage extends Ant {
    capacity = 0
    var steps = 0
    var switchDir:Boolean = true
    var homingBeq:Boolean = false
    var holding:Boolean = false
    def nearEnyQueen:Boolean = {
        if (pos.x==39&&pos.y==0) true 
        if (pos.x==40&&pos.y==1) true 
        if (pos.x==39&&pos.y== -1) true 
        if (pos.x==40&&pos.y==0) true 
        if (pos.x==40&&pos.y== -1) true 
        false
    }
    def scavange:Boolean = {
        if (lookForFood(East)==1) {move(East);icon='*';true}
        else if (lookForFood(North)==1) {move(North);icon='*';true}
        else if (lookForFood(South)==1) {move(South);icon='*';true}
        else if (lookForFood(West)==1) {move(West);icon='*';true}
        else if (lookForFood(Here)==1) {pickup;icon='*';true}
        else false
    }
    def attack360 = {
        if (lookForEnemy(East)==true) {attack(East)}
        else if (lookForEnemy(North)==true) {attack(North)}
        else if (lookForEnemy(South)==true) {attack(South)}
        else if (lookForEnemy(West)==true) {attack(West)}
    }
    def nearQueen:Boolean = {
        if (pos.x==1&&pos.y==1) true
        else if (pos.x==0&&pos.y==1) true
        else if (pos.x==1&&pos.y==0) true
        else if (pos.x==1&&pos.y== -1) true
        else if (pos.x==0&&pos.y== -1) true
        else false 
    }
    var gtCoordCnt:Int = 1
    def homing(origX:Int,origY:Int,goalX:Int,goalY:Int) = {
        val southWest:Boolean = origX < goalX && origY < goalY
        val northWest:Boolean = origX < goalX && origY > goalY
        val northEast:Boolean = origX > goalX && origY > goalY
        val xDist:Int = abs(origX-goalX)
        val yDist:Int = abs(origY-goalY)
        val factor:Int = if (xDist>yDist) 1+(xDist/yDist) else 1+(yDist/xDist)
        if (gtCoordCnt>factor)gtCoordCnt=0
        //println(("factor: ",factor))
        if (southWest){
            if (factor ==gtCoordCnt){
                gtCoordCnt=1
                move(North)
                }
            else {
                gtCoordCnt+=1
                move(East)
            }
        }
        else if (northEast){
            if (factor == gtCoordCnt){
                gtCoordCnt=1
                move(South)
            }
            else {
                gtCoordCnt+=1
                move(West)
            }
        }
        else if (northWest) {
            if (factor == gtCoordCnt){
                gtCoordCnt=1
                move(South)
            }
            else {
                gtCoordCnt+=1
                move(East)
            }
        }
        else {
            if (factor == gtCoordCnt){
                gtCoordCnt=1
                move(North)
            }
            else {
                gtCoordCnt+=1
                move(West)
            }
        }
    }
}

abstract class Clicker extends AdvancedStage {
    icon = 'C'
    strength = 10
    health = 2
    def guard = {
        if (switchDir){
            if (pos.y<=10) {attack360;move(North)}
            else switchDir=false
        }
        else {
            if (pos.y>= -10) {attack360;move(South)}
            else switchDir=true
        }
    }
}


class ClickerFar extends Clicker {
    def takeTurn = {
        if (pos.x<=38) {attack360;move(East)}
        else if (pos.y>= -2) {attack360;move(South)}
        else attack360
    }
}
class ClickerMid extends Clicker {
    def takeTurn = {
        if (pos.x<=38) {attack360;move(East)}
        else if (pos.y>= -1) {attack360;move(South)}
        else attack360
    }
}

class ClickerClose extends Clicker {
    def takeTurn = {
        if (pos.x<=38) {attack360;move(East)}
        else if (pos.y>= 0) {attack360;move(South)}
        else attack360
    }
}

        /*println((pos.x,pos.y))
        if (nearEnyQueen) attack360
        else if (pos.x<40&&pos.y>0&&pos.x>19&&pos.y<5) {println("in");attack360;homing(20,5,40,0)}
        else if (pos.x<20&&pos.y<5) homing(0,1,20,5)
        else move(East)*/