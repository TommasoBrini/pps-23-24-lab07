package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotSpec extends AnyFlatSpec with Matchers:
  "A SimpleRobot" should "turn correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  "A RobotWithBattery" should "do action correctly until it turns off" in:
    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North), 5)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    
    robot.turn(Direction.South)
    robot.direction should be(Direction.South)
    
    robot.turn(Direction.West)
    robot.direction should be(Direction.West)
    
    robot.turn(Direction.North)
    robot.direction should be(Direction.North)
  
    robot.act()
    robot.position should be((0, 1))
    
    robot.act()
    robot.position should be((0, 1))
    robot.turn(Direction.East)
    robot.direction should be(Direction.North)

  "A RobotCanFail" should "failed correctly" in:
    val robot = RobotCanFail(SimpleRobot((0, 0), Direction.North), 100)
    robot.act()
    robot.position should be((0, 0))
    robot.turn(Direction.East)
    robot.direction should be(Direction.North)

  "A RobotRepeated" should "act repeated correctly" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 5)
    robot.act()
    robot.position should be((0,5))
    robot.turn(Direction.East)
    robot.act()
    robot.position should be((5, 5))
  