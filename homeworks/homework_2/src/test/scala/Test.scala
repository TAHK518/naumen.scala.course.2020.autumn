import Exercises.{Vector2D, balls}
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(2, 4) == 3)
            assert(Exercises.sumOfDivBy3Or5(4, 5) == 5)
            assert(Exercises.sumOfDivBy3Or5(2, 5) == (3+5))
            assert(Exercises.sumOfDivBy3Or5(9, 17) == (9+10+12+15))
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(11) == Seq(11))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(0,0), Vector2D(2,2), Vector2D(3,1), Vector2D(4,2)) == 14.0)
            assert(Exercises.sumScalars(Vector2D(0,0), Vector2D(0,1), Vector2D(0,0), Vector2D(1,0)) == 0.0)
        }

        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(0,1), Vector2D(1,0), Vector2D(1,0), Vector2D(0,1)) == 0.0)
            assert((Exercises.sumCosines(Vector2D(1,0), Vector2D(1,2), Vector2D(0,1), Vector2D(1,2)) - 1.34).abs <= 0.01)
            assert((Exercises.sumCosines(Vector2D(3,0), Vector2D(4,2), Vector2D(5,6), Vector2D(1,4)) - 1.79).abs <= 0.01)
        }

        'test_sortByHeavyweight - {
            val testBalls = Map("FirstBall" ->  (2, 2.0), "SecondBall" -> (2, 1.0))
            val expectedResult = Seq("SecondBall", "FirstBall")

            assert(Exercises.sortByHeavyweight(testBalls) == expectedResult)

            val expectedResultForDefaultBalls = Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite")
            assert(Exercises.sortByHeavyweight(Exercises.balls) == expectedResultForDefaultBalls)
        }
    }
}
