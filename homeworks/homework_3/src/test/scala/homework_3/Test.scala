package homework_3

import utest._

object Test extends TestSuite{

    val collectionWithOneElement = Seq(1)
    val collectionWithMultipleItems = Seq(1, 5, 3)
    val emptySeq = Seq()

    val tests = Tests{
        'test_prettyBooleanFormatter1 - {
            testSomePrettyBooleanFormatter(Exercises.prettyBooleanFormatter1)
        }
        'test_prettyBooleanFormatter2 - {
            testSomePrettyBooleanFormatter(Exercises.prettyBooleanFormatter2)
        }
        'test_prettyBooleanFormatter3 - {
            testSomePrettyBooleanFormatter(Exercises.prettyBooleanFormatter3)
        }

        'test_max1 - {
            assert(Exercises.max1(collectionWithOneElement) == 1)
            assert(Exercises.max1(collectionWithMultipleItems) == 5)
            var exceptionWasThrow = false;
            try{
                Exercises.max1(emptySeq)
            }
            catch {
                case _ : Throwable => exceptionWasThrow = true
            }
            assert(exceptionWasThrow)
        }

        'test_max2 - {
            assert(Exercises.max2(collectionWithOneElement) == Seq(1))
            assert(Exercises.max2(collectionWithMultipleItems) == Seq(5))
            assert(Exercises.max2(emptySeq) == Seq())
        }

        'test_max3 - {
            assert(Exercises.max3(collectionWithOneElement).get == 1)
            assert(Exercises.max3(collectionWithMultipleItems).get == 5)
            assert(Exercises.max3(emptySeq).isEmpty)
        }

        'test_sum1 - {
            testSomeSumFunction(Exercises.sum1)
        }

        'test_sum2 - {
            testSomeSumFunction(Exercises.sum2)
        }

        'test_sum3 - {
            testSomeSumFunction(Exercises.sum3)
        }
    }

    def testSomePrettyBooleanFormatter(targetFunction: (Any) => String): Unit = {
        val trueStr = "правда"
        val falseStr = "ложь"
        assert(targetFunction(true) == trueStr)
        assert(targetFunction(false) == falseStr)
        assert(targetFunction("Example") == "Example".toString)
        assert(targetFunction(2) == 2.toString)
    }

    def testSomeSumFunction(targetTestFunction: (Int, Int) => Int): Unit = {
        assert(targetTestFunction(-1, 1) == 0)
        assert(targetTestFunction(0, 0) == 0)
        assert(targetTestFunction(1, 1) == 2)
        assert(targetTestFunction(4, 5) == 9)
    }
}
