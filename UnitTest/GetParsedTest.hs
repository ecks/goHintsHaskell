import Test.HUnit
import GetParsed

s = "7 [48]        qqww [ 2k*] vs.     shinkun [ 3k*] (  0   19  0 -5.5  5  I) (  0)"

findWhiteTest      = TestCase (assertEqual "Asserting findWhite"      (Right "qqww")    (getParsed findWhite s))
findBlackTest      = TestCase (assertEqual "Asserting findBlack"      (Right "shinkun") (getParsed findBlack s))
findGameIdTest     = TestCase (assertEqual "Asserting findGameId"     (Right "48")      (getParsed findGameId s))
findWrkTest        = TestCase (assertEqual "Asserting findWrk"        (Right "2k")      (getParsed findWrk s))
findBrkTest        = TestCase (assertEqual "Asserting findBrk"        (Right "3k")      (getParsed findBrk s))
findMoveTest       = TestCase (assertEqual "Asserting findMove"       (Right "0")       (getParsed findMove s))
findSizeTest       = TestCase (assertEqual "Asserting findSize"       (Right "19")       (getParsed findSize s))
findHandicapTest   = TestCase (assertEqual "Asserting findHandicap"   (Right "0")       (getParsed findHandicap s))

tests = TestList [TestLabel "findWhiteTest"    findWhiteTest, 
		  TestLabel "findBlackTest"    findBlackTest, 
		  TestLabel "findGameIdTest"   findGameIdTest,
		  TestLabel "findWrkTest"      findWrkTest,
		  TestLabel "findBrkTest"      findBrkTest,
                  TestLabel "findMoveTest"     findMoveTest,
                  TestLabel "findSizeTest"     findSizeTest,
                  TestLabel "findHandicapTest" findHandicapTest]
