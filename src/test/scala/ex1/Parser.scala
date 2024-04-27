package ex1

import org.scalatest.matchers.should.Matchers.*

class ParserTests extends org.scalatest.funsuite.AnyFunSuite:
  import Parsers.*  
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser()
  def parserShortenThenN = new ShortenThenNParser(Set('a', 'b', 'c'), 5) 

  test("testBasicParser"):
    parser.parseAll("abbc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true


  test("testNotEmptyParser"):
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List()) shouldBe false

  test("testNotTwoConsecutiveParser"):
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  test("testNotEmptyAndNotTwoConsecutiveParser"):
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  test("testStringParser"):
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true

  test("testShorterThenN"):
    parserShortenThenN.parseAll("aaaaaaaaa".toList) shouldBe false
    parserShortenThenN.parseAll("abc".toList) shouldBe true
    parserShortenThenN.parseAll("abcd".toList) shouldBe false
