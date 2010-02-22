package com.etsy.phps.tests

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scala.io.Source


class PHPSSpec extends Spec with ShouldMatchers {

  def fixture(name : String) = new File("tests/fixtures/%s.phps".format(name))
  
  def parse(name : String) = PHPS.parse(Source.fromFile(fixture(name)))


  describe("the PHPS parser") {

    it("should parse integers") {
      parse("int") should be (3)
    }

    it("should parse large integers") {
      parse("biggerint") should be (252873459)
    }

    it("should parse negative integers") {
      parse("negativeint") should be (-70)
    }

    it("should parse doubles") {
      parse("overflowint") should be (6442450941.0)
    }

    it("should parse strings") {
      parse("string") should be ("foo")
    }

    it("should parse empty strings") {
      parse("empty-string") should be ("")
    }

    it("should parse true booleans") {
      parse("bool") == true should be (true)
    }

    it("should parse false booleans") {
      parse("bool-false") == false should be (true)
    }

    it("should parse nulls") {
      parse("null") == null should be (true)
    }

    it("should parse empty arrays") {
      parse("empty-array") should be (Nil)
    }

    it("should parse simple numbered arrays as lists") {
      parse("array") should be (List(1,2,3))
    }

    it("should parse unordered numbered arrays as maps") {
      parse("tricky-array") should be (Map(0 -> 1, 3 -> 2))
    }

    it("should return associative arrays as maps") {
      parse("assoc-array") should be (Map("foo" -> 1, "bar" -> 2))
    }

    it("should return arrays with mixed key types as maps") {
      parse("mixed-array") should be (Map(1 -> "a", "foo" -> "bar"))
    }

  }

}
