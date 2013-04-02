package com.careverge.test.lift.snippet.util

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.careverge.lift.snippet.util.HtmlStringTransformers
import xml.{NodeSeq, Unparsed, Text}

class TestHtmlStringTransformers extends FunSpec
with ShouldMatchers
with HtmlStringTransformers
{

  describe("NodeSeqOps") {
    describe("join") {
      it ("should join two nodes") {
        val theResult = Seq(Text("1"), Text("3")).join(Text("2"))
        theResult should equal (List(Text("1"), Text("2"), Text("3")))
      }

      it ("should join four nodes") {
        val theResult = Seq(Text("G"), Text("T"), Text("C"), Text("!")).join(Text("A"))
        theResult should equal (List(Text("G"), Text("A"), Text("T"), Text("A"), Text("C"), Text("A"), Text("!")))
      }

      it ("should handle using multiple nodes as a separator") {
        val theResult = Seq(Text("1"), Text("4")).join(Text("2") ++ Text("3"))
        theResult should equal (List(Text("1"), Text("2"), Text("3"), Text("4")))
      }
    }
  }

  describe("HtmlTransformHelpers") {
    describe("lineBreaksToBr") {
      it ("should be testable") {
        NodeSeq.fromSeq(Seq(Text("A"))).toList should equal (List(Text("A")))
        NodeSeq.fromSeq(Seq(Text("A"))).toList should not equal (List(Text("B")))
        NodeSeq.fromSeq(Seq(Unparsed("A"))).toList should equal (List(Unparsed("A")))
      }

      it ("should convert new lines to <br />") {
        val theResult = "\n".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(BR, NL))
      }

      it ("should convert carrage returns to <br />") {
        val theResult = "\r".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(BR, NL))
      }

      it ("should convert carrage return + new lines to <br />") {
        val theResult = "\r\n".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(BR, NL))
      }

      it ("should convert new line + carriage return into 2 <br />s") {
        val theResult = "\n\r".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(BR, NL, BR, NL))
      }

      it ("should convert 2 new lines into 2 <br />s") {
        val theResult = "\n\n".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(BR, NL, BR, NL))
      }

      it ("should convert 2 carriage returns into 2 <br />s") {
        val theResult = "\r\r".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(BR, NL, BR, NL))
      }

      it ("should not add line breaks after the text") {
        val theResult = "start \r\n finish.".convertToHtml(lineBreaksToBr).toList
        theResult should equal (List(Text("start "), BR, NL, Text(" finish.")))
      }
    }

    describe("leadingSpacesToNbsp") {
      it ("should convert leading space into &nbsp;") {
        val theResult = " with one space".convertToHtml(leadingSpacesToNbsp).toList
        theResult should equal (List(Unparsed("&nbsp;"), Text("with one space")))
      }

      it ("should convert leading space on multiple lines") {
        val theResult = "  two\r\n  two".convertToHtml(leadingSpacesToNbsp).toList
        theResult should equal (List(NBSP, NBSP, Text("two\r\n"), NBSP, NBSP, Text("two")))
      }

      it ("should preserve ending newlines") {
        val theResult = "two\r\n".convertToHtml(leadingSpacesToNbsp).toList
        theResult should equal (List(Text("two\r\n")))
      }

      it ("should work with starting newlines") {
        val theResult = "\r\n  two".convertToHtml(leadingSpacesToNbsp).toList
        theResult should equal (List(NL, NBSP, NBSP, Text("two")))
      }
    }

    it ("should be able to replace line breaks followed by replacing leading spaces") {
      val theResult = "2 lines\r\n  with 2 leading spaces and spaces between words\r\n".convertToHtml(leadingSpacesToNbsp, lineBreaksToBr).toList
      theResult should equal (List(Text("2 lines"), BR, NL, NBSP, NBSP, Text("with 2 leading spaces and spaces between words"), BR, NL))
    }

    it ("should be able to replace leading spaces followed by replacing line breaks") {
      val theResult = "2 lines\r\n  with 2 leading spaces and spaces between words\r\n".convertToHtml(lineBreaksToBr, leadingSpacesToNbsp).toList
      theResult should equal (List(Text("2 lines"), BR, NL, NBSP, NBSP, Text("with 2 leading spaces and spaces between words"), BR, NL))
    }
  }
}
