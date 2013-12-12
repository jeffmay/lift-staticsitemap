package net.liftmodules.staticsitemap.path

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import PathUtils._

class TestPathUtils extends FunSpec with ShouldMatchers {

  describe("PathUtils.splitPathAndQuery") {

    it("should split the empty String") {
      splitPathAndQuery("") should be ("", "")
    }

    it("should split a single '?'") {
      splitPathAndQuery("?") should be ("", "")
    }

    it("should split the empty String with a parameter") {
      splitPathAndQuery("?q") should be ("", "q")
    }

    it("should split the root path '/'") {
      splitPathAndQuery("/") should be ("/", "")
    }

    it("should split the root path '/' with a parameter") {
      splitPathAndQuery("/?q") should be ("/", "q")
    }

    it("should split a url with no path parts and a query string of '&'") {
      splitPathAndQuery("?&") should be ("", "&")
    }

    it("should split a url with no path parts and a query string of '=&='") {
      splitPathAndQuery("?=&=") should be ("", "=&=")
    }

    it("should split a url with multiple path parts and no query string") {
      splitPathAndQuery("/p1/p2") should be ("/p1/p2", "")
    }

    it("should split a url with multiple path parts and a blank query string") {
      splitPathAndQuery("/p1/p2?") should be ("/p1/p2", "")
    }

    it("should split a url with multiple path parts and a query string with a single parameter") {
      splitPathAndQuery("/p1/p2?q1=1") should be ("/p1/p2", "q1=1")
    }

    it("should split a url with multiple path parts and a query string with multiple parameters") {
      splitPathAndQuery("/p1/p2?q1=1&q2=2") should be ("/p1/p2", "q1=1&q2=2")
    }

    it("should split a url with no path parts and a query string with a single parameter") {
      splitPathAndQuery("?q1=1") should be ("", "q1=1")
    }

    it("should split a url with no path parts and a query string with multiple parameters") {
      splitPathAndQuery("?q1=1&q2=2") should be ("", "q1=1&q2=2")
    }

    it("should not get confused by '\\?'") {
      splitPathAndQuery("/\\?q") should be ("/\\", "q")
    }

    it("should not get confused by multiple '?' characters") {
      splitPathAndQuery("/?q1?q2") should be ("/", "q1?q2")
    }

    it("should not get confused by '\\?' after a '?'") {
      splitPathAndQuery("/?q1?q2\\?") should be ("/", "q1?q2\\?")
    }

  }

}
