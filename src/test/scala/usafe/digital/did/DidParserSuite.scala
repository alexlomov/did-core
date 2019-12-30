package usafe.digital.did

import cats.data.NonEmptyList
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import usafe.digital.did.parser.parse
import usafe.digital.did.types._

class DidParserSuite extends AnyFunSpecLike with Matchers {

  describe("DID parser") {
    it("parses a valid DID with a method name and a method specific ID") {
      val raw = RawDid("did:test:very-basic")

      parse(raw) shouldBe Right(Did(MethodName("test"), MethodSpecificId("very-basic")))
    }

    it("parses a valid DID with a method name, a method specific ID and method parameters") {
      val raw = RawDid("did:test:very-basic;a=b;very-basic:p=123")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          methodParameters = Option(NonEmptyList.fromListUnsafe(Parameter("a", "b") :: Parameter("very-basic:p", "123") :: Nil))
        )
      )
    }

    it("parses a valid DID with a method name, a method specific ID and a path") {
      val raw = RawDid("did:test:very-basic/path/to/salvation")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          path = Option(Path("/path/to/salvation"))
        )
      )
    }

    it("parses a valid DID with a method name, a method specific ID, method parameters and a path") {
      val raw = RawDid("did:test:very-basic;a=b;very-basic:p=123/path/to/salvation")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          methodParameters = Option(NonEmptyList.fromListUnsafe(Parameter("a", "b") :: Parameter("very-basic:p", "123") :: Nil)),
          path = Option(Path("/path/to/salvation"))
        )
      )
    }

    it("parses a valid DID with a method name, a method specific ID and query parameters") {
      val raw = RawDid("did:test:very-basic?with=query&with=repeat")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          queryParameters = Option(NonEmptyList.fromListUnsafe(Parameter("with", "query") :: Parameter("with", "repeat") :: Nil))
        )
      )
    }

    it("parses a valid DID with a method name, a method specific, method parameters, path and and query parameters") {
      val raw = RawDid("did:test:very-basic;a=b;very-basic:p=123/path/to/salvation?with=query&with=repeat")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          methodParameters = Option(NonEmptyList.fromListUnsafe(Parameter("a", "b") :: Parameter("very-basic:p", "123") :: Nil)),
          path = Option(Path("/path/to/salvation")),
          queryParameters = Option(NonEmptyList.fromListUnsafe(Parameter("with", "query") :: Parameter("with", "repeat") :: Nil))
        )
      )
    }

    it("parses a valid DID with a method name, a method specific, method parameters, a path, query parameters and a fragment") {
      val raw = RawDid("did:test:very-basic;a=b;very-basic:p=123/path/to/salvation?with=query&with=repeat#frag-me")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          methodParameters = Option(NonEmptyList.fromListUnsafe(Parameter("a", "b") :: Parameter("very-basic:p", "123") :: Nil)),
          path = Option(Path("/path/to/salvation")),
          queryParameters = Option(NonEmptyList.fromListUnsafe(Parameter("with", "query") :: Parameter("with", "repeat") :: Nil)),
          fragment = Option(Fragment("frag-me"))

        )
      )
    }

    it("parses a valid DID with a method name, a method specific ID and a fragment") {
      val raw = RawDid("did:test:very-basic#frag-me")
      parse(raw) shouldBe Right(
        Did(
          MethodName("test"),
          MethodSpecificId("very-basic"),
          fragment = Option(Fragment("frag-me"))
        )
      )
    }
  }

}
