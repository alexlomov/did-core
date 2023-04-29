package usafe.digital.did

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, digit, hexdig}
import usafe.digital.did.types.*

object parser:

  def parse(rawDid: RawDid): Either[DidValidationFault, Did] =
    didParseScheme.parse(rawDid.toString())
      .map { _._2 }
      .leftMap { pe => DidValidationFault(pe.toString()) }


  @throws[DidValidationFault]("If input is not a valid DID. This function is intended for test code only.")
  def parseUnsafe(rawDid: RawDid): Did = parse(rawDid) match
    case Right(d) => d
    case Left(e) => throw e

  val lowerAlpha = Parser.charIn('a' to 'z')
  val didScheme = Parser.string("did")
  val colon = Parser.char(':')
  val semicolon = Parser.char(';')
  val slash = Parser.charIn('/')
  val pctEnc = (Parser.charIn('%') ~ hexdig.rep(2, 2))
  val unreservedChar = alpha | digit | Parser.charIn('-', '.', '_', '~')
  val paramChar = unreservedChar | Parser.charIn(':', '@')
  val subDelim = Parser.charIn('!', '$', '&', '\'', '(',  ')', '*', '+', ',',  ';', '=')
  val methodName = (lowerAlpha | digit).repAs[String].map { MethodName.apply }
  val methodSpecificId = (alpha | Parser.charIn(List('_', '-', '.'))).repAs[String].map { MethodSpecificId.apply }
  val paramChars = paramChar.rep.string | pctEnc.string
  val kvParam = ((paramChars <* Parser.char('=')) ~ paramChars).map { Parameter.apply }
  val methodParams = kvParam.repSep(semicolon)
  val queryParams = kvParam.repSep(Parser.char('&'))
  val segment = (paramChar | subDelim).repAs0[String]
  val path = (slash.string ~ segment).rep.string.map { Path.apply }
  val fragment = paramChar.repAs[String].map { Fragment.apply }

  val didParseScheme: Parser[Did] =
    ((didScheme.void *> methodName.surroundedBy(colon)) ~
    methodSpecificId ~
    (semicolon *> methodParams).? ~
    path.? ~
    (Parser.char('?') *> queryParams).? ~
    (Parser.char('#') *> fragment).?)
    .map { case (((((mn, msId), mparams), path), qparams), frag) => Did(mn, msId, mparams, path, qparams, frag)  }


