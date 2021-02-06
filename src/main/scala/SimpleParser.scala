import cats.implicits.catsSyntaxTuple2Semigroupal
import cats.{Applicative, Functor}
import cats.syntax.applicative._
import cats.effect.IO
import cats.data.Newt

import java.nio.file.Path
import scala.io.Source

object SimpleParser {

    trait JsonValue

    case class JsonNull() extends JsonValue
    case class JsonBool(v: Boolean) extends JsonValue
    case class JsonNumber(v: Int) extends JsonValue // no support for floats for now
    case class JsonString(v: String) extends JsonValue
    case class JsonArray(v: List[JsonValue]) extends JsonValue
    case class JsonObject(v: (String, JsonValue)) extends JsonValue

    trait Parser[T] {
        def runParser(v: String) = Option[(String, T)]
    }

    implicit val parserFunctor: Functor[Parser] =
        new Functor[Parser] {
            def fmap[A, B](p: Parser[A])(func: A => B): Option[(String, A)] =
                value.map(func)
        }

    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
        case Nil     => Applicative[F].pure(Nil: List[A])
        case x :: xs => (x, sequenceA(xs)) mapN {_ :: _}
    }

    def jsonNull: Parser[JsonValue] = JsonNull(stringP("null"))

    def charP(x: Char): Parser[Char] {
        def f(): Option[] {

        }
        Parser[]
    }

    def stringP(s: String): Parser[JsonValue] = sequenceA(s.map(charP))

    def jsonBool(): Parser[JsonValue] = {
        def f(v: String): JsonBool = v match {
            case "true"     => JsonBool(true)
            case "false"    => JsonBool(false)
            case _          => ???
        }
        f(stringP("true").)
    }

    def spanP(f: Char => Boolean): Parser[String] = {
        Parser[]
    }

    def notNull(p: Parser[T]): Parser[T] = {
        Parser[]
    }


    def jsonNumber():Parser[JsonValue] = {
        def f(ds): JsonNumber = JsonNumber(Integer.parseInt(ds))
        f(notNull(spanP(_.isDigit)))
    }

    def stringLiteral(): Parser[String] = charP('"') spanP() charP('"')

    def ws(): Parser[String] = spanP(_.isSpaceChar)

    def sepBy(sep: Parser[A])(element: Parser[B]) = element // TODO

    def jsonArray(): Parser[JsonValue] = {
        // TODO
    }

    def jsonObject(): Parser[JsonValue] = {
        // TODO
    }

    def jsonValue(): Parser[JsonValue] = {
        jsonNull
    }

    def parseFile(filePath: Path)(parser: Parser[]): IO[Option[?]] =
        parser.runParser(Source.fromFile(filePath))
}
