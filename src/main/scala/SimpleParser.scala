import cats.{Alternative, Applicative, Functor}
import cats.implicits._

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
        def runParser(in: String): Option[(String, T)]
    }

    object Parser {
        def from[A](f: String => Option[(String, A)]): Parser[A] = (in) => f(in)
    }

    implicit val parserFunctor: Functor[Parser] =
        new Functor[Parser] {
            override def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
                (input) => {
                    for (res <- p.runParser(input)) yield (res._1, f(res._2))
                }
            }
        }

    implicit val parserApplicative: Applicative[Parser] =
        new Applicative[Parser] {
            override def pure[A](x: A): Parser[A] = {
                (input) => Some(input, x)
            }
            override def ap[A, B](p1: Parser[A => B])(p2: Parser[A]): Parser[B] = {
                (input) => {
                    for (
                        res1 <- p1.runParser(input);
                        res2 <- p2.runParser(res1._1)
                    ) yield (res2._1, res1._2(res2._2))
                }
            }
        }

    implicit val parserAlternative: Alternative[Parser] =
        new Alternative[Parser] {
            override def pure[A](a: A) = Parser.from(Function.const(Option(null, a)))
            override def empty[A] = Parser.from(Function.const(None))
            override def combineK[A](l: Parser[A], r: Parser[A]): Parser[A] =
                new Parser[A] {
                    def runParser(in: String) = l.runParser(in).orElse(r.runParser(in))
                }

            override def ap[A, B](p1: Parser[A => B])(p2: Parser[A]): Parser[B] =
                (input) => {
                    for (
                        res1 <- p1.runParser(input);
                        res2 <- p2.runParser(res1._1)
                    ) yield (res2._1, res1._2(res2._2))
                }
        }

    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
        case Nil     => Applicative[F].pure(Nil: List[A])
        case x :: xs => (x, sequenceA(xs)) mapN {_ :: _}
    }

    def jsonNull: Parser[JsonValue] = {
        (new Parser[JsonValue] {
            def runParser(in: String): = (JsonNull
        })
    }

    def charP(x: Char): Parser[Char] {
        def f(input: String): Option[] {

        }

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
