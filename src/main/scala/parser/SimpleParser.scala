package parser

import cats._
import cats.implicits._

object SimpleParser {

    trait JsonValue
    case class JsonNull() extends JsonValue
    case class JsonBool(v: Boolean) extends JsonValue
    case class JsonNumber(v: Int) extends JsonValue // no support for floats for now
    case class JsonString(v: String) extends JsonValue
    case class JsonArray(v: List[JsonValue]) extends JsonValue
    case class JsonObject(v: List[(String, JsonValue)]) extends JsonValue

    trait Parser[A] {
        def runParser(v: String): Option[(String, A)]
    }

    implicit val parserFunctor: Functor[Parser] =
        new Functor[Parser] {
            def map[A, B](p: Parser[A])(func: A => B): Parser[B] = {
                (v: String) => {
                    val x = p.runParser(v).get
                    Some(x._1, func(x._2))
                }
            }
        }

    implicit def applicativeForParser[A]: Applicative[Parser] = new Applicative[Parser] with Functor[Parser] {
        def pure[A](a: A): Parser[A] = (v: String) => Some(v, a)

        override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = new Parser[B] {
            override def runParser(v: String): Option[(String, B)] = {
                try {
                    val x = ff.runParser(v).get
                    val y = fa.runParser(x._1).get
                    Some(y._1, x._2(y._2))
                } catch {
                    case _ : Throwable => None
                }
            }
        }
    }

    implicit val parserAlternative = new Alternative[Parser] with Applicative[Parser] {
        override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
            (v: String) => x.runParser(v) orElse y.runParser(v)

        override def pure[A](x: A): Parser[A] = Applicative[Parser].pure(x)

        override def empty[A]: Parser[A] = (v: String) => None

        override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Applicative[Parser].ap(ff)(fa)
    }

    implicit val parserMonad = new Monad[Parser] with Alternative[Parser] with Functor[Parser] {
            override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = {
                v: String => {
                    try {
                        val x = fa.runParser(v).get
                        f.apply(x._2).runParser(x._1)
                    } catch {
                        case _ => None
                    }
                }
            }

            override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Functor[Parser].map(fa)(f)

            override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = Alternative[Parser].combineK(x, y)

            override def empty[A]: Parser[A] = Alternative[Parser].empty

            override def pure[A](a: A): Parser[A] = Alternative[Parser].pure(a)

            override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = {
                v => {
                    val x = f(a).runParser(v).get
                    x._2 match {
                        case Left(a) => tailRecM(a)(f).runParser(x._1)
                        case Right(b) => Some(x._1, b)
                    }
                }
            }
        }

    def sequenceA[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = list match {
        case Nil => Applicative[F].pure(Nil: List[A])
        case x :: xs => (x, sequenceA(xs)) mapN {
            _ :: _
        }
    }

    def jsonNull: Parser[JsonValue] = stringP("null").map(_ => JsonNull())

    def charP(x: Char): Parser[Char] = {
        case v if v.startsWith(x.toString) => Some((v.substring(1), x))
        case _ => None
    }

    def stringP(s: String): Parser[List[Char]] =
        sequenceA[Parser, Char](s.toList.map(charP))

    def jsonBool(): Parser[JsonValue] = {
        (stringP("true") <+> stringP("false")).map {
            v => v.mkString match {
                case "true" => JsonBool(true)
                case "false" => JsonBool(false)
                case _ => ??? // never happens
            }
        }
    }

    def spanP(f: Char => Boolean): Parser[String] = (v: String) => {
        val x = v.span(f)
        Some(x._2, x._1)
    }

    def jsonNumber(): Parser[JsonValue] = {
        spanP(_.isDigit) map {
            ds => JsonNumber(Integer.parseInt(ds))
        }
    }

    def stringLiteral(): Parser[String] = charP('"').*>(spanP(_ != '"')).<*(charP('"'))

    // TODO: escape support
    def jsonString(): Parser[JsonValue] = stringLiteral() map JsonString

    def ws(): Parser[String] = spanP(_.isSpaceChar)

    def succeed[A](a: A): Parser[A] = stringP("") map (_ => a)

    def some1[A](a: Parser[A]): Parser[List[A]] = {
        for {
            x <- a
            y <- many(a)
        } yield x :: y
    }

    def some[A](a: Parser[A]): Parser[List[A]] = {
        for {
            x <- a
            y <- many(a)
        } yield x :: y
    }

    def many[A](a: Parser[A]): Parser[List[A]] = {
        some(a) <+> succeed(Nil)
    }

    def sepBy[A, B](sep: Parser[A], element: Parser[B]): Parser[List[B]] = {
        (many(element), many(sep *> element)) mapN {_ ::: _}
    }

    def jsonArray(): Parser[JsonValue] = {
        def sep() = ws *> charP(',') <* ws
        def elements() = sepBy(sep, jsonValue)
        for {
            _ <- charP('[') <* ws
            values <- elements()
            _ <- ws *> charP(']')
        } yield JsonArray(values)
    }

    def jsonValue(): Parser[JsonValue] = {
        jsonNull <+> jsonBool <+> jsonNumber <+> jsonString <+> jsonArray <+> jsonObject
    }

    def jsonObject(): Parser[JsonValue] = {
        def pair() = {
            (stringLiteral, (ws *> charP(':') <* ws), jsonValue) mapN { (x, _, y) => (x, y) }
        }
        for {
            _ <- charP('{').<*(ws())
            values <- sepBy((ws *> charP(',') <* ws), pair)
            _ <- ws().*>(charP('}'))
        } yield JsonObject(values)
    }
}
