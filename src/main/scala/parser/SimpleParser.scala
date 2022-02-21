package parser

import cats._
import cats.implicits._

object SimpleParser {

    trait JsonValue
    case class JsonNull() extends JsonValue
    case class JsonBool(v: Boolean) extends JsonValue
    case class JsonNumber(v: Number) extends JsonValue // no support for floats for now
    case class JsonString(v: String) extends JsonValue
    case class JsonArray(v: List[JsonValue]) extends JsonValue
    case class JsonObject(v: List[(String, JsonValue)]) extends JsonValue

    trait Parser[+A] {
        def run(v: String): Option[(String, A)]
    }

    implicit val parserFunctor = new Functor[Parser] {
        def map[A, B](p: Parser[A])(func: A => B): Parser[B] = {
            v => {
                val x = p.run(v).get
                Some(x._1, func(x._2))
            }
        }
    }

    def succeed[A](a: A): Parser[A] = stringP("") map (_ => a)

    implicit val applicativeForParser = new Applicative[Parser] with Functor[Parser] {
        def pure[A](a: A): Parser[A] = v => Some(v, a)

        override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = v => {
            try {
                val x = ff.run(v).get
                val y = fa.run(x._1).get
                Some(y._1, x._2(y._2))
            } catch {
                case _: Throwable => None
            }
        }
    }

    implicit val parserAlternative = new Alternative[Parser] with Applicative[Parser] {
        override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
            v => x.run(v) orElse y.run(v)

        override def pure[A](x: A): Parser[A] = Applicative[Parser].pure(x)

        override def empty[A]: Parser[A] = _ => None

        override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Applicative[Parser].ap(ff)(fa)
    }

    implicit val parserMonad = new Monad[Parser] with Alternative[Parser] with Functor[Parser] {
            override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
                v => {
                    try {
                        val x = fa.run(v).get
                        f(x._2).run(x._1)
                    } catch {
                        case _: Throwable => None
                    }
                }

            override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Functor[Parser].map(fa)(f)

            override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = Alternative[Parser].combineK(x, y)

            override def empty[A]: Parser[A] = Alternative[Parser].empty

            override def pure[A](a: A): Parser[A] = Alternative[Parser].pure(a)

            override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] =
                v => {
                    val x = f(a).run(v).get
                    x._2 match {
                        case Right(b) => Some(x._1, b)
                        case Left(a) => tailRecM(a)(f).run(x._1)
                    }
                }
        }

    def sequenceA[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = list match {
        case Nil => Applicative[F].pure(Nil)
        case x :: xs => (x, sequenceA(xs)) mapN {_ :: _}
    }

    def jsonNull: Parser[JsonValue] = stringP("null") map { _ => JsonNull() }

    def charP(x: Char): Parser[Char] = {
        case v if v.startsWith(x.toString) => Some((v.substring(1), x))
        case _ => None
    }

    def stringP(s: String): Parser[List[Char]] = sequenceA[Parser, Char](s.toList.map(charP))

    def jsonBool(): Parser[JsonValue] = {
        def jsonTrue() = stringP("true") map { _ => JsonBool(true) }
        def jsonFalse() = stringP("false") map { _ => JsonBool(false) }
        jsonTrue <+> jsonFalse
    }

    def spanP(f: Char => Boolean): Parser[String] = v => {
        val x = v.span(f)
        Some(x._2, x._1)
    }

    def jsonNumber(): Parser[JsonValue] = {
        def span(): Parser[String] = (v: String) => {
            val x = "[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r.findPrefixOf(v).get
            Some(v.substring(x.length), x)
        }
        span map {
            ds => JsonNumber(ds.toDouble)
        }
    }

    def stringLiteral(): Parser[String] = charP('"') *> spanP(_ != '"') <* (charP('"'))

    // TODO: escape support?
    //  it might be better to just remove all escape characters in json when/before called rather than processing them here.
    def jsonString(): Parser[JsonValue] = stringLiteral() map JsonString

    def ws(): Parser[String] = spanP(_.isSpaceChar)

    def some[A](a: Parser[A]): Parser[List[A]] = {
        for {
            x <- a
            y <- many(a)
        } yield x :: y
    }

    def many[A](a: Parser[A]): Parser[List[A]] = {
        some(a) <+> succeed(Nil)
    }

    def sepBy[A](sep: Parser[_], element: Parser[A]): Parser[List[A]] = {
        (many(element), many(sep *> element)) mapN {_ ::: _}
    }

    def jsonArray(): Parser[JsonValue] = for {
        _ <- charP('[') <* ws
        values <- sepBy(ws *> charP(',') <* ws, jsonValue)
        _ <- ws *> charP(']')
    } yield JsonArray(values)

    def jsonObject(): Parser[JsonValue] = for {
        _ <- charP('{') <* ws
        values <- sepBy(ws *> charP(',') <* ws, (stringLiteral <* ws <* charP(':') <* ws, jsonValue) mapN { (x, y) => (x, y)})
        _ <- ws() *> charP('}')
    } yield JsonObject(values)

    def jsonValue(): Parser[JsonValue] =
        jsonNull <+> jsonBool <+> jsonNumber <+> jsonString <+> jsonArray <+> jsonObject
}
