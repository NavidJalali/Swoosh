package io.navidjalali.swoosh.parse

import io.navidjalali.swoosh.models.HexString
import io.navidjalali.swoosh.syntax.BytesSyntax._

import java.nio.charset.{Charset, StandardCharsets}
import scala.collection.mutable

sealed trait Parser[+A] {
  self =>
  def parse(input: HexString): Either[String, A] = parse(input.underlyingBytes)

  def parse(input: String, charset: Charset = StandardCharsets.UTF_8): Either[String, A] =
    parse(input.getBytes(charset))

  def parse(input: Seq[Byte]): Either[String, A] = {
    var loop = true

    type Continuation = Any => Parser[Any]

    var currentParser: Parser[Any] = self

    val stack = mutable.Stack[Continuation]()

    var remainder = input

    var result: Either[String, A] = null

    case class Handler[X, Y](fold: Parser.Fold[X, Y], remainder: Seq[Byte]) extends (X => Parser[Y]) {
      override def apply(x: X): Parser[Y] = fold.success(x)
    }

    def complete(res: Either[String, Any]): Unit =
      res match {
        case Right(value) =>
          if (stack.isEmpty) {
            loop = false
            result = Right(value.asInstanceOf[A])
          } else {
            val continue = stack.pop()
            currentParser = continue(value)
          }
        case Left(message) =>
          val errorHandler = findNextErrorHandler()
          if (errorHandler eq null) {
            loop = false
            result = Left(message)
          } else {
            currentParser = errorHandler.fold.failure(message)
            remainder = errorHandler.remainder
          }
      }

    def findNextErrorHandler(): Handler[Any, Any] = {
      var looping = true
      var handler: Handler[Any, Any] = null
      while (looping) {
        if (stack.isEmpty) looping = false
        else {
          val next = stack.pop()
          if (next.isInstanceOf[Handler[_, _]]) {
            looping = false
            handler = next.asInstanceOf[Handler[Any, Any]]
          }
        }
      }

      handler
    }

    while (loop) {
      currentParser match {
        case Parser.Succeed(a) =>
          complete(Right(a))

        case Parser.Fail(reason) =>
          complete(Left(reason))

        case Parser.FlatMap(parser, f) =>
          currentParser = parser
          stack.push(f.asInstanceOf[Any => Parser[Any]])

        case fold@Parser.Fold(parser, _, _) =>
          currentParser = parser
          stack.push(Handler(fold, remainder))

        case Parser.ByteIf(predicate) =>
          remainder.headOption match {
            case Some(value) if predicate(value) =>
              remainder = remainder.tail
              complete(Right(value))
            case _ =>
              complete(Left("Unexpected byte"))
          }

        case Parser.CharIf(predicate) =>
          val str = new String(remainder.toArray, StandardCharsets.UTF_8)
          str.headOption match {
            case Some(value) if predicate(value) =>
              remainder = str.tail.getBytes
              complete(Right(value))
            case None =>
              complete(Left("Unexpected char"))
          }

        case Parser.Sequence(seq) =>
          if (remainder.startsWith(seq)) {
            remainder = remainder.drop(seq.length)
            complete(Right(seq))
          } else {
            complete(Left(s"Expected ${seq.toHexString} but found ${remainder.take(seq.length).toHexString}"))
          }
      }
    }

    result
  }

  def suchThat(predicate: A => Boolean, error: String): Parser[A] = Parser.Fold[A, A](
    self, a => if (predicate(a)) Parser.succeed(a) else Parser.fail(error), Parser.fail
  )

  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser.FlatMap(self, f)

  def foldParser[B](failure: String => Parser[B], success: A => Parser[B]): Parser[B] =
    Parser.Fold(self, success, failure)

  def fold[B](failure: String => B, success: A => B): Parser[B] =
    Parser.Fold[A, B](self, a => Parser.succeed(success(a)), error => Parser.succeed(failure(error)))

  def map[B](f: A => B): Parser[B] = self.flatMap(a => Parser.succeed(f(a)))

  def as[B](b: => B): Parser[B] = self.map(_ => b)

  def unit(): Parser[Unit] = self.map(_ => ())

  def orElse[A1 >: A](that: => Parser[A1]): Parser[A1] = Parser.Fold(self, (a: A) => Parser.succeed(a), _ => that)

  def repeatedly: Parser[Vector[A]] =
    self.map(Vector(_)).zipWith(repeatedly)(_ ++ _).orElse(Parser.succeed(Vector()))

  def repeat(n: Int): Parser[Vector[A]] =
    if (n > 0) Vector.fill(n)(self).foldLeft(Parser.succeed(Vector.empty[A])) {
      case (acc, next) => acc.flatMap(vec => next.map(a => vec :+ a))
    }
    else Parser.Succeed(Vector.empty)

  def zip[B](that: => Parser[B]): Parser[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)

  def zipLeft[B](that: => Parser[B]): Parser[A] = self.zip(that).map(_._1)

  def zipRight[B](that: => Parser[B]): Parser[B] = self.zip(that).map(_._2)

  def zipWith[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
    (self zip that).map(f.tupled)
}

object Parser {

  def parseAll[A](parsers: Vector[Parser[A]])(input: Seq[Byte]): Vector[A] = {
    parsers.map(_.parse(input)).collect {
      case Right(value) => value
    }
  }

  def succeed[A](a: A): Parser[A] = Succeed(a)

  def fail(reason: String): Parser[Nothing] = Fail(reason)

  val anyChar: Parser[Char] = CharIf(_ => true)

  def char(c: Char): Parser[Char] = Sequence(c.toString.getBytes).as(c)

  def string(string: String): Parser[String] = Sequence(string.getBytes).as(string)

  def sequence(seq: Seq[Byte]): Parser[Seq[Byte]] = Sequence(seq)

  def byte(b: Byte): Parser[Byte] = ByteIf(_ == b)

  def anyLong: Parser[Long] = anyChar.suchThat(_.isDigit, "Not a digit").repeatedly
    .suchThat(_.nonEmpty, "Digit expected but not found.")
    .map(_.mkString)
    .flatMap { str =>
      str.toLongOption match {
        case Some(value) => Parser.succeed(value)
        case None => Parser.fail(s"$str could not be casted to Long")
      }
    }

  val anyByte: Parser[Byte] = ByteIf(_ => true)

  private final case class Succeed[A](a: A) extends Parser[A]

  private final case class Fail(reason: String) extends Parser[Nothing]

  private final case class FlatMap[A, B](parser: Parser[A], f: A => Parser[B]) extends Parser[B]

  private final case class Fold[A, B](parser: Parser[A], success: A => Parser[B], failure: String => Parser[B]) extends Parser[B]

  private case class Sequence(seq: Seq[Byte]) extends Parser[Seq[Byte]]

  private case class ByteIf(predicate: Byte => Boolean) extends Parser[Byte]

  private case class CharIf(predicate: Char => Boolean) extends Parser[Char]
}
