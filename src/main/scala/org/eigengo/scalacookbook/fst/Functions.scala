package org.eigengo.scalacookbook.fst

trait Functions {
  type Result = Unit

  def foo(i: Int, j: Int): Result = ???
  def fooc(i: Int)(j: Int): Result = ???
  def foom[M[_]](m: M[_]): Result = m match {
    case opt: Option[_] ⇒ println("Opt"); ()
    case xs: List[_] ⇒ println("List"); ()
  }

  def fmap[M[_], A, B](f: A ⇒ B, m: M[A])(implicit F: Functor[M]): M[B] = F.fmap(f, m)

  trait Functor[M[_]] {
    def fmap[A, B](f: A ⇒ B, m: M[A]): M[B]
  }

  implicit object OptionFunctor extends Functor[Option] {
    override def fmap[A, B](f: (A) ⇒ B, m: Option[A]): Option[B] = m match {
      case Some(a) ⇒ Some(f(a))
      case None ⇒ None
    }
  }

  implicit object ListFunctor extends Functor[List] {
    override def fmap[A, B](f: (A) ⇒ B, m: List[A]): List[B] = m match {
      case Nil ⇒ Nil
      case h::t ⇒ f(h)::fmap(f, t)
    }
  }

  implicit object EitherSFunctor extends Functor[({type L[R] = Either[String, R]})#L] {
    override def fmap[A, B](f: (A) ⇒ B, m: Either[String, A]): Either[String, B] = m match {
      case Left(e) ⇒ Left(e)
      case Right(x) ⇒ Right(f(x))
    }
  }


}

object FunctionsMain extends Functions with App {
  type EitherS[R] = Either[String, R]

//  val x = foo _
//  val x1 = foo(_, _)
  val x2 = foo(1, _: Int)
//
//  val r = ({val f = foo(1, _: Int); f})(2)

  foom(List(1, 2, 3))
  foom(Some("x"))
  //foom[EitherS](Right(1))
  //foom[Either[String, _]]
  //foom[({type L[R] = Either[String, R]})#L](Right(2))

  def addOne(i: Int): Int = i + 1
  println( fmap(addOne, List(1, 2, 3)) )

}

