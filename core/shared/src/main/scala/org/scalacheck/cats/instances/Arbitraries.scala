package org.scalacheck.cats.instances

import cats._
import cats.data._
import cats.syntax.all._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.cats.implicits._
import org.scalacheck.cats.instances.PartiallyAppliedFromOneAnd.fromOneAnd

import scala.collection.immutable._

trait Arbitraries extends Arbitraries1 with GenInstancesForCatsData

trait GenInstancesForCatsData {
  def genOneAnd[F[_], A: Arbitrary](implicit F: Arbitrary[F[A]]): Gen[OneAnd[F, A]] =
    (arbitrary[A], arbitrary[F[A]]).mapN(OneAnd(_, _))

  def genNonEmptyList[A: Arbitrary]: Gen[NonEmptyList[A]] =
    fromOneAnd[NonEmptyList](NonEmptyList[A](_, _))

  def genChain[A: Arbitrary]: Gen[Chain[A]] =
    arbitrary[List[A]].map(Chain.fromSeq)

  def genNonEmptyChain[A: Arbitrary]: Gen[NonEmptyChain[A]] =
    fromOneAnd[NonEmptyChain](NonEmptyChain.fromChainPrepend[A])

  def genNonEmptySeq[A: Arbitrary]: Gen[NonEmptySeq[A]] =
    fromOneAnd[NonEmptySeq](NonEmptySeq[A](_, _))

  def genNonEmptySet[A: Arbitrary: Order]: Gen[NonEmptySet[A]] =
    fromOneAnd[NonEmptySet]((head: A, tail: Seq[A]) => NonEmptySet.of(head, tail: _*))

  def genNonEmptyVector[A: Arbitrary]: Gen[NonEmptyVector[A]] =
    fromOneAnd[NonEmptyVector](NonEmptyVector[A](_, _))

  def genConst[A: Arbitrary, B]: Gen[Const[A, B]] =
    arbitrary[A].map(Const(_))

  def genNonEmptyMap[K: Arbitrary: Order, V: Arbitrary]: Gen[NonEmptyMap[K, V]] =
    arbitrary[OneAnd[List, (K, V)]].map {
      case OneAnd(head, tail) => NonEmptyMap.of(head, tail: _*)
    }

  def genEitherTWithErrorChannel[F[_], A: Arbitrary, B: Arbitrary, C: Arbitrary](
      implicit F: ApplicativeError[F, C]): Gen[EitherT[F, A, B]] =
    arbitrary[Either[C, Either[A, B]]].map {
      case Left(c) => EitherT(c.raiseError[F, Either[A, B]])
      case Right(ab) => EitherT.fromEither[F](ab)
    }

  def genEitherT[F[_]: Applicative, A, B](
      implicit arb: Arbitrary[Either[A, B]]): Gen[EitherT[F, A, B]] =
    arbitrary[Either[A, B]].map(EitherT.fromEither[F](_))

  def genOptionTWithErrorChannel[F[_], A: Arbitrary, C: Arbitrary](
      implicit F: ApplicativeError[F, C]): Gen[OptionT[F, A]] =
    arbitrary[Either[C, Option[A]]].map {
      case Left(c) => OptionT(c.raiseError[F, Option[A]])
      case Right(a) => OptionT.fromOption[F](a)
    }

  def genOptionT[F[_]: Applicative, A](implicit arb: Arbitrary[Option[A]]): Gen[OptionT[F, A]] =
    arbitrary[Option[A]].map(OptionT.fromOption[F](_))
}

sealed private[instances] trait Arbitraries1 extends Arbitraries0 { this: GenInstancesForCatsData =>
  implicit def arbOneAnd[F[_], A: Arbitrary](implicit F: Arbitrary[F[A]]): Arbitrary[OneAnd[F, A]] =
    Arbitrary(genOneAnd[F, A])

  implicit def arbNonEmptyList[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(genNonEmptyList[A])

  implicit def arbChain[A: Arbitrary]: Arbitrary[Chain[A]] = Arbitrary(genChain[A])

  implicit def arbNonEmptyChain[A: Arbitrary]: Arbitrary[NonEmptyChain[A]] =
    Arbitrary(genNonEmptyChain[A])

  implicit def arbNonEmptySeq[A: Arbitrary]: Arbitrary[NonEmptySeq[A]] =
    Arbitrary(genNonEmptySeq[A])

  implicit def arbNonEmptySet[A: Arbitrary: Order]: Arbitrary[NonEmptySet[A]] =
    Arbitrary(genNonEmptySet[A])

  implicit def arbNonEmptyVector[A: Arbitrary]: Arbitrary[NonEmptyVector[A]] =
    Arbitrary(genNonEmptyVector[A])

  implicit def arbConst[A: Arbitrary, B]: Arbitrary[Const[A, B]] = Arbitrary(genConst[A, B])

  implicit def arbNonEmptyMap[K: Arbitrary: Order, V: Arbitrary]: Arbitrary[NonEmptyMap[K, V]] =
    Arbitrary(genNonEmptyMap[K, V])

  implicit def arbEitherTWithErrorChannel[F[_], A: Arbitrary, B: Arbitrary, C: Arbitrary](
      implicit F: ApplicativeError[F, C]): Arbitrary[EitherT[F, A, B]] =
    Arbitrary(genEitherTWithErrorChannel[F, A, B, C])

}

sealed private[instances] trait Arbitraries0 { this: GenInstancesForCatsData =>
  implicit def arbEitherT[F[_]: Applicative, A, B](
      implicit arb: Arbitrary[Either[A, B]]): Arbitrary[EitherT[F, A, B]] =
    Arbitrary(genEitherT[F, A, B])

  implicit def arbOptionT[F[_]: Applicative, A, B](
      implicit arb: Arbitrary[Option[A]]): Arbitrary[OptionT[F, A]] = Arbitrary(genOptionT[F, A])
}

private[instances] final class PartiallyAppliedFromOneAnd[G[_]](val dummy: Unit = ())
    extends AnyVal {
  def apply[F[_], A: Arbitrary](f: (A, F[A]) => G[A])(implicit F: Arbitrary[F[A]]): Gen[G[A]] =
    arbitrary[OneAnd[F, A]].map { case OneAnd(head, tail) => f(head, tail) }
}

private[instances] object PartiallyAppliedFromOneAnd {
  private[instances] def fromOneAnd[G[_]] = new PartiallyAppliedFromOneAnd[G]
}
