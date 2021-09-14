package org.scalacheck.cats.instances

import cats._
import cats.data._
import cats.syntax.all._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.cats.implicits._

import scala.collection.immutable._

private[instances] class PartiallyAppliedFromOneAnd[G[_]](val dummy: Unit = ()) extends AnyVal {
  def apply[F[_], A : Arbitrary](f: (A, F[A]) => G[A])
                                (implicit F: Arbitrary[F[A]]): Gen[G[A]] =
    arbitrary[OneAnd[F, A]].map { case OneAnd(head, tail) => f(head, tail) }
}

trait Arbitraries {
  def genOneAnd[F[_], A: Arbitrary](implicit F: Arbitrary[F[A]]): Gen[OneAnd[F, A]] =
    (arbitrary[A], arbitrary[F[A]]).mapN(OneAnd(_, _))

  implicit def arbOneAnd[F[_], A: Arbitrary](implicit F: Arbitrary[F[A]]): Arbitrary[OneAnd[F, A]] = Arbitrary(genOneAnd[F, A])

  private def fromOneAnd[G[_]] = new PartiallyAppliedFromOneAnd[G]

  def genNonEmptyList[A: Arbitrary]: Gen[NonEmptyList[A]] =
    fromOneAnd[NonEmptyList](NonEmptyList[A](_, _))

  implicit def arbNonEmptyList[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Arbitrary(genNonEmptyList[A])

  def genChain[A: Arbitrary]: Gen[Chain[A]] =
    arbitrary[List[A]].map(Chain.fromSeq)

  implicit def arbChain[A: Arbitrary]: Arbitrary[Chain[A]] = Arbitrary(genChain[A])

  def genNonEmptyChain[A: Arbitrary]: Gen[NonEmptyChain[A]] =
    fromOneAnd[NonEmptyChain](NonEmptyChain.fromChainPrepend[A])

  implicit def arbNonEmptyChain[A: Arbitrary]: Arbitrary[NonEmptyChain[A]] = Arbitrary(genNonEmptyChain[A])

  def genNonEmptySeq[A: Arbitrary]: Gen[NonEmptySeq[A]] =
    fromOneAnd[NonEmptySeq](NonEmptySeq[A](_, _))

  implicit def arbNonEmptySeq[A: Arbitrary]: Arbitrary[NonEmptySeq[A]] = Arbitrary(genNonEmptySeq[A])

  def genNonEmptySet[A: Arbitrary : Order]: Gen[NonEmptySet[A]] =
    fromOneAnd[NonEmptySet]((head: A, tail: Seq[A]) => NonEmptySet.of(head, tail: _*))

  implicit def arbNonEmptySet[A: Arbitrary : Order]: Arbitrary[NonEmptySet[A]] = Arbitrary(genNonEmptySet[A])

  def genNonEmptyVector[A: Arbitrary]: Gen[NonEmptyVector[A]] =
    fromOneAnd[NonEmptyVector](NonEmptyVector[A](_, _))

  implicit def arbNonEmptyVector[A: Arbitrary]: Arbitrary[NonEmptyVector[A]] = Arbitrary(genNonEmptyVector[A])

  def genConst[A: Arbitrary, B]: Gen[Const[A, B]] =
    arbitrary[A].map(Const(_))

  implicit def arbConst[A: Arbitrary, B]: Arbitrary[Const[A, B]] = Arbitrary(genConst[A, B])

  def genNonEmptyMap[K : Arbitrary : Order, V: Arbitrary]: Gen[NonEmptyMap[K, V]] =
    arbitrary[OneAnd[List, (K, V)]].map { case OneAnd(head, tail) => NonEmptyMap.of(head, tail: _*) }

  implicit def arbNonEmptyMap[K : Arbitrary : Order, V: Arbitrary]: Arbitrary[NonEmptyMap[K, V]] = Arbitrary(genNonEmptyMap[K, V])

  def genEitherT[F[_] : Applicative, A, B](implicit arb: Arbitrary[Either[A, B]]): Gen[EitherT[F, A, B]] =
    arbitrary[Either[A, B]].map(EitherT.fromEither[F](_))

  implicit def arbEitherT[F[_] : Applicative, A, B](implicit arb: Arbitrary[Either[A, B]]): Arbitrary[EitherT[F, A, B]] = Arbitrary(genEitherT[F, A, B])

  def genOptionT[F[_] : Applicative, A](implicit arb: Arbitrary[Option[A]]): Gen[OptionT[F, A]] =
    arbitrary[Option[A]].map(OptionT.fromOption[F](_))

  implicit def arbOptionT[F[_] : Applicative, A, B](implicit arb: Arbitrary[Option[A]]): Arbitrary[OptionT[F, A]] = Arbitrary(genOptionT[F, A])
}
