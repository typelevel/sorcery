/*
 * Copyright 2020 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.sorcery.cats

import cats.Applicative
import cats.ApplicativeError
import cats.Monad
import cats.MonadError
import org.typelevel.sorcery.GenK
import org.typelevel.sorcery.Generators1

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import cats.implicits._

//Applicative is the first place that lets us introduce values in the context, if we discount InvariantMonoidal
trait ApplicativeGenerators[F[_]] extends Generators1[F] {
  implicit val F: Applicative[F]

  protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[F[A]])] =
    List("pure" -> genPure[A])

  protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[F]): List[(String, Gen[F[A]])] =
    List(
      "map" -> genMap[A](deeper),
      "ap" -> genAp[A](deeper)
    )

  private def genPure[A: Arbitrary]: Gen[F[A]] =
    arbitrary[A].map(_.pure[F])

  private def genMap[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for {
      fa <- deeper[A]
      f <- arbitrary[A => A]
    } yield F.map(fa)(f)

  private def genAp[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for {
      fa <- deeper[A]
      ff <- deeper[A => A]
    } yield F.ap(ff)(fa)
}

trait MonadGenerators[F[_]] extends ApplicativeGenerators[F] {

  implicit val F: Monad[F]

  override protected def recursiveGen[A: Arbitrary: Cogen](
      deeper: GenK[F]): List[(String, Gen[F[A]])] =
    List(
      "flatMap" -> genFlatMap(deeper)
    ) ++ super.recursiveGen(deeper)

  private def genFlatMap[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for {
      fa <- deeper[A]
      f <- Gen.function1[A, F[A]](deeper[A])
    } yield fa.flatMap(f)
}

trait ApplicativeErrorGenerators[F[_], E] extends ApplicativeGenerators[F] {
  implicit val arbitraryE: Arbitrary[E]
  implicit val cogenE: Cogen[E]

  implicit val F: ApplicativeError[F, E]

  override protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[F[A]])] =
    List(
      "raiseError" -> genRaiseError[A]
    ) ++ super.baseGen[A]

  override protected def recursiveGen[A: Arbitrary: Cogen](
      deeper: GenK[F]): List[(String, Gen[F[A]])] =
    List(
      "handleErrorWith" -> genHandleErrorWith[A](deeper)
    ) ++ super.recursiveGen[A](deeper)

  private def genRaiseError[A]: Gen[F[A]] =
    arbitrary[E].map(F.raiseError[A](_))

  private def genHandleErrorWith[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for {
      fa <- deeper[A]
      f <- Gen.function1[E, F[A]](deeper[A])
    } yield F.handleErrorWith(fa)(f)
}

trait MonadErrorGenerators[F[_], E]
    extends MonadGenerators[F]
    with ApplicativeErrorGenerators[F, E] {
  implicit val F: MonadError[F, E]
}
