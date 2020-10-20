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

package org.typelevel.sorcery

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import scala.collection.immutable.SortedMap

trait GenK[F[_]] {
  def apply[A: Arbitrary: Cogen]: Gen[F[A]]
}

// Generators for * -> * kinded types
trait Generators1[F[_]] {
  protected val maxDepth: Int = 10

  //todo: uniqueness based on... names, I guess. Have to solve the diamond problem somehow

  //Generators of base cases, with no recursion
  protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[F[A]])]

  //Only recursive generators - the argument is a generator of the next level of depth
  protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[F]): List[(String, Gen[F[A]])]

  //All generators possible at depth [[depth]]
  private def gen[A: Arbitrary: Cogen](depth: Int): Gen[F[A]] = {
    val genK: GenK[F] = new GenK[F] {
      def apply[B: Arbitrary: Cogen]: Gen[F[B]] = Gen.delay(gen(depth + 1))
    }

    val gens =
      if (depth > maxDepth) baseGen[A]
      else baseGen[A] ++ recursiveGen[A](genK)

    Gen.oneOf(SortedMap(gens: _*).map(_._2)).flatMap(identity)
  }

  //All generators possible at depth 0 - the main public method
  def generators[A: Arbitrary: Cogen]: Gen[F[A]] = gen[A](0)

  def compile: GenK[F] =
    new GenK[F] {
      def apply[A: Arbitrary: Cogen]: Gen[F[A]] = generators[A]
    }

  ///////////////////////////
  ///////////////////////////
  // watch this: new thing //
  ///////////////////////////
  ///////////////////////////
  object arbitary {
    implicit def arbitraryFromGenerators1[A: Arbitrary: Cogen]: Arbitrary[F[A]] =
      Arbitrary(generators[A])
  }
}
