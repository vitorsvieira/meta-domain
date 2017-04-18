/*
 * Copyright 2017 Vitor S. Vieira
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

package com.vitorsvieira.shapeless.labelledgeneric.bigclass

import cats.Monoid
import cats.instances.all._
import shapeless._
import shapeless.labelled.{ field, FieldType }
import shapeless.ops.hlist._

/**
  * Type class for converting values of type A
  * to another type B.
  */
trait Migration[A, B] {
  def apply(original: A): B
}

/** Type class instances for Migration */
object Migration {

  /**
    * This single rule covers
    * the following changes to product types:
    *
    * - fields that are in A but not in B;
    * - fields that are in B but not in A
    *   (provided we can calculate "empty" values as defaults)
    * - reorderings of fields
    *
    * The rule does *not* cover:
    *
    * - renaming fields
    * - changing the types of fields
    *
    * (these would make it a heuristic process)
    */
  implicit def genericMigration[A,
                                B,
                                ARepr <: HList,
                                BRepr <: HList,
                                CommonFields <: HList,
                                AddedFields <: HList,
                                Unaligned <: HList](
      implicit aGen: LabelledGeneric.Aux[A, ARepr],
      bGen: LabelledGeneric.Aux[B, BRepr],
      inter: Intersection.Aux[ARepr, BRepr, CommonFields],
      diff: Diff.Aux[BRepr, CommonFields, AddedFields],
      empty: Empty[AddedFields], // see below
      prepend: Prepend.Aux[AddedFields, CommonFields, Unaligned],
      align: Align[Unaligned, BRepr]
  ): Migration[A, B] =
    new Migration[A, B] {
      def apply(a: A): B = {
        val aRepr     = aGen.to(a)
        val common    = inter(aRepr)
        val added     = empty.value
        val unaligned = prepend(added, common)
        val bRepr     = align(unaligned)
        bGen.from(bRepr)
      }
    }
}

/**
  * Helper type class for computing "empty" values
  * fill in new fields.
  */
case class Empty[A](value: A)

object Empty {

  /**
    * By default we rely on Cats' Monoid
    * to provide empty values.
    *
    * This has instances for the most common types:
    * Int, String, List, Option, etc.
    *
    * We could probably generate Monoid instances
    * for HNil and :: directly.
    * I haven't tried this yet.
    * Maybe github.com/milessabin/kittens provides them?
    */
  implicit def monoidEmpty[A](implicit monoid: Monoid[A]): Empty[A] =
    Empty(monoid.empty)

  implicit def hnilEmpty: Empty[HNil] =
    Empty(HNil)

  implicit def hlistEmpty[K <: Symbol, H, T <: HList](
      implicit hEmpty: Lazy[Empty[H]],
      tEmpty: Empty[T]
  ): Empty[FieldType[K, H] :: T] =
    Empty(field[K](hEmpty.value.value) :: tEmpty.value)
}

case class Foo(
    one: String,
    two: Int,
    three: Boolean,
    field1: String,
    field2: String,
    field3: String,
    field4: String,
    field5: String,
    field6: String,
    field7: Int,
    field8: Double,
    field9: Double,
    field10: String,
    field11: Int,
    field12: Double,
    field13: Double,
    field14: String
)

case class Bar(
    three: Boolean,
    one: String,
    field1: String,
    field2: String,
    field3: String,
    field4: String,
    field5: String,
    field6: String,
    field7: Int,
    field8: Double,
    field9: Double,
    field10: String
)

object Main extends App {
  implicit class MigrationOps[A](original: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration(original)
  }

  println(
    Foo(
      one = "One",
      two = 2,
      three = false,
      field1 = "test",
      field2 = "test",
      field3 = "test",
      field4 = "test",
      field5 = "test",
      field6 = "test",
      field7 = 1,
      field8 = 2.0,
      field9 = 2.0,
      field10 = "test",
      field11 = 1,
      field12 = 2.0,
      field13 = 2.0,
      field14 = "test"
    ).migrateTo[Bar]
  )
}
