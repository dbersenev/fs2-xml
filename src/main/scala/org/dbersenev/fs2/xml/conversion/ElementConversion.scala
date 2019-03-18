/*
 * Copyright [2019] [Dmitry Bersenev]
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

package org.dbersenev.fs2.xml.conversion


import cats._
import cats.data.{Validated, ValidatedNec}
import cats.implicits._

import scala.xml.{Elem, Node, NodeSeq}

trait ElementConversion[T] {
  def converted(el: Node): ElementConversion.ConversionResult[T]
}

object ElementConversion {

  type ConversionResult[A] = ValidatedNec[String, A]

  def apply[T](el:Node)(implicit cnv:ElementConversion[T]):ConversionResult[T] = cnv.converted(el)

  implicit val xmlContentAttrToStringConv:ElementConversion[String] = n => Validated.valid(n.text)

  implicit class NodeExtensions(val ns:NodeSeq) extends AnyVal {
    def as[T:ElementConversion]:ConversionResult[Option[T]] = ns.headOption.traverse(_.as[T])

    def asRequired[T:ElementConversion]:ConversionResult[T] = as[T].andThen(v => Validated.condNec(v.isDefined, v.get, "value is empty"))

    def fromSeqAs[T:ElementConversion]:ConversionResult[List[T]] = ns.toList.collect{
      case e:Elem => e
    }.traverse(_.as[T])

    def attrAs[T:ElementConversion](name:String):ConversionResult[Option[T]] = (ns \ s"@$name").as[T]
    def attrAsRequired[T:ElementConversion](name:String):ConversionResult[T] = ns.attrAs[T](name).andThen(v => Validated.condNec(v.isDefined, v.get, s"attribute $name not found"))
  }

  implicit class NodeExtensionsUtils(val ns:NodeSeq) extends AnyVal {
    def asString:ConversionResult[Option[String]] = ns.as[String]
    def asStringRequired:ConversionResult[String] = ns.asRequired[String]
  }

  implicit class NodeSeqExtensions(val n:Node) extends AnyVal {
    def as[T:ElementConversion]:ConversionResult[T] = ElementConversion[T](n)
  }

}
