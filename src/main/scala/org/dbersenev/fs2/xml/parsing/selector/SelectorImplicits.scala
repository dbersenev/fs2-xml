/*
 * Copyright [2020] [Dmitry Bersenev]
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

package org.dbersenev.fs2.xml.parsing.selector

import cats.data.NonEmptyVector

import scala.language.implicitConversions
import cats.syntax.option._

object SelectorImplicits {

  implicit class PathBuilderToSelector(val pb: SelectorPathBuilder) extends AnyVal {
    def toSelector: Selector = Selector(pb.path)
  }

  implicit class XmlSelectorAttributeExt(val a: SelectorAttribute) extends AnyVal {
    def withIntValue(v: Int): SelectorAttribute = a.copy(value = v.toString.some)
  }

  implicit class TripleToElAndAttr(val trp: (String, String, String)) extends AnyVal {
    def toSelElem: SelectorElement = SelectorElement(
      trp._1,
      stopOnAdjacent = false,
      ns = None,
      attrs = SelectorAttribute(trp._2, trp._3.some) :: Nil
    )
  }

  implicit class AnyStrToXmlSelEl(val s: String) extends AnyVal {
    def toSelElem: SelectorElement = SelectorElement(s)
  }

  implicit class StringToPathExt(val path: String) extends AnyVal {
    def toSelPath: NonEmptyVector[SelectorElement] = NonEmptyVector.fromVector(
      path.split("/")
        .map(_.trim)
        .filter(_.nonEmpty)
        .zipWithIndex
        .map {
          case (el, 0) => SelectorElement(el)
          case (el, _) => SelectorElement(el, stopOnAdjacent = false)
        }
        .toVector
    ).get
  }

  implicit def pathBuilderToSelector(pb: SelectorPathBuilder): Selector = Selector(pb.path)

  implicit def vecPathToSelector(path: NonEmptyVector[SelectorElement]): Selector = Selector(path)

  implicit def tripleToElAndAttr(trp: (String, String, String)): SelectorElement = trp.toSelElem

  implicit def anyStrToXmlSelEl(s: String): SelectorElement = SelectorElement(s)

  implicit def builderToVecExtr(builder: SelectorPathBuilder): NonEmptyVector[SelectorElement] = builder.path
}
