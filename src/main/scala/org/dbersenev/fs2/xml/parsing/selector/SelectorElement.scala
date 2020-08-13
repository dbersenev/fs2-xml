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

import cats.implicits.catsSyntaxOptionId
import org.dbersenev.fs2.xml.events.Attribute
import org.dbersenev.fs2.xml.parsing.selector.Selector.ExtractedAttr

import scala.language.implicitConversions

case class SelectorElement(
                               entry: String,
                               stopOnAdjacent: Boolean = true, //do not process adjacent elements
                               ns: Option[String] = None,
                               attrs: List[Attribute] = List.empty
                             ) {

  lazy val compiledAttrs: Map[String, Attribute] = attrs.map(a => (a.name, a)).toMap

  def allowingAdjacent: SelectorElement = this.copy(stopOnAdjacent = false)

  def withNs(namespace: String): SelectorElement = this.copy(ns = Some(namespace))

  def withAttr(a:Attribute): SelectorElement = this.copy(attrs = a :: attrs)

  def attributesMatches(other: Set[ExtractedAttr]): Boolean = other.forall(extAttr => compiledAttrs.get(extAttr.name)
    .forall(_.value.exists(_ == extAttr.value))
  )
}

object SelectorElement {

  implicit class XmlSelectorAttributeExt(val a:Attribute) extends AnyVal {
    def withIntValue(v:Int):Attribute = a.copy(value = v.toString.some)
  }

  implicit class TripleToElAndAttr(val trp:(String,String,String)) extends AnyVal {
    def toSelElem: SelectorElement = SelectorElement(
      trp._1,
      stopOnAdjacent = false,
      ns = None,
      attrs = Attribute(trp._2, trp._3.some) :: Nil
    )
  }

  implicit def tripleToElAndAttr(trp:(String,String,String)):SelectorElement = trp.toSelElem

  implicit class AnyStrToXmlSelEl(val s: String) extends AnyVal {
    def toSelElem: SelectorElement = SelectorElement(s)
  }

  implicit def anyStrToXmlSelEl(s:String):SelectorElement = SelectorElement(s)

}
