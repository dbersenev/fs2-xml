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

import scala.language.implicitConversions

case class SelectorElement(
                            entry: String,
                            stopOnAdjacent: Boolean = true, //do not process adjacent elements
                            ns: Option[String] = None,
                            attrs: List[SelectorAttribute] = List.empty
                          ) {

  def allowingAdjacent: SelectorElement = this.copy(stopOnAdjacent = false)

  def withNs(namespace: String): SelectorElement = this.copy(ns = Some(namespace))

  def withAttr(a: SelectorAttribute): SelectorElement = this.copy(attrs = a :: attrs)

}
