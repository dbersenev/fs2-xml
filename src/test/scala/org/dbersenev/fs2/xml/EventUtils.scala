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

package org.dbersenev.fs2.xml

import cats.implicits.catsSyntaxOptionId
import javax.xml.stream.XMLEventFactory
import javax.xml.stream.events.{Characters, EndElement}

object EventUtils {

  private val evFact = XMLEventFactory.newFactory()

  type TestEvent = (Option[String], String, Option[String])

  def startElementEvent(name:String):TestEvent = (name.some, "start", None)
  def charactersEvent(value:String):TestEvent = (None, "characters", value.some)
  def endElementEvent(name:String):TestEvent = (name.some, "end", None)
}
