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

import cats._
import cats.effect._
import cats.implicits._
import fs2._
import javax.xml.stream.events.XMLEvent
import org.dbersenev.fs2.xml.parsing.EventStream

import scala.language.higherKinds

object StreamFunctions {

  implicit val cs:ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)

  def byteStream(src:String):Stream[IO,Byte] =
    Stream.emit(src).through(text.utf8Encode)

  def eventStream(src:String):Stream[IO,XMLEvent] =
    Stream.resource(Blocker[IO])
      .flatMap(b => byteStream(src).through(EventStream(b)))
}
