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
import cats.implicits._
import cats.data._
import org.dbersenev.fs2.xml.conversion.ElementConversion

import scala.language.implicitConversions
import scala.xml.{Elem, Node}

object TestConversion {


  case class Record(name: String, sr:List[String])

  val el = <record name="test">
    <sub at1 ="1">hello</sub>
    <sub at1 ="2">hello</sub>
    <sub at1 ="3">hello</sub>
    <sub at1 ="4">hello</sub>
    <sub at1 ="5">hello</sub>
  </record>


  def main(argv: Array[String]): Unit = {

    import conversion.ElementConversion._


    implicit val toRecord:ElementConversion[Record] = el => (
      el attrAsRequired[String]  "name",
      (el \ "sub" \ "@at1").fromSeqAs[String]
    ).mapN(Record)

    val r:ConversionResult[Record] = el.as[Record]

    println(el \ "sub" \\ "@at1")

  }

}
