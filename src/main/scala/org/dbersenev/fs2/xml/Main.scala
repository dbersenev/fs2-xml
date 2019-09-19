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

import java.io.StringReader
import java.nio.file.Paths
import java.util.concurrent.Executors

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fs2._
import javax.xml.stream.XMLEventFactory
import javax.xml.stream.events.{Characters, XMLEvent}
import org.dbersenev.fs2.xml.parsing.options.SelOpt

import scala.xml._
import parsing._

import scala.concurrent.ExecutionContext


object Main extends IOApp {


  override def run(args: List[String]): IO[ExitCode] = {
    import scala.concurrent.ExecutionContext._
    import XMLSelector._

    val s =
      """
        |<root>
        | <wrap>
        | <k1/>
        | <k2/>
        | <k3/>
        | <item><x>a</x><y/><x>b</x></item>
        | <item><x>c</x><x>d</x><f/><x>e</x></item>
        | <elem1><![CDATA[test1]]><some>value</some></elem1>
        | <elem2>test2</elem2>
        | <elem3>test3</elem3>
        | </wrap>
        | <wrap><nope/></wrap>
        | <end></end>
        |</root>
      """.stripMargin

    val props:Set[SelOpt] = Set.empty//Set(options.StopBefore("root/wrap/elem2".selPath))
    val selector = XMLSelector(root("REGAPI_INPUT") |\| "DATA_UNIT" , props)

    //val eFactory = XMLEventFactory.newFactory()

    val cs = IO.contextShift(global)

    def readFile(bc:ExecutionContext):IO[Unit] =
      io.file.readAll[IO](Paths.get("xml/0000-0001-8880-7084_ORSInput.xml"), bc, 1024)
        .through(EventStream[IO](global))
        .through(SelectedEventStream(selector))
        .through(ElementStream.apply)
        .map(_ \ "SOURCEID")
        .map(_.toString).intersperse("\n").through(text.utf8Encode).through(io.stdout(global))
        .compile.drain

    Resource.make(IO.delay(Executors.newFixedThreadPool(1)))(e => IO.delay(e.shutdownNow()))
        .flatMap(e => Resource.liftF(IO.delay(ExecutionContext.fromExecutor(e)))).use(bc =>
      cs.evalOn(bc)(IO(Thread.sleep(5000))).start >>
      List(readFile(bc), readFile(bc), readFile(bc), readFile(bc), readFile(bc), readFile(bc)).parSequence.as(Unit)
    ).as(ExitCode.Success)
  }

}
