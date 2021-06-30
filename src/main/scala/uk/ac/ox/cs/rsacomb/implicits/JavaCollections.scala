/*
 * Copyright 2020, 2021 KRR Oxford
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

package uk.ac.ox.cs.rsacomb.implicits

import java.util.stream.{Collectors, Stream}
import scala.collection.JavaConverters._

object JavaCollections {

  implicit def javaToScalaList[A](list: java.util.List[A]): List[A] =
    list.asScala.toList

  implicit def scalaToJavaList[A](list: List[A]): java.util.List[A] =
    list.asJava

  implicit def javaSetToScalaList[A](set: java.util.Set[A]): List[A] =
    set.asScala.toList

  implicit def javaStreamToScalaSeq[A](
      stream: java.util.stream.Stream[A]
  ): Seq[A] =
    stream.collect(Collectors.toList()).asScala.toSeq

  implicit def javaCollectionToScalaList[A](
      set: java.util.Collection[A]
  ): List[A] =
    set.asScala.toList

  implicit def scalaSeqTojavaCollection[A](
      seq: Seq[A]
  ): java.util.Collection[A] = seq.asJavaCollection
}
