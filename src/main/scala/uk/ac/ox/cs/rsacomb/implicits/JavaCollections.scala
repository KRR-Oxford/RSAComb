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
