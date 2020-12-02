package uk.ac.ox.cs.rsacomb.implicits

import scala.collection.JavaConverters._

object JavaCollections {

  implicit def javaToScalaList[A](list: java.util.List[A]): List[A] =
    list.asScala.toList

  implicit def scalaToJavaList[A](list: List[A]): java.util.List[A] =
    list.asJava

  implicit def javaSetToScalaList[A](set: java.util.Set[A]): List[A] =
    set.asScala.toList

  implicit def javaCollectionToScalaList[A](
      set: java.util.Collection[A]
  ): List[A] =
    set.asScala.toList

  implicit def scalaSeqTojavaCollection[A](
      seq: Seq[A]
  ): java.util.Collection[A] = seq.asJavaCollection
}
