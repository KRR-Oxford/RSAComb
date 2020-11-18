package rsacomb.implicits

import scala.collection.JavaConverters._

object JavaCollections {

  implicit def javaToScalaList[A](list: java.util.List[A]): List[A] =
    list.asScala.toList

  implicit def scalaToJavaList[A](list: List[A]): java.util.List[A] =
    list.asJava

}
