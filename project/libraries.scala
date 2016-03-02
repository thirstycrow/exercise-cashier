import sbt._

object v {
  val jackson = "2.7.2"
  val scalaTest = "2.2.4"
}

object lib {

  object jackson {
    val databind     = "com.fasterxml.jackson.core"   %  "jackson-databind"     % v.jackson
    val module_scala = "com.fasterxml.jackson.module" %% "jackson-module-scala" % v.jackson
  }

  val scalaTest = "org.scalatest" %% "scalatest" % v.scalaTest
}
