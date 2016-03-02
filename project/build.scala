import sbt._
import Keys._

object CashierBuild extends Build {

  lazy val cashier = (project in file("."))
    .settings(
      libraryDependencies ++= Seq(
        lib.jackson.databind,
        lib.jackson.module_scala,
        lib.scalaTest % "test"
      )
    )
}

