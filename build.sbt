ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2022",
    idePackagePrefix := Some("com.github.mideo"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    scalacOptions ++= Seq(
      "-unchecked",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-deprecation",
      "-encoding",
      "utf8"
    ),
    javaOptions in run ++= Seq("-Xms2G", "-Xmx4G")
  )
