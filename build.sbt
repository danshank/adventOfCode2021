val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventOfCode2021",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    scalacOptions ++= Seq(
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-language:postfixOps"
    )
  )
