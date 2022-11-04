ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "type-inference",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"        % "2.0.3",
      "dev.zio" %% "zio-parser" % "0.1.7",
      "dev.zio" %% "zio-test"   % "2.0.3" % Test,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
