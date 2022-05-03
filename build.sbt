lazy val root = project
  .in(file("."))
  .settings(
    name := "Logics",
    version := "0.1.1",
    scalaVersion := "3.1.3-RC1-bin-20220218-29f9d33-NIGHTLY",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
    libraryDependencies += "be.adamv" %% "picograph" % "0.1.2"
  )
