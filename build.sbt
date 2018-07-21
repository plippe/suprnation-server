val catsVersion = "1.1.0"
val utestVersion = "0.6.3"

scalaVersion := "2.12.6"
scalacOptions ++= ScalacOptions.tpolecat

scalafmtOnCompile := true

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "com.lihaoyi" %% "utest" % utestVersion % Test
testFrameworks += new TestFramework("utest.runner.Framework")
