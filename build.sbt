val utestVersion = "0.6.3"

scalaVersion := "2.12.6"
scalacOptions ++= ScalacOptions.tpolecat

scalafmtOnCompile := true

libraryDependencies += "com.lihaoyi" %% "utest" % utestVersion % Test
testFrameworks += new TestFramework("utest.runner.Framework")
