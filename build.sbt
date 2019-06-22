name := "TryScala213"

version := "0.1"

scalaVersion in ThisBuild := "2.13.0"
organization in ThisBuild := "mrvplusone.github.io"

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds",
  "-deprecation",
  "-Ymacro-annotations"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.1.3",
  "com.lihaoyi" %% "scalaparse" % "2.1.3",
  "com.lihaoyi" %% "pythonparse" % "2.1.3",
  "org.scalacheck" %% "scalacheck" % "1.14.0",
//  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "com.lihaoyi" %% "ammonite-ops" % "1.6.8",
//  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.typelevel" %% "cats-core" % "2.0.0-M4",
  "org.typelevel" %% "cats-free" % "2.0.0-M4",
//  "org.typelevel" %% "cats-mtl-core" % "0.4.0",
  "org.typelevel" %% "cats-effect" % "2.0.0-M4",
  "com.github.mpilquist" %% "simulacrum" % "0.19.0",
  "org.typelevel" %% "cats-laws" % "2.0.0-M4",
//  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.0",
  "com.lihaoyi" %% "utest" % "0.7.1" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")
