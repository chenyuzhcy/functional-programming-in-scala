name := "functional-programming-in-scala"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-Ypartial-unification",             // Enable partial unification in type constructor inference
  "-language:higherKinds",             // Allow higher-kinded types
)