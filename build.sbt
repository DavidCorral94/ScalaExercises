name := "ScalaExercises"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "com.47deg" %% "scalacheck-toolbox-datetime" % "0.5.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.8.0"

// For type refinement exercise at Functional Domain Modelling training
libraryDependencies += "eu.timepit" %% "refined" % "0.9.26"

libraryDependencies += "com.codecommit"    %% "cats-effect-testing-scalatest" % "0.5.2"

addCompilerPlugin(
  "org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full
)
