val projectBaseName = "bateman"

val Versions = new Object {
  val cats = "2.2.0"
  val fastparse = "2.3.1"
  val scalatest = "3.2.3"
  val shapeless = "2.3.3"
}

val commonSettings = Seq(
  scalaVersion := "2.12.13",
  scalacOptions ++= Seq(
    "-Ypartial-unification",
    "-language:implicitConversions",
//    "-Xlog-implicits",
    "-deprecation",
    "-feature"
  ),
  testOptions += Tests.Argument("-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % Versions.scalatest
  ).map(_ % Test)
)

val json = project
  .settings(
    name := s"$projectBaseName-json",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Versions.cats
    )
  )
  .settings(commonSettings)

val parser = project
  .dependsOn(json)
  .settings(
    name := s"$projectBaseName-json-parser",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % Versions.fastparse
    )
  )
  .settings(commonSettings)

val jsonGeneric = project
  .dependsOn(json)
  .dependsOn(parser % Test)
  .settings(
    name := s"$projectBaseName-json-generic",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % Versions.shapeless
    )
  )
  .settings(commonSettings)

val jsonapi = project
  .dependsOn(json)
  .dependsOn(parser % Test)
  .settings(
    name := s"$projectBaseName-jsonapi",
    libraryDependencies ++= Seq(
    )
  )
  .settings(commonSettings)

val jsonapiGeneric = project
  .dependsOn(jsonapi % "compile->compile;test->test")
  .dependsOn(jsonGeneric)
  .dependsOn(parser % Test)
  .settings(
    name := s"$projectBaseName-jsonapi-generic",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % Versions.shapeless
    )
  )
  .settings(commonSettings)

val root = (project in file("."))
  .aggregate(json, parser, jsonGeneric, jsonapi, jsonapiGeneric)
  .settings(
    name := s"$projectBaseName-build",
    skip in publish := true
  )
