
val catsVersion = "2.9.0"
val catsParseVersion = "0.3.7"
val scalaUriVersion = "4.0.3"
val scalaTestVersion = "3.2.15"

lazy val root = project
  .in(file("."))
  .settings(
    name := "did-core",
    version := "0.2.0",
    scalaVersion := "3.2.2",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-parse" % catsParseVersion,
      "io.lemonlabs" %% "scala-uri" % scalaUriVersion,
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    ),
    scalacOptions ++= Seq(
      "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
      "-encoding", "utf-8",                // Specify character encoding used by source files.
      "-explaintypes",                     // Explain type errors in more detail.
      "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
      "-language:higherKinds",             // Allow higher-kinded types
      "-language:implicitConversions",     // Allow definition of implicit functions called views
      "-language:fewerBraces",             // Even fewer braces
      "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
      "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-Xmacro-settings:materialize-derivations", //Explain internal macro derivations
    )
  )
