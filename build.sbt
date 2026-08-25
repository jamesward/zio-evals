organization := "com.jamesward"

name := "zio-evals"

scalaVersion := "3.8.4"

scalacOptions ++= Seq(
  "-language:strictEquality",
  "-deprecation",
)

val zioVersion       = "2.1.26"
val zioSchemaVersion = "1.8.6"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"                   % zioVersion,
  "dev.zio" %% "zio-concurrent"        % zioVersion,
  // The eval domain types derive `Schema`; the transcript is persisted as JSON
  // by hosts via the schema-derived codec (EvalCodecs). zio-json (the AST +
  // decoders the CLI backends parse with) comes in transitively via
  // zio-schema-json.
  "dev.zio" %% "zio-schema"            % zioSchemaVersion,
  "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion,
  "dev.zio" %% "zio-schema-json"       % zioSchemaVersion,
  // The bundled CLI agent backends (claude / kiro-cli) shell out via zio-process.
  "dev.zio" %% "zio-process"           % "0.8.0",

  "dev.zio" %% "zio-test"          % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"      % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
)

fork := true

javaOptions ++= Seq(
  "-Djava.net.preferIPv4Stack=true",
  // JDK 25: suppress sun.misc.Unsafe / restricted-method warnings emitted by
  // upstream libs (scala-library, zio internals).
  "--enable-native-access=ALL-UNNAMED",
  "--sun-misc-unsafe-memory-access=allow",
)

licenses := Seq("MIT License" -> uri("https://opensource.org/licenses/MIT"))

homepage := Some(uri("https://github.com/jamesward/zio-evals"))

developers := List(
  Developer(
    "jamesward",
    "James Ward",
    "james@jamesward.com",
    uri("https://jamesward.com"),
  )
)

ThisBuild / versionScheme := Some("semver-spec")
