lazy val tapirInterpreter = project
  .in(file("tapir-interpreter"))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s"             %% "http4s-dsl"          % V.http4s,
      "org.http4s"             %% "http4s-blaze-server" % V.http4s,
      "org.http4s"             %% "http4s-blaze-client" % V.http4s,
      "com.softwaremill.tapir" %% "core"                % V.tapir,
      "com.softwaremill.tapir" %% "openapi-docs"        % V.tapir,
      "com.softwaremill.tapir" %% "openapi-circe-yaml"  % V.tapir,
      "com.softwaremill.tapir" %% "json-circe"          % V.tapir,
      "com.softwaremill.tapir" %% "sttp-client"         % V.tapir,
      "com.softwaremill.tapir" %% "akka-http-server"    % V.tapir,
      "ch.qos.logback"         % "logback-classic"      % "1.3.0-alpha4"
    ),
    scalaVersion := V.scala,
    scalacOptions ++= Seq("-Ypartial-unification")
  )

lazy val example = project
  .in(file("example"))
  .settings(
    scalaVersion := V.scala
  )
  .dependsOn(tapirInterpreter)
