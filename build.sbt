val scala3Version = "3.3.8"
val zioVersion    = "2.1.26"

// Global settings using ThisBuild scope
ThisBuild / scalaVersion         := scala3Version
ThisBuild / organization         := "rocks.earlyeffect"
ThisBuild / organizationName     := "Early Effect"
ThisBuild / organizationHomepage := Some(url("https://www.earlyeffect.rocks"))
ThisBuild / licenses             := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage             := Some(url("https://github.com/early-effect/saferis"))
ThisBuild / scmInfo              := Some(
  ScmInfo(
    url("https://github.com/early-effect/saferis"),
    "scm:git@github.com:early-effect/saferis.git",
  )
)
ThisBuild / developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = url("https://github.com/russwyte"),
  )
)
ThisBuild / versionScheme := Some("early-semver")

// Publishing to Sonatype's Central Portal. sbt 1.11+ has built-in support via
// `localStaging` / `publishSigned` / `sonaRelease` — no sbt-sonatype plugin needed.
ThisBuild / publishTo := {
  val centralSnapshots =
    "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

// CI-only publishing: the signing key hex comes from the PGP_KEY_HEX env var, set by
// the shared early-effect org secret in the release workflow. There is no real key in
// this file — the "MISSING_KEY_HEX" sentinel keeps the build loadable for local
// compile/test but makes signing fail loudly if anyone tries to publish off-CI.
usePgpKeyHex(sys.env.getOrElse("PGP_KEY_HEX", "MISSING_KEY_HEX"))

// zipx: Aggregate verify (tests + marklit docs) + Central publish + Scala Steward.
zipxJavaVersion  := "25"
zipxScalaSteward := true
zipxCapabilities += Capability.once("fmt", "scalafmtCheckAll")
zipxCapabilities += Capability.once(
  name = "test",
  command = "test; docs/marklitCompile",
  needsCapabilities = List("fmt"),
)
zipxCapabilities += ZipxCentral.release.copy(command = _ => "+publishSigned; sonaRelease")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-Wunused:all",
    "-feature",
  ),
  scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.23",
)

lazy val publishSettings = Seq(
  publishMavenStyle    := true,
  pomIncludeRepository := { _ => false },
)

// Root project aggregates all modules but is not published
lazy val root = project
  .in(file("."))
  .aggregate(core, docs)
  .settings(
    name           := "saferis-root",
    publish / skip := true,
  )

// Core library - the main publishable artifact
lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name        := "saferis",
    description := "Saferis mitigates the discord of unsafe SQL. It is a resource safe SQL client library.",
    libraryDependencies ++= Seq(
      "dev.zio"           %% "zio"                       % zioVersion % "provided",
      "dev.zio"           %% "zio-streams"               % zioVersion % "provided",
      "dev.zio"           %% "zio-json"                  % "0.9.2"    % "provided",
      "dev.zio"           %% "zio-logging-slf4j2-bridge" % "2.5.3"    % Test,
      "dev.zio"           %% "zio-test"                  % zioVersion % Test,
      "dev.zio"           %% "zio-test-sbt"              % zioVersion % Test,
      "dev.zio"           %% "zio-test-magnolia"         % zioVersion % Test,
      "org.testcontainers" % "postgresql"                % "1.21.4"   % Test,
      "org.postgresql"     % "postgresql"                % "42.7.13"  % Test,
    ),
  )

lazy val docs = project
  .in(file("saferis-docs"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name           := "saferis-docs",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "dev.zio"           %% "zio"        % zioVersion,
      "dev.zio"           %% "zio-json"   % "0.9.0",
      "org.testcontainers" % "postgresql" % "1.21.4",
      "org.postgresql"     % "postgresql" % "42.7.13",
      "org.slf4j"          % "slf4j-nop"  % "2.0.18",
    ),
    marklitTargetDirectory  := (ThisBuild / baseDirectory).value / "docs",
    marklitRunResourceClass := Some("saferis.docs.DocsTransactor"),
    marklitShowWarnings     := false,
    scalacOptions ~= (_.filterNot(_ == "-Wunused:all")),
  )
