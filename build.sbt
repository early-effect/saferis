val scala3Version = "3.3.8"
val zioVersion    = "2.1.26"

// Global settings. Iterable/saferis overrides group via PUBLISH_ORG from ZipxGitHubPackages.
ThisBuild / scalaVersion         := scala3Version
ThisBuild / organization         := sys.env.getOrElse("PUBLISH_ORG", "rocks.earlyeffect")
ThisBuild / organizationName     := sys.env.getOrElse("PUBLISH_ORG_NAME", "Early Effect")
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

// Dual publish: Central by default; GitHub Packages when CI sets PUBLISH_PACKAGES_REPO.
val githubPackagesRepo: Option[MavenRepository] =
  sys.env.get("PUBLISH_PACKAGES_REPO").map("GitHub Package Registry" at _)

ThisBuild / credentials ++= sys.env
  .get("GITHUB_TOKEN")
  .map { token =>
    Credentials("GitHub Package Registry", "maven.pkg.github.com", "_", token)
  }
  .toSeq

ThisBuild / resolvers ++= githubPackagesRepo.toSeq

ThisBuild / publishTo := githubPackagesRepo.orElse {
  val centralSnapshots =
    "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

// CI-only Central signing. Fork Packages publishes are unsigned (token auth).
githubPackagesRepo match {
  case None    => usePgpKeyHex(sys.env.getOrElse("PGP_KEY_HEX", "MISSING_KEY_HEX"))
  case Some(_) => Seq.empty
}

// zipx: Aggregate verify (tests + marklit on all repos) + dual publish by repo + Steward.
zipxJavaVersion  := "25"
zipxScalaSteward := true
zipxCapabilities ++= {
  val upstream = JobCondition.repositoryIs("early-effect/saferis")
  Seq(
    Capability.once("fmt", "scalafmtCheckAll"),
    Capability.once(
      name = "test",
      command = "test; docs/marklitCompile",
      needsCapabilities = List("fmt"),
    ),
    ZipxCentral.release
      .copy(command = _ => "core/publishSigned; sonaRelease")
      .withCondition(upstream),
    ZipxGitHubPackages.sharedRegistry(
      repository = Some("Iterable/saferis"),
      packagesRepo = Some("https://maven.pkg.github.com/iterable/maven-packages"),
      publishOrg = Some("com.iterable"),
      publishOrgName = Some("Iterable"),
    ).copy(command = _ => "core/publish"),
  )
}
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
      "org.postgresql"     % "postgresql"                % "42.7.11"  % Test,
    ),
  )

lazy val docs = project
  .in(file("saferis-docs"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name            := "saferis-docs",
    publish / skip  := true,
    publishArtifact := false,
    zipxPublish     := Some(false), // never join Central / Packages publish jobs
    libraryDependencies ++= Seq(
      "dev.zio"           %% "zio"        % zioVersion,
      "dev.zio"           %% "zio-json"   % "0.9.0",
      "org.testcontainers" % "postgresql" % "1.21.4",
      "org.postgresql"     % "postgresql" % "42.7.11",
      "org.slf4j"          % "slf4j-nop"  % "2.0.18",
    ),
    marklitTargetDirectory  := (ThisBuild / baseDirectory).value / "docs",
    marklitRunResourceClass := Some("saferis.docs.DocsTransactor"),
    marklitShowWarnings     := false,
    scalacOptions ~= (_.filterNot(_ == "-Wunused:all")),
  )
