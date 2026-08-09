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

// zipx: Aggregate verify (tests + Specular docs site) + dual publish by repo + Steward + Pages.
val Fmt = CapabilityName("fmt")

/** Pre-pull with retries. Verbatim shell, so runRaw declares the escape hatch and earns a
  * generate-time warning naming the step, rather than hiding it in a bare `run =`.
  */
val prePullPostgres = Steps.built("pre-pull-postgres")(
  Step
    .runRaw(
      """|set -euo pipefail
         |image=postgres:latest
         |max=5
         |for attempt in $(seq 1 "$max"); do
         |  if docker pull "$image"; then
         |    exit 0
         |  fi
         |  if [ "$attempt" -eq "$max" ]; then
         |    echo "Failed to pull $image after $max attempts" >&2
         |    exit 1
         |  fi
         |  sleep $((attempt * 10))
         |done
         |""".stripMargin
    )
    .named("Pre-pull Postgres image")
)

zipxJavaVersion      := JdkVersion("25")
zipxScalaSteward     := true
zipxWorkflowDispatch := true
zipxCapabilities ++= {
  val upstream = JobCondition.repositoryIs("early-effect/saferis")
  Seq(
    zipxTasks.once(Fmt, scalafmtCheckAll),
    Capability.once(
      name = Capability.TestName,
      // Compound, so a literal SbtCommand rather than a spliced task key.
      command = SbtCommand("test; docs/specularSite"),
      needsCapabilities = List(Fmt),
      // GHA VMs are disposable; skip Ryuk so Hub flakes on testcontainers/ryuk cannot fail CI.
      env = Map("TESTCONTAINERS_RYUK_DISABLED" -> EnvValue.plain("true")),
      extraSteps = prePullPostgres,
    ),
    ZipxCentral.release
      .copy(command = _ => SbtCommand("core/publishSigned; sonaRelease"))
      .withCondition(upstream),
    ZipxGitHubPackages
      .sharedRegistry(
        // 0.1.6 dropped the `repository` param, which used to become this fork gate implicitly.
        // Stated explicitly so the Packages publish still cannot run outside Iterable/saferis.
        condition = Some(JobCondition.repositoryIs("Iterable/saferis")),
        packagesRepo = Some("https://maven.pkg.github.com/iterable/maven-packages"),
        publishOrg = Some("com.iterable"),
        publishOrgName = Some("Iterable"),
      )
      .copy(command = _ => SbtCommand("core/publish")),
    // Same org reusable workflow as peers; generated into ci.yml (no hand-rolled docs.yml).
    ZipxDocs.pages().andCondition(upstream),
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
      "dev.zio"           %% "zio-json"                  % "0.10.0"    % "provided",
      "dev.zio"           %% "zio-logging-slf4j2-bridge" % "2.5.3"    % Test,
      "dev.zio"           %% "zio-test"                  % zioVersion % Test,
      "dev.zio"           %% "zio-test-sbt"              % zioVersion % Test,
      "dev.zio"           %% "zio-test-magnolia"         % zioVersion % Test,
      "org.testcontainers" % "postgresql"                % "1.21.4"   % Test,
      "org.postgresql"     % "postgresql"                % "42.7.13"  % Test,
    ),
  )

val specularVersion = "0.12.0"

lazy val docs = project
  .in(file("saferis-docs"))
  .dependsOn(core)
  .enablePlugins(SpecularPlugin)
  .settings(commonSettings)
  .settings(
    name := "saferis-docs",
    // Specular 0.11.0 is built on 3.8.x; keep published core on ThisBuild LTS (3.3.8).
    scalaVersion    := "3.8.4",
    publish / skip  := true,
    publishArtifact := false,
    zipxPublish     := Some(false), // never join Central / Packages publish jobs
    libraryDependencies ++= Seq(
      "dev.zio"           %% "zio"                     % zioVersion,
      "dev.zio"           %% "zio-streams"             % zioVersion,
      "dev.zio"           %% "zio-json"                % "0.9.0",
      "dev.zio"           %% "zio-test"                % zioVersion      % Test,
      "dev.zio"           %% "zio-test-sbt"            % zioVersion      % Test,
      "rocks.earlyeffect" %% "specular-core"           % specularVersion % Test,
      "rocks.earlyeffect" %% "specular-zio-test"       % specularVersion % Test,
      "rocks.earlyeffect" %% "specular-site"           % specularVersion % Test,
      "rocks.earlyeffect" %% "early-effect-docs-theme" % specularVersion % Test,
      "org.testcontainers" % "postgresql"              % "1.21.4"        % Test,
      "org.postgresql"     % "postgresql"              % "42.7.13"       % Test,
      "org.slf4j"          % "slf4j-nop"               % "2.0.18"        % Test,
    ),
    specularBuildMain     := "saferis.docs.BuildSite",
    specularMetaProject   := Some(LocalProject("core")),
    specularArtifactKind  := "library",
    specularSiteDirectory := (ThisBuild / baseDirectory).value / "target" / "site",
    // Docs-only (workflow_dispatch) builds are dynver `-ci`; don't advertise that as a Central coord.
    // Empty string → Specular uses build version (clean v* tags).
    specularDisplayVersion := {
      val v = (ThisBuild / version).value
      if v.endsWith("-ci") || v.endsWith("-SNAPSHOT") then
        previousStableVersion.value.getOrElse("<version>")
      else ""
    },
    scalacOptions ~= (_.filterNot(_ == "-Wunused:all")),
  )
