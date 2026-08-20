MyVersions.settings

// Global settings. Iterable/saferis overrides group via PUBLISH_ORG from ZipxGitHubPackages.
// Explicit ThisBuild pin: sbt 2's default ThisBuild scalaVersion is the meta Scala (3.8.4), and
// zipxCheckDeps compares that to zipxScala. Core stays on catalog LTS; docs overrides below.
ThisBuild / scalaVersion         := (MyVersions.scala: String)
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

// zipx: Aggregate verify (tests + Specular docs site) + dual publish by repo + Pages + catalog PRs.

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
zipxWorkflowDispatch := true
zipxCapabilities ++= {
  val upstream = JobCondition.repositoryIs("early-effect/saferis")
  Seq(
    Capability.once(
      name = Capability.TestName,
      command = zipxTasks.session(testFull, LocalProject("docs") / specularSite),
      // GHA VMs are disposable; skip Ryuk so Hub flakes on testcontainers/ryuk cannot fail CI.
      env = Map("TESTCONTAINERS_RYUK_DISABLED" -> EnvValue.plain("true")),
      extraSteps = prePullPostgres,
    ),
    // Same core-only command as before; sessionTail cleared so sonaRelease is not appended twice.
    ZipxCentral.release
      .runningPerModule(cmd"core/publishSigned; sonaRelease")
      .copy(sessionTail = None)
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
      .runningPerModule(cmd"core/publish"),
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
  scalafixDependencies += MyVersions.moduleID(MyVersions.scaluzzi),
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
    MyVersions.coreLib,
    MyVersions.coreTest,
  )

lazy val docs = project
  .in(file("saferis-docs"))
  .dependsOn(core)
  .enablePlugins(SpecularPlugin)
  .settings(commonSettings)
  .settings(
    name := "saferis-docs",
    // Specular 0.12.0 is built on 3.8.x; keep published core on catalog LTS (3.3.8).
    scalaVersion    := "3.8.4",
    publish / skip  := true,
    publishArtifact := false,
    zipxPublish     := Some(false), // never join Central / Packages publish jobs
    MyVersions.docsLib,
    MyVersions.docsTest,
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
