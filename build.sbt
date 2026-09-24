import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
// sbt has its own `Exec` (a queued command). This is the shell AST's simple command.
import zipx.shell.Exec

MyVersions.settings

// Global settings. Iterable/saferis overrides group via PUBLISH_ORG from ZipxGitHubPackages.
// Explicit ThisBuild pin: sbt's default ThisBuild scalaVersion is the metabuild Scala, and
// zipxCheckDeps compares that to zipxScala. The catalog scala is the project scala.
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
val pgJsCiSetup = Steps.buildingWith("pg-js-ci") { ctx =>
  List(
    Step
      .usesRef(ctx.actions.setupNode)
      .named("Set up Node")
      .withInputs(scala.collection.immutable.ListMap("node-version" -> "24", "cache" -> "npm")),
    Step.run(Script(Exec("npm", Word.lit("ci")))).named("Install Node dependencies"),
  )
}

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
      command = zipxTasks.session(
        LocalProject("core") / testFull,
        LocalProject("coreJS") / testFull,
        LocalProject("jdbc") / testFull,
        LocalProject("docs") / specularSite,
      ),
      // GHA VMs are disposable; skip Ryuk so Hub flakes on testcontainers/ryuk cannot fail CI.
      env = Map("TESTCONTAINERS_RYUK_DISABLED" -> EnvValue.plain("true")),
      extraSteps = prePullPostgres,
    ),
    // Node pg suite. Not part of verify: it needs npm, and it starts Postgres itself.
    // The `test` job must not wait on this one.
    Capability.once(
      name = CapabilityName("test-pg"),
      command = zipxTasks.session(LocalProject("pgJS") / testFull),
      extraSteps = pgJsCiSetup ++ prePullPostgres,
      // GHA VMs are disposable; skip Ryuk so Hub flakes on testcontainers/ryuk cannot fail CI.
      env = Map("TESTCONTAINERS_RYUK_DISABLED" -> EnvValue.plain("true")),
    ),
    // Every publishing row, then sonaRelease once. docs and root do not publish.
    ZipxCentral.release
      .withCondition(upstream),
    ZipxGitHubPackages
      .sharedRegistry(
        // 0.1.6 dropped the `repository` param, which used to become this fork gate implicitly.
        // Stated explicitly so the Packages publish still cannot run outside Iterable/saferis.
        condition = Some(JobCondition.repositoryIs("Iterable/saferis")),
        packagesRepo = Some("https://maven.pkg.github.com/iterable/maven-packages"),
        publishOrg = Some("com.iterable"),
        publishOrgName = Some("Iterable"),
      ),
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

lazy val scala3Version: String = MyVersions.scala
lazy val scalaVersions         = Seq(scala3Version)

// Root project aggregates all modules but is not published
lazy val root = project
  .in(file("."))
  .aggregate((core.projectRefs ++ jdbc.projectRefs ++ pg.projectRefs ++ Seq[sbt.ProjectReference](docs))*)
  .settings(
    name           := "saferis-root",
    publish / skip := true,
  )

// Core library. JVM and Scala.js. No JDBC.
lazy val core = (projectMatrix in file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name        := "saferis",
    description := "Saferis mitigates the discord of unsafe SQL. It is a resource safe SQL client library.",
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions)

// JDBC driver. JVM only. Depends on the core JVM row.
lazy val jdbc = (projectMatrix in file("jdbc"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.jdbcLib)
  .settings(MyVersions.coreTest)
  .settings(MyVersions.jdbcTest)
  .settings(
    name        := "saferis-jdbc",
    description := "JDBC SqlSession for Postgres.",
    Test / unmanagedSourceDirectories ++= Def.uncached(
      Seq(
        (ThisBuild / baseDirectory).value / "testkit" / "src" / "scala",
        (ThisBuild / baseDirectory).value / "testkit" / "src" / "jvm" / "scala",
      )
    ),
  )
  .jvmPlatform(scalaVersions = scalaVersions)

// Node pg driver. Scala.js only. Depends on the core JS row.
lazy val pg = (projectMatrix in file("pg"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.pgLib)
  .settings(MyVersions.coreTest)
  .settings(
    name        := "saferis-pg",
    description := "Node pg SqlSession for Postgres.",
    Test / unmanagedSourceDirectories ++= Def.uncached(
      Seq(
        (ThisBuild / baseDirectory).value / "testkit" / "src" / "scala",
        (ThisBuild / baseDirectory).value / "testkit" / "src" / "js" / "scala",
      )
    ),
  )
  .jsPlatform(
    scalaVersions = scalaVersions,
    Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    ),
  )

lazy val docs = project
  .in(file("saferis-docs"))
  .dependsOn(jdbc.jvm(scala3Version))
  .enablePlugins(SpecularPlugin)
  .settings(commonSettings)
  .settings(
    name            := "saferis-docs",
    publish / skip  := true,
    publishArtifact := false,
    zipxPublish     := Some(false), // never join Central / Packages publish jobs
    MyVersions.docsLib,
    MyVersions.docsTest,
    specularBuildMain     := "saferis.docs.BuildSite",
    specularMetaProject   := Some(LocalProject("core")),
    specularArtifactKind  := "library",
    specularSiteDirectory := (ThisBuild / baseDirectory).value / "target" / "site",
    // CI docs builds are dynver `-ci`; stripCi drops the suffix so install snippets show the last published tag.
    specularDisplayVersion := stripCi,
    scalacOptions ~= (_.filterNot(_ == "-Wunused:all")),
  )
