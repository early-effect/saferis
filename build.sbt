import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.nativeConfig
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
         |image=postgres:17
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
        LocalProject("coreNative") / testFull,
        LocalProject("postgres") / testFull,
        LocalProject("postgresJS") / testFull,
        LocalProject("postgresNative") / testFull,
        LocalProject("jdbc") / testFull,
        LocalProject("postgresJdbc") / testFull,
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
      command = zipxTasks.session(LocalProject("pgJS") / testFull, LocalProject("pgEsm") / pgEsmProbe),
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
  .aggregate(
    (core.projectRefs ++ postgres.projectRefs ++ jdbc.projectRefs ++ postgresJdbc.projectRefs ++ pg.projectRefs ++
      testkit.projectRefs ++ Seq[sbt.ProjectReference](docs, pgEsm))*
  )
  .settings(
    name           := "saferis-root",
    publish / skip := true,
  )

// Core library. JVM, Scala.js, and Scala Native. No JDBC.
lazy val nativeCore =
  MyVersions.nativeJavaTime ++ Seq(
    nativeConfig ~= (_.withMultithreading(true)),
    libraryDependencySchemes += "org.scala-native" % "test-interface_native0.5_3" % "early-semver",
    dependencyOverrides += Def.uncached(
      "org.scala-native" % "test-interface_native0.5_3" % (MyVersions.scalaNative.version: String)
    ),
  )

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
  .nativePlatform(scalaVersions = scalaVersions, nativeCore)

// Postgres text codec and connection settings. JVM, Scala.js, and Scala Native. No driver.
lazy val postgres = (projectMatrix in file("postgres"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.coreLib)
  .settings(MyVersions.coreTest)
  .settings(
    name        := "saferis-postgres",
    description := "Postgres text codec and connection settings shared by drivers.",
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions)
  .nativePlatform(scalaVersions = scalaVersions, nativeCore)

// Test container and the conformance suite. Not published: it starts Docker.
lazy val testkit = (projectMatrix in file("testkit"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name           := "saferis-testkit",
    description    := "Postgres test container and the driver conformance suite.",
    publish / skip := true,
    MyVersions.coreLib,
    MyVersions.coreTest,
    MyVersions.testkitTest,

  )
  .jvmPlatform(scalaVersions = scalaVersions, MyVersions.testkitJvm)
  .jsPlatform(
    scalaVersions = scalaVersions,
    settings = Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
      Compile / sourceGenerators += Def.task {
        val in  = (ThisBuild / baseDirectory).value / "testkit" / "src" / "main" / "resources" / "init.sql"
        val out = (Compile / sourceManaged).value / "saferis" / "tests" / "InitScript.scala"
        val body = sbt.io.IO.read(in).replace("\"\"\"", "\\\"\\\"\\\"")
        sbt.io.IO.write(
          out,
          s"""|package saferis.tests
              |
              |private[tests] object InitScript:
              |  val sql: String =
              |    \"\"\"$body\"\"\"
              |""".stripMargin,
        )
        Seq(out)
      }.taskValue,
    ),
  )

// java.sql session. No database driver. A JdbcAdapter supplies bind, read, and errors.
lazy val jdbc = (projectMatrix in file("jdbc"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.jdbcLib)
  .settings(MyVersions.coreTest)
  .settings(
    name        := "saferis-jdbc",
    description := "JDBC SqlSession. Database behavior is a JdbcAdapter.",
  )
  .jvmPlatform(scalaVersions = scalaVersions)

// Postgres JdbcAdapter. The only JVM module that depends on pgjdbc and saferis-postgres.
lazy val postgresJdbc = (projectMatrix in file("postgres-jdbc"))
  .dependsOn(jdbc, postgres)
  .dependsOn(testkit % "test->compile;test->test")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.postgresJdbcLib)
  .settings(MyVersions.coreTest)
  .settings(MyVersions.postgresJdbcTest)
  .settings(
    name        := "saferis-postgres-jdbc",
    description := "Postgres JdbcAdapter on pgjdbc.",
  )
  .jvmPlatform(scalaVersions = scalaVersions)

// Node pg driver. Scala.js only. Depends on the core JS row and the shared Postgres codec.
lazy val pg = (projectMatrix in file("pg"))
  .dependsOn(core, postgres)
  .dependsOn(testkit % "test->compile;test->test")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(MyVersions.pgLib)
  .settings(MyVersions.coreTest)
  .settings(
    name        := "saferis-postgres-node",
    description := "Node pg SqlSession for Postgres.",
  )
  .jsPlatform(
    scalaVersions = scalaVersions,
    Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    ),
  )

lazy val pgEsmProbe = taskKey[Unit]("Load the Node driver as an ES module under Node")

// Second link of the Node driver as an ES module. Sources are the pg module. Not published.
lazy val pgEsm = project
  .in(file("pg/esm"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js(scala3Version), postgres.js(scala3Version))
  .settings(commonSettings)
  .settings(MyVersions.pgLib)
  .settings(
    name           := "saferis-pg-esm",
    publish / skip := true,
    Compile / unmanagedSourceDirectories += Def.uncached(
      (ThisBuild / baseDirectory).value / "pg" / "src" / "main" / "scala"
    ),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true,
    // sbt's `run` executes the `.js` output as CommonJS. Node only treats this file as
    // an ES module when the name is `.mjs`, which is when a wrong `pg` import fails.
    pgEsmProbe := Def.uncached {
      val report = (Compile / fastLinkJS).value
      val output = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val js     = output / report.data.publicModules.head.jsFileName
      val mjs    = output / "main.mjs"
      sbt.io.IO.copyFile(js, mjs)
      val code = scala.sys.process
        .Process(List("node", mjs.getAbsolutePath), (ThisBuild / baseDirectory).value)
        .!
      if code != 0 then sys.error(s"ES module load exited $code")
    },
  )

lazy val docs = project
  .in(file("saferis-docs"))
  .dependsOn(postgresJdbc.jvm(scala3Version))
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
