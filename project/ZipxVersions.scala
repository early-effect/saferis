import zipx.*

/** Typed catalog: every library and plugin this build may use. `zipxDepUpdate` rewrites constructors here. sbt-zipx is
  * not a row: generate emits it from the loaded plugin (`zipxSelfPlugins`). sbt-pgp is not a row: zipx already brings it
  * in. Action pins stay on jar defaults.
  *
  * Parent `Lib` vals used only for `.mod` are catalog rows; they are not `library()`-selected when another selected
  * module already pulls them (specular-core / specular-site via the docs theme). Core selects zio directly, so zio stays
  * a selected row. Docs share the zio-json row; the docs theme pulls it transitively at that version.
  */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.0.7")
  val scala: ScalaVersion = ScalaVersion("3.3.8")

  val zio             = Lib("dev.zio", "zio", "2.1.26")
  val zioStreams      = zio.mod("zio-streams")
  val zioJson         = Lib("dev.zio", "zio-json", "1.0.0")
  val zioTest         = zio.mod("zio-test")
  val zioTestSbt      = zio.mod("zio-test-sbt")
  val zioTestMagnolia = zio.mod("zio-test-magnolia")
  val zioLoggingSlf4j = Lib("dev.zio", "zio-logging-slf4j2-bridge", "2.5.3")

  val postgresqlTc = Lib("org.testcontainers", "postgresql", "1.21.4").java
  val postgresql   = Lib("org.postgresql", "postgresql", "42.7.13").java
  val slf4jNop     = Lib("org.slf4j", "slf4j-nop", "2.0.18").java
  val scaluzzi     = Lib("com.github.vovapolu", "scaluzzi", "0.1.23")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.14.1")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test

  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.14.1")
  val scalafix       = Plugin("ch.epfl.scala", "sbt-scalafix", "0.14.7")
  val dynverCi       = Plugin("rocks.earlyeffect", "sbt-dynver-ci", "0.2.3")
  val scoverage      = Plugin("org.scoverage", "sbt-scoverage", "2.4.4")

  private def provided(lib: Lib): Lib = lib.copy(config = Some("provided"))

  def coreLib = library(provided(zio), provided(zioStreams), provided(zioJson))
  def coreTest = library(
    zioLoggingSlf4j.test,
    zioTest.test,
    zioTestSbt.test,
    zioTestMagnolia.test,
    postgresqlTc.test,
    postgresql.test,
  )
  def docsLib  = library(zio, zioStreams, zioJson)
  def docsTest = library(specularZioTest, specularTheme, postgresqlTc.test, postgresql.test, slf4jNop.test)
end MyVersions
