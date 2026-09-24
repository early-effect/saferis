import zipx.*

/** Typed catalog: every library and plugin this build may use. `zipxDepUpdate` rewrites constructors here. sbt-zipx is
  * not a row: generate emits it from the loaded plugin (`zipxSelfPlugins`). sbt-pgp is not a row: zipx already brings it
  * in. Action pins stay on jar defaults.
  *
  * Parent `Lib` vals used only for `.mod` are catalog rows; they are not `library()`-selected when another selected
  * module already pulls them (specular-core / specular-site via the docs theme). Core selects zio directly, so zio stays
  * a selected row. Core selects zio-json as provided (JSONB helpers). The docs theme is the same early-effect
  * release, so it does not pin an older zio-json.
  */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.1.0-M2")
  val scala: ScalaVersion = ScalaVersion("3.9.0")

  val zio             = Lib("dev.zio", "zio", "2.1.26")
  val zioStreams      = zio.mod("zio-streams")
  val zioJson         = Lib("dev.zio", "zio-json", "1.1.0")
  val zioTest    = zio.mod("zio-test")
  val zioTestSbt = zio.mod("zio-test-sbt")
  val zioLoggingSlf4j = Lib("dev.zio", "zio-logging-slf4j2-bridge", "2.5.3")

  val postgresqlTc = Lib("org.testcontainers", "postgresql", "1.21.4").java
  val postgresql   = Lib("org.postgresql", "postgresql", "42.7.13").java
  val slf4jNop     = Lib("org.slf4j", "slf4j-nop", "2.0.18").java
  val scaluzzi     = Lib("com.github.vovapolu", "scaluzzi", "0.1.23")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.17.0")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test

  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.17.0")
  val scalafix       = Plugin("ch.epfl.scala", "sbt-scalafix", "0.14.7")
  val dynverCi       = Plugin("rocks.earlyeffect", "sbt-dynver-ci", "0.2.3")
  val scoverage      = Plugin("org.scoverage", "sbt-scoverage", "2.4.4")
  val scalajs        = Plugin("org.scala-js", "sbt-scalajs", "1.22.0")
  val scalaNative    = Plugin("org.scala-native", "sbt-scala-native", "0.5.12")

  val scalaJavaTime     = Lib("io.github.cquiroz", "scala-java-time", "2.7.0")
  val scalaJavaTimeTzdb = scalaJavaTime.mod("scala-java-time-tzdb")

  private def provided(lib: Lib): Lib = lib.copy(config = Some("provided"))

  def coreLib  = library(provided(zio), provided(zioStreams), provided(zioJson))
  def coreTest = library(zioTest.test, zioTestSbt.test)
  def jdbcLib  = library(provided(zio), provided(zioStreams), postgresql)
  def jdbcTest = library(zioLoggingSlf4j.test, postgresqlTc.test, zioJson.test)
  def pgLib    = library(provided(zio), provided(zioStreams))
  def docsLib  = library(zio, zioStreams)
  // postgresql comes from saferis-jdbc's compile dependency. A second test-scoped copy is redundant.
  def docsTest   = library(specularZioTest, specularTheme, postgresqlTc.test, slf4jNop.test)
  def testkitJvm     = library(postgresqlTc)
  def testkitTest    = library(zioJson.test)
  def nativeJavaTime = library(scalaJavaTime, scalaJavaTimeTzdb)
end MyVersions
