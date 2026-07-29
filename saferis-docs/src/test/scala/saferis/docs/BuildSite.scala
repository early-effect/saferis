package saferis.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.*
import specular.site.*
import zio.*

import java.nio.file.Path

/** Specular DocsSite: Test classpath main invoked by `docs/specularSite`. */
object BuildSite extends DocsSite:

  @navLabel("Getting Started")
  final case class GettingStartedNav(started: GettingStarted.type, concepts: CoreConcepts.type)

  @navLabel("Safety")
  final case class Safety(
      injection: SqlInjectionPrevention.type,
      capabilities: Capabilities.type,
      errors: ErrorHandling.type,
      timeouts: StatementTimeouts.type,
      retry: RetryableErrors.type,
  )

  @navLabel("Schema")
  final case class SchemaNav(
      ddl: Ddl.type,
      foreignKeys: ForeignKeys.type,
      schemaValidation: SchemaValidation.type,
      dialect: DialectSystem.type,
  )

  @navLabel("Querying")
  final case class Querying(
      dml: Dml.type,
      queryBuilder: QueryBuilder.type,
      subqueries: Subqueries.type,
      aggregates: AggregateFunctions.type,
      upsert: UpsertDocs.type,
  )

  @navLabel("Streaming")
  final case class StreamingNav(streaming: Streaming.type, paged: PagedStreaming.type)

  @navLabel("Reference")
  final case class Reference(typeSupport: TypeSupport.type, queryExecution: QueryExecution.type)

  final case class SaferisNav(
      gettingStarted: GettingStartedNav,
      safety: Safety,
      schema: SchemaNav,
      querying: Querying,
      streaming: StreamingNav,
      reference: Reference,
  ) derives SiteNav

  private val siteNav: NavModel = SiteNav[SaferisNav].toNavModel

  def pages: Vector[DocPage] = siteNav.pages

  override def site: SiteModel =
    EarlyEffectTheme
      .brand(super.site)
      .copy(
        nav = Some(siteNav),
        pages = siteNav.pages,
        summaryMarkdown = Some(
          """**Saferis** is a type-safe, resource-safe SQL client for Scala 3 and ZIO.
Every example on this site is a Specular DocSpec: it asserts under zio-test and runs against
a live PostgreSQL database (Testcontainers) when the site is built.
"""
        ),
      )

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): Task[Unit] =
    val _ = result
    EarlyEffectTheme.writeLogo(out)
end BuildSite
