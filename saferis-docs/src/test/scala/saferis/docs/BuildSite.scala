package saferis.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.*
import specular.site.*
import zio.*

import java.nio.file.{Files, Path, StandardCopyOption}

/** Specular DocsSite: Test classpath main invoked by `docs/specularSite`. */
object BuildSite extends DocsSite:

  @navLabel("Getting Started")
  final case class GettingStartedNav(
      started: GettingStarted.type,
      concepts: CoreConcepts.type,
      databases: Databases.type,
  )

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

  /** Not a nav item. The sidebar is the table of contents; this page is the front. */
  private val frontPage: DocPage = Front.doc

  def pages: Vector[DocPage] = frontPage +: siteNav.pages

  override def site(settings: DocsSettings): SiteModel =
    EarlyEffectTheme
      .brand(super.site(settings))
      .copy(
        nav = Some(siteNav),
        pages = pages,
        summaryMarkdown = None,
      )

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _ = result
    // Specular always writes index.html as a summary plus a second copy of the nav.
    // The front DocPage is the index. Copy it over that file. Asset paths stay site-relative.
    val front   = out.resolve(s"${frontPage.slug}.html")
    val index   = out.resolve("index.html")
    val promote =
      ZIO.attemptBlockingIO(Files.copy(front, index, StandardCopyOption.REPLACE_EXISTING)).mapError { err =>
        SiteError.WriteFailed(index, err)
      }
    promote *> EarlyEffectTheme.writeLogo(out)
  end afterBuild
end BuildSite
