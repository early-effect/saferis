package saferis.docs

import specular.ExampleRunner
import specular.ziotest.DocSpecSuite
import specular.ziotest.DocTestInterpreter
import zio.test.*

/** DocSpecs share one Postgres container; run examples sequentially to avoid concurrent `CREATE TABLE IF NOT EXISTS`
  * races.
  */
trait SaferisDocSpecSuite extends DocSpecSuite:
  override def spec: Spec[TestEnvironment, Any] =
    DocTestInterpreter.specOf(this).provideLayer(ExampleRunner.live) @@ TestAspect.sequential
