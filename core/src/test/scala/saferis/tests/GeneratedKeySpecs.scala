package saferis.tests

import saferis.*

import zio.test.*

object GeneratedKeySpecs extends ZIOSpecDefault:

  def spec = suite("generated key")(
    test("eight inputs, and a compound key is never IdentityPrimaryKey"):
      assertTrue(
        GeneratedKey.column(false, false, false) == GeneratedKey.Plain,
        GeneratedKey.column(false, false, true) == GeneratedKey.Plain,
        GeneratedKey.column(false, true, false) == GeneratedKey.PrimaryKey,
        GeneratedKey.column(false, true, true) == GeneratedKey.Plain,
        GeneratedKey.column(true, false, false) == GeneratedKey.Identity,
        GeneratedKey.column(true, false, true) == GeneratedKey.Identity,
        GeneratedKey.column(true, true, false) == GeneratedKey.IdentityPrimaryKey,
        GeneratedKey.column(true, true, true) == GeneratedKey.Identity,
      )
  )
end GeneratedKeySpecs
