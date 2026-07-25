addSbtPlugin("org.scalameta"     % "sbt-scalafmt"  % "2.6.2")
addSbtPlugin("rocks.earlyeffect" % "sbt-marklit"   % "0.1.0")
addSbtPlugin("ch.epfl.scala"     % "sbt-scalafix"  % "0.14.7")
addSbtPlugin("com.github.sbt"    % "sbt-dynver"    % "5.1.1")
addSbtPlugin("com.github.sbt"    % "sbt-pgp"       % "2.3.1")
addSbtPlugin("org.scoverage"     % "sbt-scoverage" % "2.4.4")
addSbtPlugin("rocks.earlyeffect" % "sbt-zipx"      % "0.0.10")

// zipx bundles sbt-remote-cache; compiler-interface is on both sbt-2.x and zinc-1.x schemes.
libraryDependencySchemes += "org.scala-sbt" % "compiler-interface" % "always"
