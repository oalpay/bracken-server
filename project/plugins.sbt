resolvers += Resolver.url("untyped", url("http://ivy.untyped.com"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.untyped" % "sbt-js"       % "0.7" )

addSbtPlugin("com.untyped" % "sbt-less"     % "0.7" )

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.2.5")

addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "0.4.2")
