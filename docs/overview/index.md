---
id: overview_index
title: "Summary"
---

## Getting started

Start by adding `zio-direct` as a dependency to your project:

```scala mdoc:passthrough
    println(s"""```scala""")
    if (zio.direct.BuildInfo.isSnapshot) {
        println(s"""resolvers += Resolver.sonatypeRepo("snapshots")""")
    }
    println(s"""libraryDependencies += "dev.zio" %% "zio-direct" % "${zio.direct.BuildInfo.version}"""")
    println(s"""```""")
```
