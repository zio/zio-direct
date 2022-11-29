---
id: index
title:  "Introduction to ZIO Direct Style"
sidebar_label: "ZIO Direct Style"
---

ZIO Direct style is a library that allows using directly-style i.e. imperative programming with ZIO effects.

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
