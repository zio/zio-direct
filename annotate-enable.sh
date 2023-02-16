#!/bin/bash
sed -i 's/\/\/ @scala.reflect.macros.internal.macroImpl("nothing")/@scala.reflect.macros.internal.macroImpl("nothing")/' zio-direct/src/main/scala-3.x/zio/direct/Dsl.scala
