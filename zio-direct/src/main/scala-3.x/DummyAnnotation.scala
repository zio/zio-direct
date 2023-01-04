package scala.reflect.macros.internal

// A dummy annotation for IntelliJ to recognize a Scala 3 inline macro... as a macro
final class macroImpl(referenceToMacroImpl: Any) extends scala.annotation.StaticAnnotation
