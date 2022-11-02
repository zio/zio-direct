package zio.asyncawait.core.metaprog

import scala.quoted._
import zio.asyncawait.core.metaprog.Extractors.Lambda1
import zio.asyncawait.core.util.Format
import scala.meta.Type.ByName.apply
import zio.ZIO

object DefDefCopy {

  private def methodTypeMaker(using Quotes)(clause: quotes.reflect.TermParamClause): quotes.reflect.TypeRepr => quotes.reflect.MethodType =
    import quotes.reflect._
    (outputType: TypeRepr) =>
      MethodType(clause.params.map(_.name + "New"))(_ => clause.params.map(_.tpt.tpe), _ => outputType)

  private def polyTypeMaker(using Quotes)(clause: quotes.reflect.TypeParamClause): quotes.reflect.TypeRepr => quotes.reflect.MethodType =
    import quotes.reflect._
    // TODO what about await clauses in lambdas? Should there be an explicit check to disable them?
    report.throwError("Poly types with await-clauses are not supported yet. Please move the await clause out of the function.")

  private def outputTypeMaker(using Quotes)(params: quotes.reflect.ParamClause): quotes.reflect.TypeRepr => quotes.reflect.MethodType | quotes.reflect.PolyType =
    import quotes.reflect._
    params match
      case clause: TermParamClause => methodTypeMaker(clause)
      case clause: TypeParamClause => polyTypeMaker(clause)



  def computeNewSymbol(using Quotes)(defdef: quotes.reflect.DefDef, functionOutputType: quotes.reflect.TypeRepr): quotes.reflect.Symbol = {
    import quotes.reflect._
    def handleParamClause(clause: ParamClause, rest: List[ParamClause]): TypeRepr =
      rest match
        case Nil => outputTypeMaker(clause)(functionOutputType)
        case head :: tail => outputTypeMaker(clause)(handleParamClause(head, tail))

    val methodType =
      defdef.paramss match
        case Nil => ByNameType(functionOutputType)
        case head :: tail => handleParamClause(head, tail)


    val allSymbols = defdef.paramss.flatMap(symbolsOfTerms(_))

    println(s"-------------- New Method Type: ${methodType.show}")

    // Note, for nested methods it would not be the Symbol.spliceOwner. Throw error if they are nested methods?
    //Symbol.newMethod(defdef.symbol.owner, defdef.name + "New", methodType)
    Symbol.newMethod(defdef.symbol.owner, defdef.name + "New", MethodType(List("bNew"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[ZIO[Any, Throwable, String]]))
  }


  // TODO check for a given-params clause and error that given-params clauses with awaits are not supported
  // TODO Also, how to do you create methods with parameters with default values?
  def of(using q: Quotes)(methodSymbol: quotes.reflect.Symbol, defdef: quotes.reflect.DefDef, functionOutputType: quotes.reflect.TypeRepr)(prepareBody: (List[List[quotes.reflect.Tree]], quotes.reflect.Term) => quotes.reflect.Term): quotes.reflect.DefDef = {
    import quotes.reflect._

    val allSymbols = defdef.paramss.flatMap(symbolsOfTerms(_))

    val newDefDef = DefDef.apply(
      methodSymbol,
      args => {
        val argsAsTerms =
          args.flatMap(t => t).map {
            case term: Term => term
            //case other => report.errorAndAbort(s"The input-argument: `$other` is not a Term.")
          }
        if (argsAsTerms.length != allSymbols.length)
          report.errorAndAbort(s"Different number of new-function-arguments (${argsAsTerms}) and old-function-arguments (${allSymbols}) detected.")

        val mappings = allSymbols.zip(argsAsTerms)
        //val mappings = allSymbols.zip(List('{ ??? }.asTerm))

        // Note: assuming at this point that the right-hand-side exists hence rhs.get. Need to replace the Idents to the old function with idents pointing to the new one.
        val originalBody = defdef.rhs.get
        val originalArgsReplaced = Trees.replaceIdents(originalBody, defdef.symbol.owner)(mappings:_*)
        // Once we have corrected all the identifiers on the new body, pass it to downstream processing
        Some(prepareBody(args, originalArgsReplaced))
      }
    )

    newDefDef
  }

  private def symbolsOfTerms(using Quotes)(params: quotes.reflect.ParamClause): List[quotes.reflect.Symbol] =
    import quotes.reflect._
    params match
      case clause: TermParamClause => clause.params.map(_.symbol)
      // TODO need to figure out what to do for PolyFunction with types
      case clause: TypeParamClause => List()
}