package zio.asyncawait.core.metaprog

import scala.quoted._
import zio.asyncawait.core.metaprog.Extractors.Lambda1
import zio.asyncawait.core.util.Format
import scala.meta.Type.ByName.apply

object DefDefCopy {

  private def methodTypeMaker(using Quotes)(clause: quotes.reflect.TermParamClause): quotes.reflect.TypeRepr => quotes.reflect.MethodType =
    import quotes.reflect._
    (outputType: TypeRepr) =>
      MethodType(clause.params.map(_.name))(_ => clause.params.map(_.tpt.tpe), _ => outputType)

  private def polyTypeMaker(using Quotes)(clause: quotes.reflect.TypeParamClause): quotes.reflect.TypeRepr => quotes.reflect.MethodType =
    import quotes.reflect._
    // TODO what about await clauses in lambdas? Should there be an explicit check to disable them?
    report.throwError("Poly types with await-clauses are not supported yet. Please move the await clause out of the function.")

  private def outputTypeMaker(using Quotes)(params: quotes.reflect.ParamClause): quotes.reflect.TypeRepr => quotes.reflect.MethodType | quotes.reflect.PolyType =
    import quotes.reflect._
    params match
      case clause: TermParamClause => methodTypeMaker(clause)
      case clause: TypeParamClause => polyTypeMaker(clause)

  // TODO check for a given-params clause and error that given-params clauses with awaits are not supported
  // TODO Also, how to do you create methods with parameters with default values?
  def of(using Quotes)(defdef: quotes.reflect.DefDef, body: quotes.reflect.Term, functionOutputType: quotes.reflect.TypeRepr): quotes.reflect.DefDef = {
    import quotes.reflect._

    def handleParamClause(clause: ParamClause, rest: List[ParamClause]): TypeRepr =
      rest match
        case Nil => outputTypeMaker(clause)(functionOutputType)
        case head :: tail => outputTypeMaker(clause)(handleParamClause(head, tail))

    val methodType =
      defdef.paramss match
        case Nil => ByNameType(functionOutputType)
        case head :: tail => handleParamClause(head, tail)

    val methodSymbol = Symbol.newMethod(Symbol.spliceOwner, defdef.name, functionOutputType)
    // TODO replace symbols in body?
    DefDef.apply(methodSymbol, terms => Some(body.changeOwner(methodSymbol)))
  }
}