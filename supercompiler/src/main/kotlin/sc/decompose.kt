package sc

sealed class Decomposition

/** [Lets] не встречается в исходном выражении, а мы генерируем [Lets] только на верхнем уровне */
class DecomposeLets(val lets: Lets) : Decomposition()

sealed class Observable : Decomposition()
data class ObsVar(val variable: Variable, val exprs: List<Expr>, val originalExpr: Expr) : Observable()
data class ObsCons(val constructor: Constructor) : Observable()
data class ObsLam(val lambda: Lambda) : Observable()

class Context(
    val expr: Expr,
    /** subexpression in [expr], equal to [redex] */
    val hole: Expr,
    val redex: Redex,
) : Decomposition() {
    fun replaceHole(new: Expr): Expr = expr.replace { if (it === hole) new else null }
}

sealed class Redex
data class RedexFunction(val function: Function) : Redex()
data class RedexApplication(val lambda: Lambda, val expr: Expr) : Redex()
data class RedexCaseVar(val value: ObsVar, val arms: List<Arm>) : Redex()
data class RedexCaseCons(val value: ObsCons, val arms: List<Arm>) : Redex()

fun decompose(expr: Expr): Decomposition {
    return when (expr) {
        is Variable -> ObsVar(expr, emptyList(), expr)
        is Function -> Context(expr, expr, RedexFunction(expr))
        is Constructor -> ObsCons(expr)
        is Lambda -> ObsLam(expr)
        is Lets -> DecomposeLets(expr)
        is Application -> {
            when (val decompose1 = decompose(expr.expr1)) {
                is DecomposeLets -> error("unreachable")
                is ObsVar -> ObsVar(decompose1.variable, decompose1.exprs + expr.expr2, expr)
                is ObsCons -> error("Can't apply anything to constructor")
                is ObsLam -> Context(expr, expr, RedexApplication(decompose1.lambda, expr.expr2))
                is Context -> Context(expr, decompose1.hole, decompose1.redex)
            }
        }
        is Case -> {
            when (val decompose = decompose(expr.value)) {
                is DecomposeLets -> error("unreachable")
                is ObsVar -> Context(expr, expr, RedexCaseVar(decompose, expr.arms))
                is ObsCons -> Context(expr, expr, RedexCaseCons(decompose, expr.arms))
                is ObsLam -> error("Can't pattern match on lambda")
                is Context -> Context(expr, decompose.hole, decompose.redex)
            }
        }
    }
}
