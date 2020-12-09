package sc

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class GeneralizeTest : BaseTest() {

    private val xGenerator = getFreshVariableGenerator("x", emptyList())

    @Test
    fun test1() = doTest(
        "map".f at "f".v,
        "map".f at application("compose".f, "f".v, "g".v),

        "map".f at "x2".v,
        mapOf("x2" to "f".v),
        mapOf("x2" to application("compose".f, "f".v, "g".v))
    )

    @Test
    fun test2() = doTest(
        lambda("x", constructor("Nil")),
        lambda("x", constructor("Cons", "x".v, "a".v)),

        lambda("x1", "x2".v),
        mapOf("x2" to constructor("Nil")),
        mapOf("x2" to constructor("Cons", "x1".v, "a".v))
    )

    @Test
    fun testTemp() {
        val expr1 = constructor("A", "x".v)
        val expr2 = constructor("A", 0.nat)
        val (result, subst1, subst2) = generalize(expr1, expr2, xGenerator)
        println(result)
        println(subst1)
        println(subst2)
    }

    private fun doTest(expr1: Expr, expr2: Expr, result: Expr, subst1: Substitution, subst2: Substitution) {
        val (actualResult, actualSubst1, actualSubst2) = generalize(expr1, expr2, xGenerator)
        println(actualResult)
        println(actualSubst1)
        println(actualSubst2)
        assertEquals(result, actualResult)
        assertEquals(subst1, actualSubst1)
        assertEquals(subst2, actualSubst2)
    }
}
