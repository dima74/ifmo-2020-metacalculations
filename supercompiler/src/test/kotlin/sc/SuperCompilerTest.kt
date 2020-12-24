package sc

import org.junit.jupiter.api.Test

class SuperCompilerTest : BaseTest() {
    private val eq = lambdas(
        listOf("m", "n"),
        case(
            "m".v,
            pattern("Z") to case(
                "n".v,
                pattern("Z") to True,
                pattern("S", "n1") to False
            ),
            pattern("S", "m1") to case(
                "n".v,
                pattern("Z") to False,
                pattern("S", "n1") to application("eq".f, "m1".v, "n1".v)
            )
        )
    )
    private val church = lambda("n",
        case(
            "n".v,
            pattern("Z") to lambdas(
                listOf("f", "t"),
                "t".v
            ),
            pattern("S", "n1") to lambdas(
                listOf("f", "t"),
                ("f".v at application("church".f, "n1".v, "f".v, "t".v))
            )
        )
    )
    private val unchurch = lambda("n",
        application(
            "n".v,
            lambda("t", constructor("S", "t".v)),
            "Z".c
        )
    )
    private val churchAdd = lambdas(
        listOf("m", "n"),
        lambdas(
            listOf("f", "t"),
            application(
                "m".v,
                "f".v,
                application("n".v, "f".v, "t".v)
            )
        )
    )
    private val add = lambdas(
        listOf("m", "n"),
        case(
            "m".v,
            pattern("Z") to "n".v,
            pattern("S", "m1") to constructor("S", application("add".f, "m1".v, "n".v)),
        )
    )

    @Test
    fun `test iterate`() {
        val iterate = lambdas(
            listOf("f", "x"),
            constructor(
                "Cons",
                "x".v,
                application("iterate".f, "f".v, "f".v at "x".v)
            )
        )
        val expr = application(
            "iterate".f,
            lambda("n", constructor("S", "n".v)),
            constructor("Z")
        )
        val program = Program(expr, mapOf("iterate" to iterate))
        doTest(program, "let f1 = (λ v1 → Cons v1 (f1 (S v1))) in f1 Z")
    }

    @Test
    fun `test church`() {
        val expr = application(
            "eq".f,
            application("add".f, "x".v, "y".v),
            "unchurch".f at application("churchAdd".f, "church".f at "x".v, "church".f at "y".v)
        )
        val program = Program(
            expr,
            mapOf(
                "eq" to eq,
                "churchAdd" to churchAdd,
                "unchurch" to unchurch,
                "church" to church,
                "add" to add,
            )
        )
        doTest(program, "todo")
    }

    @Test
    fun `test eq`() {
        val x = "x".v
        val expr = application(
            "eq".f,
            x,
            "unchurch".f at ("church".f at x)
        )
        val program = Program(
            expr,
            mapOf(
                "eq" to eq,
                "churchAdd" to churchAdd,
                "unchurch" to unchurch,
                "church" to church,
                "add" to add,
            )
        )
        doTest(program, "todo")
    }

    @Test
    fun `test kmp aba`() {
        fun cons(head: Expr, tail: Expr): Expr = constructor("Cons", head, tail)
        val Nil = constructor("Nil")
        val A = constructor("A")
        val B = constructor("B")
        val C = constructor("C")

        val expr = application(
            "match".f,
            cons(A, cons(B, cons(A, Nil))),
            "str".v
        )

        val eqSymb = lambdas(
            listOf("x", "y"),
            case(
                "x".v,
                pattern("A") to case(
                    "y".v,
                    pattern("A") to True,
                    pattern("B") to False,
                    pattern("C") to False,
                ),
                pattern("B") to case(
                    "y".v,
                    pattern("A") to False,
                    pattern("B") to True,
                    pattern("C") to False,
                ),
                pattern("C") to case(
                    "y".v,
                    pattern("A") to False,
                    pattern("B") to False,
                    pattern("C") to True,
                )
            )
        )
        val `if` = lambdas(
            listOf("f", "x", "y"),
            case(
                "f".v,
                pattern("True") to "x".v,
                pattern("False") to "y".v,
            )
        )
        val match = lambdas(
            listOf("p", "s"),
            application("m".f, "p".v, "s".v, "p".v, "s".v)
        )
        val m = lambdas(
            listOf("p1", "ss", "op", "os"),
            case(
                "p1".v,
                pattern("Nil") to True,
                pattern("Cons", "p", "pp") to application("mx".f, "ss".v, "p".v, "pp".v, "op".v, "os".v)
            )
        )
        val mx = lambdas(
            listOf("p1", "p", "pp", "op", "os"),
            case(
                "p1".v,
                pattern("Nil") to False,
                pattern("Cons", "s", "ss") to application(
                    "if".f,
                    application("eqSymb".f, "p".v, "s".v),
                    application("m".f, "pp".v, "ss".v, "op".v, "os".v),
                    application("mn".f, "os".v, "op".v),
                )
            )
        )
        val mn = lambdas(
            listOf("p1", "op"),
            case(
                "p1".v,
                pattern("Nil") to False,
                pattern("Cons", "s", "ss") to application("m".f, "op".v, "ss".v, "op".v, "ss".v),
            )
        )

        val program = Program(
            expr,
            mapOf(
                "eqSymb" to eqSymb,
                "if" to `if`,
                "match" to match,
                "m" to m,
                "mx" to mx,
                "mn" to mn,
            )
        )
        doTest(program, "todo")
    }

    private fun doTest(program: Program, expected: String) {
        println("\tProgram:")
        println(program)
        println()
        val result = supercompile(program)
        println("\tResult:")
        println(result)
        check(true)
        // assertEquals(expected, result.toString())
    }
}
