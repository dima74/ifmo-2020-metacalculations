package sc

fun main() {
    val add = lambdas(
        listOf("n", "m"),
        case(
            variable("m"),
            pattern("Z") to variable("n"),
            pattern("S", "m1") to
                    constructor(
                        "S",
                        application(
                            function("add"),
                            variable("n"),
                            variable("m1"),
                        )
                    ),
        )
    )
    val expr = application(function("add"), 1.nat, 2.nat)
    val program = Program(expr, hashMapOf("add" to add))
    val result = supercompile(program)
    println(result)
}
