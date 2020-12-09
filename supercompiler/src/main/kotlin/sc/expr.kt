package sc

sealed class Expr

data class Variable(val name: String) : Expr() {
    override fun toString(): String = toStringImpl()
}

// global variable
data class Function(val name: String) : Expr() {
    override fun toString(): String = toStringImpl()
}

data class Constructor(val name: String, val args: List<Expr>) : Expr() {
    override fun toString(): String = toStringImpl()
}

data class Lambda(val variable: String, val body: Expr) : Expr() {
    override fun toString(): String = toStringImpl()
}

data class Application(val expr1: Expr, val expr2: Expr) : Expr() {
    init {
        check(expr1 !is Constructor)
    }

    override fun toString(): String = toStringImpl()
}

data class Case(val value: Expr, val arms: List<Arm>) : Expr() {
    override fun toString(): String = toStringImpl()

    init {
        check(arms.isNotEmpty())
    }
}

data class Lets(val substitution: Substitution, val expr: Expr) : Expr() {
    override fun toString(): String = toStringImpl()
}

data class Pattern(val constructor: String, val bindings: List<String>) {
    override fun toString(): String = constructor + bindings.joinToString("") { " $it" }
}

data class Arm(val pattern: Pattern, val expr: Expr) {
    val constructor: String get() = pattern.constructor
    val bindings: List<String> get() = pattern.bindings

    constructor(constructor: String, bindings: List<String>, expr: Expr) : this(Pattern(constructor, bindings), expr)
}

private fun <T : Expr> T.check(): T {
    val boundVariables = boundVariables()
    val freeVariables = freeVariables()
    val intersection = boundVariables intersect freeVariables
    check(intersection.isEmpty())
    return this
}

fun variable(name: String): Variable = Variable(name)
fun function(name: String): Function = Function(name)
fun constructor(name: String, vararg args: Expr): Constructor = constructor(name, args.toList())
fun constructor(name: String, args: List<Expr>): Constructor = Constructor(name, args).check()
fun lambda(variable: String, expr: Expr): Lambda = Lambda(variable, expr).check()
fun lambdas(variables: List<String>, expr: Expr): Expr = variables.foldRight(expr, ::Lambda).check()
fun application(vararg exprs: Expr): Expr = application(exprs.toList())
fun application(exprs: List<Expr>): Expr = exprs.reduce(::Application).check()
fun application(first: Expr, exprs: List<Expr>): Expr = application(listOf(first) + exprs)
fun application0(first: Expr, exprs: List<Expr>): Expr = if (exprs.isEmpty()) first else application(first, exprs)
infix fun Expr.at(expr: Expr): Expr = Application(this, expr).check()
fun case(value: Expr, vararg arms: Pair<Pattern, Expr>): Case = case(value, arms.map { Arm(it.first, it.second) })
fun case(value: Expr, arms: List<Arm>): Case = Case(value, arms).check()
fun pattern(vararg strings: String): Pattern = Pattern(strings.first(), strings.drop(1))
fun lets(substitution: Substitution, expr: Expr): Expr = Lets(substitution, expr).check()

fun Lambda.unfold(): Pair<List<Lambda>, Expr> {
    val lambdas = mutableListOf(this)
    var body = body
    while (body is Lambda) {
        lambdas += body
        body = body.body
    }
    return lambdas to body
}

infix fun Expr.replace(replacer: (Expr) -> Expr?): Expr {
    replacer(this)?.let { return it }
    return when (this) {
        is Variable -> this
        is Function -> this
        is Constructor -> constructor(name, args.map { it replace replacer })
        is Lambda -> lambda(variable, body replace replacer)
        is Application -> application(expr1 replace replacer, expr2 replace replacer)
        is Case -> case(value replace replacer, arms.map { Arm(it.pattern, it.expr replace replacer) })
        is Lets -> lets(substitution.mapValues { (_, value) -> value replace replacer }, expr replace replacer)
    }
}

private fun Expr.toStringImpl(): String = prettyImpl(false)

private fun Expr.pretty(withBrackets: Boolean): String = prettyImpl(withBrackets || this is Lambda)

private fun Expr.prettyImpl(withBrackets: Boolean): String {
    val result = when (this) {
        is Variable -> return name
        is Function -> return name
        is Constructor -> {
            if (args.isEmpty()) return name
            name + args.joinToString("") { " ${it.pretty(true)}" }
        }
        is Lambda -> {
            val (lambdas, body) = unfold()
            val variables = lambdas.joinToString(" ") { it.variable }
            "λ $variables → ${body.pretty(false)}"
        }
        is Application -> "${expr1.pretty(false)} ${expr2.pretty(true)}"
        is Case -> {
            val arms = arms.joinToString("\n") { arm -> "${arm.pattern} → ${arm.expr.pretty(false)}" }
            val armsIndented = arms.lines().map { "  $it" }.joinToString("\n")
            "case ${value.pretty(false)} of {\n$armsIndented\n}"
        }
        is Lets -> {
            val declarations = substitution.entries.joinToString { (variable, value) -> "$variable = ${value.pretty(false)}" }
            "let $declarations in ${expr.pretty(false)}"
        }
    }
    return if (withBrackets) "($result)" else result
}

fun Expr.visit(visitor: (Expr) -> Unit) {
    visitor(this)
    when (this) {
        is Variable -> Unit
        is Function -> Unit
        is Constructor -> args.forEach { it.visit(visitor) }
        is Lambda -> body.visit(visitor)
        is Application -> {
            expr1.visit(visitor)
            expr2.visit(visitor)
        }
        is Case -> {
            value.visit(visitor)
            arms.forEach { it.expr.visit(visitor) }
        }
        is Lets -> {
            substitution.values.forEach { it.visit(visitor) }
            expr.visit(visitor)
        }
    }.exhaustive
}

fun Expr.boundVariables(): Set<String> {
    val result = hashSetOf<String>()
    visit { expr ->
        if (expr is Lambda) result += expr.variable
        if (expr is Case) result += expr.arms.flatMap { it.bindings }
        if (expr is Lets) result += expr.substitution.keys
    }
    return result
}

fun Expr.freeVariables(): Set<String> {
    return when (this) {
        is Variable -> setOf(name)
        is Function -> setOf(name)
        is Constructor -> args.flatMapTo(hashSetOf()) { it.freeVariables() }
        is Lambda -> body.freeVariables() - variable
        is Application -> expr1.freeVariables() + expr2.freeVariables()
        is Case -> value.freeVariables() + arms.flatMap {
            it.expr.freeVariables() - it.bindings
        }
        is Lets -> (expr.freeVariables() + substitution.values.flatMap { it.freeVariables() }) - substitution.keys
    }
}

fun Expr.allVariables(): Set<String> = allTrueVariables() + boundVariables()

private fun Expr.allTrueVariables(): HashSet<String> {
    val result = hashSetOf<String>()
    visit { expr ->
        if (expr is Variable) result += expr.name
    }
    return result
}

fun interface NameGenerator {
    operator fun invoke(): String
    operator fun times(n: Int): List<String> = (1..n).map { invoke() }
}

fun getFreshVariableGenerator(prefix: String, exprs: List<Expr> = emptyList()): NameGenerator {
    val allVariables = exprs.flatMapTo(hashSetOf()) { it.allVariables() }
    val sequence = (1..Int.MAX_VALUE)
        .asSequence()
        .map { "$prefix$it" }
        .filter { it !in allVariables }
        .iterator()
    return NameGenerator { sequence.next() }
}

fun Expr.replaceBoundVariablesToFresh(generator: NameGenerator): Expr {
    fun Expr.replaced(): Expr = replaceBoundVariablesToFresh(generator)
    return when (this) {
        is Variable -> this
        is Function -> this
        is Constructor -> constructor(name, args.map { it.replaced() })
        is Lambda -> {
            val variableNew = generator()
            val bodyNew = body.replaced().substitute(variable, Variable(variableNew))
            lambda(variableNew, bodyNew)
        }
        is Application -> application(expr1.replaced(), expr2.replaced())
        is Case -> case(
            value.replaced(),
            arms.map { arm ->
                val bindingsFresh = generator * arm.bindings.size
                Arm(
                    Pattern(arm.constructor, bindingsFresh),
                    arm.expr.replaced().substitute(arm.bindings, bindingsFresh.map(::Variable))
                )
            }
        )
        is Lets -> {
            val substitution = substitution.toSortedMap()
            val freshVariables = generator * substitution.size
            val substitutionNew = (substitution.entries zipExact freshVariables)
                .map { (entry, freshVariable) ->
                    val (variable, value) = entry
                    freshVariable to value.replaced().substitute(variable, Variable(freshVariable))
                }
                .toMap()
            val exprNew = expr.replaced().substitute(substitution.keys.toList(), freshVariables.map(::Variable))
            lets(substitutionNew, exprNew)
        }
    }
}
