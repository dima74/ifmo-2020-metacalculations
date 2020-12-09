@file:Suppress("NonAsciiCharacters")

package sc

typealias Substitution = Map<String, Expr>

fun Expr.substitute(substitution: Substitution): Expr {
    if ((substitution.keys intersect boundVariables()).isNotEmpty()) error("Attempt to substitute bound variable")
    return doSubstitute(substitution)
}

fun Expr.substitute(names: List<String>, exprs: List<Expr>): Expr = substitute((names zipExact exprs).toMap())

fun Expr.substitute(name: String, expr: Expr): Expr = substitute(hashMapOf(name to expr))

private fun Expr.doSubstitute(substitution: Substitution) = replace { expr ->
    if (expr is Lambda) check(expr.variable !in substitution)
    if (expr is Case) check(expr.arms.all { it.bindings.none(substitution::contains) })
    if (expr is Lets) check(expr.substitution.keys.none(substitution::contains))

    if (expr is Variable) substitution[expr.name] else null
}

fun generalize(expr1: Expr, expr2: Expr, freshVariable: NameGenerator): Triple<Expr, Substitution, Substitution> {
    val (result, subst1, subst2) = doGeneralize(expr1, expr2, freshVariable)
    check(expr1 isAlphaEquiv result.substitute(subst1))
    check(expr2 isAlphaEquiv result.substitute(subst2))
    return Triple(result, subst1, subst2)
}

typealias Generalization = Triple<Expr, Substitution, Substitution>

private fun doGeneralize(expr1: Expr, expr2: Expr, freshVariable: NameGenerator): Generalization {
    var expr: Expr = Variable(freshVariable())
    expr as Variable
    val subst1 = hashMapOf(expr.name to expr1)
    val subst2 = hashMapOf(expr.name to expr2)
    do {
        var changed = false

        // общий функтор
        for (name in subst1.keys intersect subst2.keys) {
            val e1 = subst1[name]!!
            val e2 = subst2[name]!!
            when {
                e1 is Variable && e2 is Variable && e1.name == e2.name -> {
                    subst1.remove(name)
                    subst2.remove(name)
                    expr = expr.substitute(name, e1)
                }
                // (?) этого правила нет в статье
                e1 is Function && e2 is Function && e1.name == e2.name -> {
                    subst1.remove(name)
                    subst2.remove(name)
                    expr = expr.substitute(name, e1)
                }
                e1 is Constructor && e2 is Constructor && e1.name == e2.name -> {
                    check(e1.args.size == e2.args.size)
                    val variables = freshVariable * e1.args.size
                    subst1.remove(name)
                    subst2.remove(name)
                    subst1 += variables zipExact e1.args
                    subst2 += variables zipExact e2.args
                    expr = expr.substitute(name, constructor(e1.name, variables.map(::Variable)))
                }
                e1 is Lambda && e2 is Lambda -> {
                    val nameVariable = freshVariable()
                    val nameBody = freshVariable()
                    subst1.remove(name)
                    subst2.remove(name)
                    subst1[nameBody] = e1.body.substitute(e1.variable, Variable(nameVariable))
                    subst2[nameBody] = e2.body.substitute(e2.variable, Variable(nameVariable))
                    expr = expr.substitute(name, lambda(nameVariable, Variable(nameBody)))
                }
                e1 is Application && e2 is Application -> {
                    val name1 = freshVariable()
                    val name2 = freshVariable()
                    subst1.remove(name)
                    subst2.remove(name)
                    subst1[name1] = e1.expr1
                    subst1[name2] = e1.expr2
                    subst2[name1] = e2.expr1
                    subst2[name2] = e2.expr2
                    expr = expr.substitute(name, Application(Variable(name1), Variable(name2)))
                }
                e1 is Case && e2 is Case -> {
                    check(e1.arms.map { it.constructor } == e2.arms.map { it.constructor })
                    val nameValue = freshVariable()
                    val nameExprs = freshVariable * e1.arms.size
                    val resultArms = mutableListOf<Arm>()
                    for ((arm1, arm2, nameExpr) in zip3(e1.arms, e2.arms, nameExprs)) {
                        check(arm1.bindings.size == arm2.bindings.size)
                        val nameBindings = freshVariable * arm1.bindings.size
                        val variableBindings = nameBindings.map(::Variable)
                        subst1[nameExpr] = arm1.expr.substitute(arm1.bindings, variableBindings)
                        subst2[nameExpr] = arm2.expr.substitute(arm2.bindings, variableBindings)
                        resultArms += Arm(arm1.constructor, nameBindings, Variable(nameExpr))
                    }
                    subst1.remove(name)
                    subst2.remove(name)
                    subst1[nameValue] = e1.value
                    subst2[nameValue] = e2.value
                    expr = expr.substitute(name, case(Variable(nameValue), resultArms))
                }
                else -> continue
            }
            changed = true
        }

        // общее подвыражение
        val common = subst1.keys intersect subst2.keys
        for (name1 in common) {
            for (name2 in common) {
                if (name1 == name2) continue
                val e11 = subst1[name1] ?: continue
                val e12 = subst1[name2] ?: continue
                val e21 = subst2[name1] ?: continue
                val e22 = subst2[name2] ?: continue
                if (e11 == e12 && e21 == e22) {
                    subst1.remove(name1)
                    subst2.remove(name1)
                    expr = expr.substitute(name1, Variable(name2))
                    changed = true
                }
            }
        }
    } while (changed)
    return Triple(expr, subst1, subst2)
}

infix fun Expr.isAlphaEquiv(other: Expr): Boolean {
    val expr1 = this.replaceBoundVariablesToFresh(getFreshVariableGenerator("z"))
    val expr2 = other.replaceBoundVariablesToFresh(getFreshVariableGenerator("z"))
    return expr1 == expr2
}

/** 3.1.8 Переименование выражения */
fun getRenaming(expr1: Expr, expr2: Expr, freshVariable: NameGenerator): Map<String, String>? {
    // todo это корректно ?
    val (_, subst1, subst2) = generalize(expr1, expr2, freshVariable)
    if (subst1.size != subst2.size) return null
    val result = hashMapOf<String, String>()
    for ((name, e1) in subst1) {
        val e2 = subst2[name] ?: return null
        if (e1 !is Variable || e2 !is Variable) return null
        result[e1.name] = e2.name
    }
    return result
}

/** 3.1.7 Частный случай выражения */
fun getPartialCase(expr1: Expr, expr2: Expr, freshVariable: NameGenerator): Generalization? {
    // todo это корректно ?
    val generalization = generalize(expr1, expr2, freshVariable)
    val (_, subst1, subst2) = generalization
    if (subst1.size != subst2.size) return null
    if (subst1.values.any { it !is Variable }) return null
    return generalization
}

/** 4.1 Расширенное гомеоморфное вложение */
fun hasEmbedding(expr1: Expr, expr2: Expr, p: Set<Pair<String, String>>): Boolean {
    return hasEmbeddingUsingVariables(expr1, expr2, p)
            || hasEmbeddingUsingInner(expr1, expr2, p)
            || hasEmbeddingUsingCoupling(expr1, expr2, p)
}

/** 4.2 Вложение переменных */
fun hasEmbeddingUsingVariables(expr1: Expr, expr2: Expr, p: Set<Pair<String, String>>): Boolean {
    return when {
        expr1 is Function && expr2 is Function -> expr1.name == expr2.name
        expr1 is Variable && expr2 is Variable -> {
            if ((expr1.name to expr2.name) in p) return true
            if (p.any { it.first == expr1.name || it.second == expr2.name }) return false
            true
        }
        else -> false
    }
}

const val circle: String = "●"

/** 4.3 Вложение через погружение */
fun hasEmbeddingUsingInner(expr1: Expr, expr2: Expr, p: Set<Pair<String, String>>): Boolean {
    // ∀v ∈ fv(e) : v !in domain(ρ)
    val freeVariables = expr1.freeVariables()
    if (p.any { it.first in freeVariables }) return false

    return when (expr2) {
        is Constructor -> expr2.args.any { hasEmbedding(expr1, it, p) }
        is Lambda -> hasEmbedding(expr1, expr2.body, p + (circle to expr2.variable))
        is Application -> hasEmbedding(expr1, expr2.expr1, p) || hasEmbedding(expr1, expr2.expr2, p)
        is Case -> {
            val valueMatches = hasEmbedding(expr1, expr2.value, p)
            val armsMatches = expr2.arms.any { arm2 ->
                hasEmbedding(expr1, arm2.expr, p + arm2.bindings.map { circle to it })
            }
            valueMatches && armsMatches
        }
        else -> false
    }
}

/** 4.4 Вложение через сцепление */
fun hasEmbeddingUsingCoupling(expr1: Expr, expr2: Expr, p: Set<Pair<String, String>> = setOf()): Boolean {
    return when {
        expr1 is Constructor && expr2 is Constructor && expr1.name == expr2.name -> {
            (expr1.args zipExact expr2.args).all { (e1, e2) -> hasEmbedding(e1, e2, p) }
        }
        expr1 is Lambda && expr2 is Lambda -> {
            hasEmbedding(expr1.body, expr2.body, p + (expr1.variable to expr2.variable))
        }
        expr1 is Application && expr2 is Application -> {
            hasEmbedding(expr1.expr1, expr2.expr1, p) && hasEmbedding(expr1.expr2, expr2.expr2, p)
        }
        expr1 is Case && expr2 is Case -> {
            val valueMatches = hasEmbedding(expr1.value, expr2.value, p)
            val armsMatches = (expr1.arms zipExact expr2.arms).all { (arm1, arm2) ->
                check(arm1.constructor == arm2.constructor)
                hasEmbedding(arm1.expr, arm2.expr, p + (arm1.bindings zipExact arm2.bindings))
            }
            valueMatches && armsMatches
        }
        else -> false
    }
}
