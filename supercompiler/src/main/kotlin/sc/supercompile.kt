package sc

class Program(
    val expr: Expr,
    val functions: Map<String, Expr>,
) {
    override fun toString(): String {
        val functions = functions.entries.joinToString("\n") { "${it.key} = ${it.value}" }
        return "$expr where\n\n$functions"
    }
}

class Node(
    val expr: Expr,
    val parent: Node?,
    /** Обратная дуга из повторного (рекурсивного) узла `this` в базовый (или функциональный) узел [next] */
    var next: BackwardEdge? = null,
) {
    private var child: Node? = null  // for debug

    @Suppress("PrivatePropertyName")
    private var children_: MutableList<Node>? = null
    var children: List<Node>
        get() = children_ ?: emptyList()
        set(value) {
            check(children_ == null)
            children_ = value.toMutableList()
            child = value.singleOrNull()
        }

    fun replaceChild(old: Node, new: Node) {
        val children = children_!!
        check(children.remove(old))
        children += new
    }

    val decompose: Decomposition = decompose(expr)
    val parents: Sequence<Node> get() = generateSequence(parent) { it.parent }
    val descendants: Sequence<Node> get() = sequenceOf(this) + children.asSequence().flatMap { it.descendants }

    /** Renaming for all nodes such that `node.next == this` */
    val previous: MutableList<Renaming> = mutableListOf()
    var generateBackwardCall: ((Renaming) -> Expr)? = null

    fun visitSubtree(visitor: (Node) -> Unit) {
        visitor(this)
        for (child in children) {
            child.visitSubtree(visitor)
        }
    }

    override fun toString(): String = expr.toString()
}

typealias Renaming = Map<String, String>

class BackwardEdge(val target: Node, val renaming: Renaming)

fun supercompile(program: Program): Expr = SuperCompiler(program).supercompile()

private fun Program.getFreshVariableGenerator(prefix: String): NameGenerator =
    getFreshVariableGenerator(prefix, listOf(expr) + functions.values)

private class SuperCompiler(val program: Program) {
    private var root = Node(program.expr, null)
    private val unprocessed = ArrayDeque<Node>()

    private val xGenerator: NameGenerator = program.getFreshVariableGenerator("x")
    private val yGenerator: NameGenerator = program.getFreshVariableGenerator("y")
    private val zGenerator: NameGenerator = program.getFreshVariableGenerator("z")

    fun supercompile(): Expr {
        checkNoLetExpressions(program)
        buildTree()
        calculateForwardEdges(root)
        val code = generateCode(root)
        return code.replaceBoundVariablesToFresh(xGenerator)
    }

    fun buildTree() {
        unprocessed += root
        while (unprocessed.isNotEmpty()) {
            val node = unprocessed.removeFirst()
            // println("node: $node")
            if (node.isTrivial) {
                drive(node)
            } else {
                if (tryRenaming(node)) continue
                if (trySubstitution(node)) continue
                if (tryGeneralization(node)) continue
                drive(node)
            }
        }
        root.visitSubtree { check(it.isProcessed) { "Found unprocessed node: $it" } }
    }

    val Node.isTrivial: Boolean
        get() = !(decompose is Context && (decompose.redex is RedexFunction || decompose.redex is RedexCaseVar))

    val Node.isProcessed: Boolean
        get() = next != null
                || (expr is Constructor && expr.args.isEmpty())
                || expr is Variable
                || children.isNotEmpty()

    fun drive(node: Node) {
        val children = when (val decompose = node.decompose) {
            is DecomposeLets -> {
                val lets = decompose.lets
                listOf(lets.expr) + lets.substitution.toSortedMap().values
            }
            is ObsVar -> {
                if (decompose.exprs.isEmpty()) return  // локальная переменная
                listOf(decompose.variable) + decompose.exprs
            }
            is ObsCons -> decompose.constructor.args.also { if (it.isEmpty()) return }
            is ObsLam -> listOf(decompose.lambda.body)
            is Context -> {
                val redex = decompose.redex
                val contextChildren = redex.drive().map {
                    decompose.replaceHole(it.replaceBoundVariablesToFresh(yGenerator))
                }
                if (redex is RedexCaseVar) {
                    listOf(redex.value.originalExpr) + contextChildren
                } else {
                    contextChildren
                }
            }
        }
        check(children.isNotEmpty())
        node.children = children.mapTo(mutableListOf()) { Node(it, node) }
        unprocessed += node.children
    }

    fun Redex.drive(): List<Expr> =
        when (this) {
            is RedexFunction -> listOf(program.functions[function.name]!!)
            is RedexApplication -> listOf(lambda.body.substitute(lambda.variable, expr))
            is RedexCaseVar -> arms.map { arm ->
                val pattern = constructor(arm.constructor, arm.bindings.map(::Variable))
                // todo делать replace не только для hole, а для всего top-level expr?
                //  что если `let x = ... in (case x of ...)` и мы хотим подставить x?
                arm.expr.replace { if (it isAlphaEquiv value.originalExpr) pattern else null }
            }
            is RedexCaseCons -> {
                val constructor = value.constructor
                val (pattern, expr) = arms.single { it.constructor == constructor.name }
                check(pattern.bindings.size == constructor.args.size)
                listOf(expr.substitute(pattern.bindings, constructor.args))
            }
        }

    private fun tryRenaming(node: Node): Boolean {
        for (parent in node.parents) {
            val renaming = getRenaming(parent.expr, node.expr, zGenerator) ?: continue
            node.next = BackwardEdge(parent, renaming)
            return true
        }
        return false
    }

    private fun trySubstitution(node: Node): Boolean {
        for (parent in node.parents) {
            // тут нужно использовать исходную generalization, чтобы были свежие переменные
            val (result, substParent, substNode) = getPartialCase(parent.expr, node.expr, zGenerator) ?: continue
            abstract(node, Generalization(result, substNode, substParent))
            return true
        }
        return false
    }

    private fun tryGeneralization(node: Node): Boolean {
        for (parent in node.parents) {
            if (!hasEmbeddingUsingCoupling(parent.expr, node.expr, emptySet())) continue
            val generalization = generalize(parent.expr, node.expr, zGenerator)
            abstract(parent, generalization)
            return true
        }
        return false
    }

    @Suppress("UNUSED_VARIABLE")
    private fun abstract(node: Node, generalization: Generalization) {
        val (abstractExpr, nodeSubst, otherSubst) = generalization
        val newExpr = lets(nodeSubst, abstractExpr)
        val newNode = Node(newExpr, node.parent)
        replace(node, newNode)
    }

    private fun replace(old: Node, new: Node) {
        // println("replace: $old => $new")
        val descendants = old.descendants.toSet()
        unprocessed.removeIf { it in descendants }
        unprocessed += new
        doReplace(old, new)
    }

    private fun doReplace(old: Node, new: Node) {
        if (root === old) {
            root = new
        } else {
            old.parent!!.replaceChild(old, new)
        }
    }

    private fun calculateForwardEdges(root: Node) {
        root.visitSubtree {
            val next = it.next ?: return@visitSubtree
            next.target.previous += next.renaming
        }
    }

    class GenerateContext(val fGenerator: NameGenerator, val vGenerator: NameGenerator)

    fun generateCode(root: Node): Expr {
        val allExpressions = root.descendants.map { it.expr }.toList()
        val context = GenerateContext(
            getFreshVariableGenerator("f", allExpressions),
            getFreshVariableGenerator("v", allExpressions)
        )
        return context.generate(root)
    }

    private fun GenerateContext.generate(node: Node): Expr = when (val decompose = node.decompose) {
        is DecomposeLets -> {
            val nodeExpr = node.children.first()
            val nodeValues = node.children.drop(1)
            val values = nodeValues.map { generate(it) }
            val names = decompose.lets.substitution.keys.sorted()
            generate(nodeExpr).substitute(names, values)
        }
        is ObsVar -> {
            // первый элемент это переменная (зачем вообще её нужно было генерировать?)
            val argsNodes = node.children.drop(1)
            check(decompose.exprs.size == argsNodes.size)
            application0(decompose.variable, argsNodes.map { generate(it) })
        }
        is ObsCons -> {
            val constructor = decompose.constructor
            check(constructor.args.size == node.children.size)
            constructor(constructor.name, node.children.map { generate(it) })
        }
        is ObsLam -> {
            lambda(decompose.lambda.variable, generate(node.children.single()))
        }
        is Context -> when (val redex = decompose.redex) {
            is RedexApplication, is RedexCaseCons -> generate(node.children.single())
            is RedexFunction -> generateNonTrivialNode(node) {
                generate(node.children.single())
            }
            is RedexCaseVar -> generateNonTrivialNode(node) {
                val value = generate(node.children.first())
                val exprs = node.children.drop(1).map { generate(it) }
                val arms = (redex.arms zipExact exprs).map { (arm, expr) -> Arm(arm.pattern, expr) }
                case(value, arms)
            }
        }
    }

    private fun GenerateContext.generateNonTrivialNode(node: Node, generateLambdaBody: () -> Expr): Expr = when {
        node.next != null -> {
            val next = node.next!!
            next.target.generateBackwardCall!!(next.renaming)
        }
        node.previous.isNotEmpty() -> {
            val originalVariables = node.previous.map { it.keys }.flatten().distinct()
            val freshVariables = vGenerator * originalVariables.size

            val f = fGenerator()
            node.generateBackwardCall = { renaming ->
                val args = originalVariables.map { Variable(renaming.getOrDefault(it, it)) }
                application0(Variable(f), args)
            }

            val child = generateLambdaBody()
            val lambdaBody = child.substitute(originalVariables, freshVariables.map(::Variable))
            val lambda = lambdas(freshVariables, lambdaBody)

            lets(
                mapOf(f to lambda),
                application0(Variable(f), originalVariables.map(::Variable))
            )
        }
        else -> generateLambdaBody()
    }
}

fun checkNoLetExpressions(program: Program) {
    for (expr in program.functions.values + program.expr) {
        expr.visit {
            check(it !is Lets) { "Can't supercompile `let`. They should be replaced with global functions" }
        }
    }
}
