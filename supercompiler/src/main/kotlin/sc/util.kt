package sc

infix fun <A, B> Collection<A>.zipExact(other: Collection<B>): List<Pair<A, B>> {
    check(size == other.size)
    return this zip other
}

fun <A, B, C> zip3(a: Collection<A>, b: Collection<B>, c: Collection<C>): Collection<Triple<A, B, C>> =
    ((a zipExact b) zipExact c).map { Triple(it.first.first, it.first.second, it.second) }

val Int.nat: Expr
    get() = when (this) {
        0 -> constructor("Z")
        else -> constructor("S", (this - 1).nat)
    }

val <T> T.exhaustive: T get() = this
