package sc

abstract class BaseTest {

    val True = constructor("True")
    val False = constructor("False")

    protected val String.v: Variable get() = variable(this)
    protected val String.f: Function get() = function(this)
    protected val String.c: Constructor get() = constructor(this)
}
