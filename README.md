# Metaprogramming

## Mix
* [`int_tm_racket.rkt`](mix/old/int_tm_racket.rkt) - интерпретатор машины Тьюринга на Racket
* [`int_flow_racket.rkt`](mix/int_flow_racket.rkt) - интерпретатор FlowChart на Racket
* [`int_tm_flow.rkt`](mix/int_tm_flow.rkt) - интерпретатор машины Тьюринга на FlowChart
* [`mix.rkt`](mix/mix.rkt) - mix (для интерпретатора машины Тьюринга)
* [`base.rkt`](mix/base.rkt) - вспомогательные функции, в том числе `eval` и `reduce`

## Super Compiler
* [code](supercompiler/src/main/kotlin/sc)
* [examples](supercompiler/src/test/kotlin/sc/SuperCompilerTest.kt) ([iterate](https://github.com/dima74/ifmo-2020-metacalculations/blob/7b3d97df6c45531a1921545c628431f9b471cce8/supercompiler/src/test/kotlin/sc/SuperCompilerTest.kt#L63), [church](https://github.com/dima74/ifmo-2020-metacalculations/blob/7b3d97df6c45531a1921545c628431f9b471cce8/supercompiler/src/test/kotlin/sc/SuperCompilerTest.kt#L82))
* Entry point: [`supercompile.kt`](supercompiler/src/main/kotlin/sc/supercompile.kt)
