# hlox

this is a half-finished implementation of the programming language described in
['crafting interpreters' by robert nystrom](https://craftinginterpreters.com/).
in haskell. i have done my best to translate the semantics of the language to
haskell without making the exact same design decisions as the author, instead
opting for my impression of the idiomatic way to do things in haskell. so, for
instance, i use parser combinators instead of recursive descent to perform the
parsing.

## implemented so far

- arithmetic and boolean expressions
- variable declarations and assignment
- primitive control flow structures (branching, while & for loops)
- functions with arbitrary recursion

## yet to implement

- sensible behaviour for variables which are closed over
- classes and instances
- inheritance

## why?

after beginning to implement each of the unfinished features from above, i just
grew really uncomfortable with the way the code looked. it makes sense, because
in implementing the language in haskell, we express the an extremely different
execution model from the one we are executing in. take the following program
for example:

```lox
fun fib(n) {
  if (n <= 1) { return n; }
  return fib(n - 1) + fib(n - 2);
} 

for (var i = 1; i < 25; i = i + 1) {
  print(fib(i));
}
```

you can run this locally with `cabal run -- -f examples/rec.lox`, which produces
the expected output. it's clear from this code sample what i mean by the above,
though--it's an imperative, strict, dynamically typed language with mutable references
which i have done my best to implement in a functional, lazy, (mostly) pure language.

this has been really interesting; parts of the language still feel quite ergonomic
to develop in haskell. in particular, expressing the effects of the language
in mtl-style lets me reason about which parts of the execution require which
effects. for example, checking the type of a variable requires only state
representing the current execution environment, where throwing an error for
the incorrect type requires the state as well as some notion of an exception.

the vast majority of the execution still needs to take place in IO, though,
which also makes sense, but feels kinda gross.

### environments

i have written the execution environment in a pure way, represented as a list of
scopes (maps from identifier to language-level values), but this is
fundamentally at odds with settling on a clear and consistent semantics for the
three problems that i mentioned above. for example, what should the behaviour of
the following program be?

```lox
fun counter() {
  var n = 0;
  fun count() {
    n = n + 1;
    return n;
  }
  return count;
}

var count = counter();
print(count());
print(count());
```

with the pure environment implementation, where we add a new scope onto a stack
each time we enter a scope, `n` will be freed by the time `count` is called in
the outer scope, raising a runtime error. the book proposes storing along with
each function an execution context, but since my implementation is pure, if we
did this naively we would just end up printing 0 twice.

there are hacks i thought about to band-aid this problem that don't need to
leave the world of purity, like reassigning the environment of the function to
the env which it finishes its execution in, but this causes other problems:

```lox
fun counters() {
  var n = 0;
  fun count1() {
    n = n + 1;
    return n;
  }
  fun count2() {
    n = n + 2
    return n;
  }
  return (count1, count2); // assuming we add tuples
}

var counts = counters();
print(counts.1());
print(counts.2());
```

with the solution i outlined above, the `n` in `count1` would be different than that
of `count2` (assuming a sensible implementation of tuples), which doesn't
really  make sense.

my current implementation disallows both of these program statically by checking ahead of
time that variables used in functions are declared in the current function,
which is equivalent to disallowing closures altogether. obviously, this is an
unsatisfactory implementation.

we can emulate the solution of the book by introducing `IORef`s into the
environment model and doing a variable resolution pass of the syntax tree.
i got pretty far along implementing this before deciding i didn't like it
and burying my head in the sand. if anything, the exercise of implementing
it has made me question why we would ever want a language feature like this?
reasoning about the execution of a program becomes so much harder when a call
to a function can have such nonlocal implications. it makes me appreciate
Rust's static ban on such behaviour while preserving a reasonable imperative model.

### slowness

at the beginning of the second half of the book, the author mentions that the
following program runs in jlox on his machine in 72 seconds.

```lox
fun fib(n) {
  if (n < 2) { return n;}
  return fib(n - 1) + fib(n - 2); 
}

var before = clock();
print(fib(40));
var after = clock();
print(after - before);
```

on my fairly well-equipped work laptop, this runs in 25 and a half minutes! i
haven't done any profiling but i suspect some of this extra overhead is due to
the monad stack that we execute much of the code in, and also in variable
lookups, which take exponentially more time as we recurse.

## what next?

i'm gonna skip ahead to the part of the book where the author reimplements the
language with bytecode, which will presumably require much more static analysis,
and see if that can be done in pure land. i will report back here along the way.
