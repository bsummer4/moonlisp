# Moonlisp

This is a purely functional lisp implementation that uses tables and strings
instead of lists and symbols. It is dynamically typed, purely functional,
and has first-class (monad) procedures as well as first-class functions.

This is basically an experiment to play with the following ideas:

  - Lisp meta-programming with JSON-like syntax.
  - Purely functional programming in Lisp (with monadic IO).
  - Terse syntax based on unicode characters.


## Data Types

Moonlisp has the following types: null bool func str num table ref foreign

    data T = Null
           | True
           | False
           | Func    (T → T)
           | Proc    (IO T)
           | Ref     (IORef T)
           | Foreign (ForeignPtr a)
           | Table   (HashMap T T)


## Base Syntax

The basic syntacitic form is an evaluated table. For example

    (x, 1, 2:null)

This is an expression to be evaluated. The quoted version of this is
equivalent to the following JSON (except with numeric keys):

    {0:"x", 1:"1", 2:null}

## Special Forms and Evaluation Rules

    (QUOTE expr)      | Fully quotes an expression.
    (UNQUOTE expr)    | Unquotes an element of a quoted expression.
    (LAMBDA var expr) | Lambda abstraction.
    (x y)             | Applies f to y
    (x y z)           | Applies “applies x to y” to z
    ((LAMBDA x x) 3)  | Applies (LAMBDA x x) to 3


## Basic Syntactic Sugar

    /#.*$/ | A comment

    |x|     → A `|` delimited bare-word.
    "x"     → (QUOTE |x|)

    $x          → (UNQUOTE x)
    `x          → (QUOTE x)
    {x y z k:v} → `($x $y $z k:$v)
    [x y z k:v] → {x y z k:v}
    <x y z>     → (x `y `z)


## Built in macros

These are just normal functions that take quoted expressions as arguments.

    <λ x x>                            → (LAMBDA x x)
    <λ [x y] EXP>                      → <λ x <λ y EXP>>

    <case expr (pat body) ...>         → ((λ pat body) expr)
                                       → TODO This is me being lazy.

    <let (pat expr) body>              → <case expr (pat body)>
    <let [(a b) (c d)] (+ a c)>        → <let (a b) <let (c d) (+ a c)>>
    <let [a b c d] x>                  → <let [(a b) (c d)]>

    <do body (pat exp) ...>            → <case exp (pat body)>

    <proc <v ← proc1> <e : exp> proc2> → (bind proc1
                                           (λ v
                                             (let (e exp)
                                               proc2)))

    <proc p>                           → p
    <proc <x ← p> x>                   → (join p)
    <proc <x ← p> (return x)>          → p
    ...


## Built in functions

    .        ∷ table → t → t
    insert   ∷ table → t → t → t
    keys     ∷ table → [t]
    $        ∷ (t→t) → t → t
    &        ∷ t → (t→t) → t
    len      ∷ [t] → num
    size     ∷ {t:t} → num
    cons     ∷ t → [t] → [t]
    snoc     ∷ [t] → t → [t]
    concat   ∷ [t] → [t] → [t]
    not      ∷ bool → bool
    ~        ∷ num → num
    +        ∷ num → num
    -        ∷ num → num
    * ·      ∷ num → num
    / ÷      ∷ num → num
    % mod    ∷ num → num
    ^ pow    ∷ num → num
    = eq     ∷ num → num
    ≠ neq    ∷ num → num
    < lt     ∷ num → num
    ≤ lte    ∷ num → num
    > gt     ∷ num → num
    ≥ gte    ∷ num → num
    ..       ∷ str → str
    nil?     ∷ {t} → bool
    bool?    ∷ {t} → bool
    func?    ∷ {t} → bool
    str?     ∷ {t} → bool
    num?     ∷ {t} → bool
    foreign? ∷ {t} → bool
    table?   ∷ {t} → bool
    seq >>   ∷ proc → proc → proc
    bind >>= ∷ proc → (t → proc) → proc
