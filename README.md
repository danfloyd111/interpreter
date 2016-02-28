# interpreter
Interpreter for an istructional functional language

This is an interpreter I wrote for a programming course at university, the language's concrete syntax is:

**Ide [Ide] :**      strings representing names
**Integers [Int]:**  Int
**Values [Val]:**    Int | True | False | fun ide -> e (single parameter, non recursive functions)
**Expressions [E]:** Ide | Val | E and E | E or E | not E | OP(E,E) (simple operations between integers) | if E then E else E |
                 Let Ide = E in E | E(E) (function application) | try Ide with E in P
**Patterns [P]:**    _ -> E | E -> E | (E -> E)::P (pattern composition)

Here is an example of a pattern:
**try** x **with** +(z,w) **in**
  (x > 0) **->** foo(x) **::**
  (x < 0) **->** bar(x) **::**
  _ **->** x The value of the expression (z+w) is binded to ide "x", if one of the "left expressions" is satisfied then is
  returned the correspondent "right expression", otherwise (if none of left expr is satisfied) is returned the last right one
  (also called "default option") associated with the symbol "_".
  
I also wrote a simple program to test this project (included in this repo with an assert module).
