# Typed Lisp

Typed lisp that compiles to LLVM.

It supports higher order functions:

```lisp
(defun add [x y] (+ x y))

(defun app [f z] (f z 1))
(app add 10)
```

### Built-in

- "+" function to add integers
- Integers
- Function declaration
- Function application
- Variable declaration

### Type-System

Hindley-Milner-Damas? Or at least a try of it!

### Next

- Partial application
- Create more tests 

