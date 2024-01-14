# GLaDOS - A Haskell-Based Programming Language

![1705261792661](image/README/1705261792661.png)![1705262268662](image/README/1705262268662.png)

## Overview

GLaDOS (Generic Language and Data Operand Syntax) is a custom programming language implemented in Haskell. The project's aim is to create a minimalist yet functional programming environment, inspired by languages like LISP, C and Scheme, and is designed with a focus on functional programming principles.

Our language is called CCS (Confused Code Syntax) and it produces DZ (Dazed) compiled files that you can execute using our VM.

## Features

- **Custom Language Design**: Implementing a language from scratch, including parsing, syntax, and semantics.
- **Build System**: Compilation via Makefile with `re`, `clean`, and `fclean` rules. Stack is recommended for building.
- **Testing**: Comprehensive unit and integration tests, with continuous integration and delivery setups.
- **Error Handling**: Robust error handling with meaningful output.
- **Functional Paradigms**: Emphasis on functional programming concepts like lambda expressions, closures, and immutable data.

## Getting Started

### Prerequisites

- Haskell
- Stack (recommended build tool)

### Installation

For Linux distributions, you need to run `./setup-haskell-stack.sh` in order to install Haskell and Stack.

Then, run `make build` to build GLaDOS binary.

### Running the Language

To run a program written in CCS (Confused Code Syntax), use the following command to compile it:

```
./glados yourFile.ccs 
```

A compiled ".dz" (Dazed) file will be generated, now run the following command to execute your code:

```
./glados-vm yourCompiledFile.dz
```

### Examples

If your ccs file contains the following code below:

```
let a = 1;
let b = 2;

if (a == b) then:
(
  print("a is equal to b");
)
print("a isn’t equal to b");
```

The output should be:

```
a isn't equal to b
```

### Another example with a factorial function is CCS:

```
fact (a) => (
  if (a == 1) then:
      return (1);
  else:
      return (fact(a - 1) * a);
);

let result = (fact(5));
print(result);
```

The output should be:

```
120
```

## Authors

- Artyom TILLON (artyom.tillon@epitech.eu).
- Aurélien LECLERCQ (aurelien.leclercq@epitech.eu).
- Edgar DION (edgar.dion@epitech.eu).
- Marius LACOMBLEZ (marius.lacomblez@epitech.eu).
- Yanis BERKANE (yanis.berkane@epitech.eu).
