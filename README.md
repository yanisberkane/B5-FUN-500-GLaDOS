# GLaDOS - A Haskell-Based Programming Language

## Overview
GLaDOS (Generic Language and Data Operand Syntax) is a custom programming language implemented in Haskell. The project's aim is to create a minimalist yet functional programming environment, inspired by languages like LISP and Scheme, and is designed with a focus on functional programming principles.

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

### Installationa

1. Navigate to the project directory:

cd glados

arduino

2. Build the project using Stack:

stack build

bash


### Running the Language
To run a program written in GLaDOS, use the following command:

./glados < your_program.glados

shell


## Usage
Describe how to write programs in your language, including syntax and basic constructs. Provide examples for better understanding.

### Examples

(define foo 21)
(* foo 2)

shell

Output: `42`