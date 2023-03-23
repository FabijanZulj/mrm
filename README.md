# Monkey Language Interpreter in Clojure
## Introduction
Monkey is a small programming language designed for simplicity and ease of learning. This project is an interpreter for Monkey, written in the functional programming language, Clojure.

This implementation uses `multimethods` for parse functions and evaluator functions. This approach might not be the best one for some cases but it is used in this implementations for education on multimethods.

## Features
-   Basic data types: integers, booleans, strings
-   Arithmetic, comparison, and logical operators
-   Variables and assignment
-   First-class functions and closures
-   Control structures: `if`, `else`
-   Error handling and reporting 🚧 🚧
-   REPL (Read-Eval-Print Loop) for interactive coding 🚧 🚧
## Example of Monkey snippet

    let factorial = fn(n) {
	    if (n == 0) {
		    1 
	    } else { 
		    n * factorial(n - 1);
	    } 
	 };
	  
	 let result = factorial(5);

