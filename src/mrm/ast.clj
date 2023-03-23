(ns mrm.ast
  (:require
   [mrm.lexer :as lex]))

(defn make-let-statement-ast [name expression lexer-state]
  {:ast-type :let-statement
   :name name
   :expression expression
   :lexer-state lexer-state})

(defn make-integer-literal-ast [value lexer-state]
  {:ast-type :integer-literal
   :value value
   :lexer-state lexer-state})

(defn make-return-statement-ast [return-expression lexer-state]
  {:ast-type :return-statement
   :return-expression return-expression
   :lexer-state lexer-state})

(defn make-expression-statement-ast [expression lexer-state]
  {:ast-type :expression-statement
   :expression expression
   :lexer-state lexer-state})

(defn make-identifier-ast [value lexer-state]
  {:ast-type :identifier
   :value value
   :lexer-state lexer-state})

(defn make-prefix-expression-ast [operator right lexer-state]
  {:ast-type :prefix-expression
   :operator operator
   :right right
   :lexer-state lexer-state})

(defn make-infix-expression-ast [left right operator lexer-state]
  {:ast-type :infix-expression
   :left left
   :operator operator
   :right right
   :lexer-state lexer-state})

(defn make-boolean-literal-ast [value lexer-state]
  {:ast-type :boolean-literal
   :value value
   :lexer-state lexer-state})

(defn make-if-expression-ast [condition consequence alternative lexer-state]
  {:ast-type :if-expression
   :condition condition
   :consequence consequence
   :alternative alternative
   :lexer-state lexer-state})

(defn make-block-statement-ast [statements lexer-state]
  {:ast-type :block-statement
   :statements statements
   :lexer-state lexer-state})

(defn make-function-literal-ast [parameters block-statement lexer-state]
  {:ast-type :function-literal
   :parameters parameters
   :block-statement block-statement
   :lexer-state lexer-state})

(defn make-call-expression-ast [function arguments lexer-state]
  {:ast-type :call-expression
   :function function
   :arguments arguments
   :lexer-state lexer-state})
