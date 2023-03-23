(ns mrm.parser
  (:require
   [mrm.ast :as ast]
   [mrm.lexer :as lex]))

(def precedences
  {:lowest 0
   :equals 1
   :lessgreater 2
   :sum 3
   :product 4
   :prefix 5
   :call 6})

(def token-type-to-precedence-map
  {:equals :equals
   :not-eq :equals
   :lt :lessgreater
   :gt :lessgreater
   :plus :sum
   :minus :sum
   :slash :product
   :asterisk :product})

(defn expect
  [required-token-type lexer-state]
  (cond
    (= required-token-type (:token-type (:lexed-item lexer-state)))
    lexer-state
    :else nil))

(defn advance-if-type
  [required-type lexer-state]
  (if (= required-type (:token-type (:lexed-item lexer-state)))
    (lex/next-token lexer-state)
    lexer-state))

(defn peek-precedence
  [lexer-state]
  (let [next-token-type (:token-type (:lexed-item (lex/next-token lexer-state)))
        next-precedence (get token-type-to-precedence-map next-token-type)]
    (cond
      next-precedence next-precedence
      :else :lowest)))

(defn get-precedence
  [lexer-state]
  (let [curr-token-type (:token-type (:lexed-item lexer-state))
        curr-precedence (get token-type-to-precedence-map curr-token-type)]
    (cond
      curr-precedence curr-precedence
      :else :lowest)))

(defn token-type-dispatch
  [lexer-state]
  (:token-type (:lexed-item lexer-state)))

(defn infix-token-type-dispatch
  [_ lexer-state]
  (:token-type (:lexed-item lexer-state)))

(defmulti prefixParseFn token-type-dispatch)
(defmulti infixParseFn infix-token-type-dispatch)
(defmulti parseStatement token-type-dispatch)

(defn parse-expression
  [precedence lexer-state]
  (let [prefixFn (get-method prefixParseFn (token-type-dispatch lexer-state))
        _ (println "parse expression" lexer-state)
        left-exp (prefixFn lexer-state)
        _ (println "left expr" left-exp)]

    (loop [left-exp left-exp
           lexer-state (:lexer-state left-exp)]
      ;; (println "left-exp" left-exp)
      ;; (println "lexer-state" lexer-state)
      (cond
        (or
         (nil? (get-method infixParseFn (infix-token-type-dispatch nil lexer-state)))
         (= :eof (:token-type (:lexed-item lexer-state)))
         (> (precedence precedences) ((peek-precedence lexer-state) precedences))
         (= :semicolon (:token-type (:lexed-item lexer-state))))
        left-exp

        :else
        (let [left (infixParseFn left-exp lexer-state)
              next-lex-state (:lexer-state left)]
          (recur left next-lex-state))))))

(defn parse-block-statement
  [lexer-state]
  (loop [lexer-state lexer-state
         statements []]
    (cond
      (or
       (= :right-brace (token-type-dispatch lexer-state))
       (= :eof (token-type-dispatch lexer-state)))
      (ast/make-block-statement-ast statements (advance-if-type :right-brace lexer-state))
      (or
       = :left-brace (token-type-dispatch lexer-state)
       = :semicolon (token-type-dispatch lexer-state))
      (recur (lex/next-token lexer-state) statements)
      :else
      (let [parsed-statement (parseStatement lexer-state)]
        (recur (:lexer-state parsed-statement)
               (conj statements parsed-statement))))))

(defn parse-prefix-expression
  [lexer-state]
  (let [operator (lex/get-literal-from-lexer lexer-state)
        right (parse-expression :prefix (lex/next-token lexer-state))]
    (ast/make-prefix-expression-ast operator right (:lexer-state right))))

(defn parse-infix-expression
  [left lexer-state]
  (let [precedence (get-precedence lexer-state)
        operator (lex/get-literal-from-lexer lexer-state)
        right (parse-expression precedence (lex/next-token lexer-state))]
    (ast/make-infix-expression-ast left right operator (:lexer-state right))))

(defmethod prefixParseFn :identifier
  [lexer]
  (ast/make-identifier-ast (:literal (:lexed-item lexer)) (advance-if-type :semicolon (lex/next-token lexer))))

(defmethod prefixParseFn :if
  [lexer]
  (let [condition (some->> (lex/next-token lexer)
                           (advance-if-type :left-paren)
                           (parse-expression :lowest))
        consequence (some->> (:lexer-state condition)
                             (advance-if-type :right-paren)
                             (parse-block-statement))
        alternative (some->> (:lexer-state consequence)
                             (expect :else)
                             (advance-if-type :else)
                             (parse-block-statement))]
    (ast/make-if-expression-ast condition consequence alternative (if alternative (:lexer-state alternative) (:lexer-state consequence)))))

(defmethod prefixParseFn :integer
  [lexer-state]
  (ast/make-integer-literal-ast (lex/get-literal-from-lexer lexer-state)
                                (if (= :semicolon (token-type-dispatch (lex/next-token lexer-state)))
                                  (lex/next-token (lex/next-token lexer-state))
                                  (lex/next-token lexer-state))))

(defmethod prefixParseFn :left-paren
  [lexer-state]
  (let [expr (parse-expression :lowest (lex/next-token lexer-state))
        is-rparen (expect :right-paren (:lexer-state expr))]
    (if is-rparen
      (assoc expr :lexer-state (lex/next-token is-rparen))
      nil)))

(defmethod prefixParseFn :bang
  [lexer-state]
  (parse-prefix-expression lexer-state))

(defmethod prefixParseFn :minus
  [lexer-state]
  (parse-prefix-expression lexer-state))

(defmethod prefixParseFn :true
  [lexer-state]
  (ast/make-boolean-literal-ast (lex/get-literal-from-lexer lexer-state)
                                (advance-if-type :semicolon (lex/next-token lexer-state))))

(defn parse-call-arguments [lexer-state]
  (loop [lexer-state lexer-state arguments []]
    (cond
      (= :right-paren (token-type-dispatch lexer-state))
      {:arguments arguments
       :lexer-state (lex/next-token lexer-state)}
      (= :comma (token-type-dispatch lexer-state))
      (let [expression-token (lex/next-token lexer-state)
            argument (parse-expression :lowest expression-token)]
        (recur (:lexer-state argument) (conj arguments argument)))
      :else
      (let [argument (parse-expression :lowest (lex/next-token lexer-state))]
        (recur (:lexer-state argument) (conj arguments argument))))))

(defmethod prefixParseFn :false
  [lexer-state]
  (ast/make-boolean-literal-ast (lex/get-literal-from-lexer lexer-state)
                                (advance-if-type :semicolon (lex/next-token lexer-state))))

(defn parse-function-parameters [lexer-state]
  (loop [lexer-state lexer-state parameters []]
    (cond
      (= :right-paren (token-type-dispatch lexer-state)) {:parameters parameters
                                                          :lexer-state (lex/next-token lexer-state)}
      (= :comma (token-type-dispatch lexer-state))
      (let [next-identifier (lex/next-token lexer-state)
            identifier (ast/make-identifier-ast (lex/get-literal-from-lexer next-identifier) next-identifier)]
        (recur (:lexer-state identifier) (conj parameters identifier)))
      :else
      (let [identifier (ast/make-identifier-ast (lex/get-literal-from-lexer lexer-state) lexer-state)]
        (recur (lex/next-token (:lexer-state identifier)) (conj parameters identifier))))))

(defn parse-call-expression [function lexer-state]
  (if (= :right-paren (token-type-dispatch (lex/next-token lexer-state)))
    (ast/make-call-expression-ast function [] (lex/next-token (lex/next-token lexer-state)))
    (let [arguments-with-state (parse-call-arguments lexer-state)]
      (ast/make-call-expression-ast function (:arguments arguments-with-state) (:lexer-state arguments-with-state)))))

(defmethod prefixParseFn :fn
  [lexer-state]
  (let [parameters-with-state (some->> (lex/next-token lexer-state)
                                       (advance-if-type :left-paren)
                                       (parse-function-parameters))
        body (some->> (:lexer-state parameters-with-state)
                      (parse-block-statement))]

    (ast/make-function-literal-ast (:parameters parameters-with-state) body (:lexer-state body))))

(defmethod infixParseFn :plus
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :minus
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :slash
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :asterisk
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :equals
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :not-eq
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :left-paren
  [function state]
  (parse-call-expression function state))

(defmethod infixParseFn :lt
  [left state]
  (parse-infix-expression left state))

(defmethod infixParseFn :gt
  [left state]
  (parse-infix-expression left state))

(defmethod parseStatement :let
  [lexer-state]
  (let [identifier (prefixParseFn (lex/next-token lexer-state))
        expected-assign (expect :assign (:lexer-state identifier))]
    (if expected-assign
      (let [expression (parse-expression (:lowest precedences) (lex/next-token expected-assign))]
        (ast/make-let-statement-ast identifier
                                    expression
                                    (advance-if-type :semicolon (:lexer-state expression))))
      nil)))

(defmethod parseStatement :return
  [lexer-state]
  (let [skip-return-token (lex/next-token lexer-state)
        return-expression (parse-expression :lowest skip-return-token)]
    (ast/make-return-statement-ast return-expression
                                   (advance-if-type :semicolon (:lexer-state return-expression)))))

(defmethod parseStatement :default
  [lexer-state]
  (let [expression (parse-expression :lowest lexer-state)]
    (ast/make-expression-statement-ast expression
                                       (advance-if-type :semicolon (:lexer-state expression)))))

(defn parse-program
  [initial-lexer]
  (loop [lexer-state (lex/next-token initial-lexer)
         ast []]
    (if (not (= :eof (:token-type (:lexed-item lexer-state))))
      (let [parsed-statement (parseStatement lexer-state)]
        (recur (:lexer-state parsed-statement) (conj ast parsed-statement)))
      ast)))

;; --------REPL
(comment
  (parse-program (lex/new-lexer "if (5 * 5 + 10 > 34) {99;55;} else { 100 };return 16;5;11;"))
  (ns-unmap *ns* 'parseStatement)
  (ns-unmap *ns* 'statement-parse-dispatch-fn)
  (ns-unmap *ns* 'prefixParseFn)
  (ns-unmap *ns* 'infixParseFn))
