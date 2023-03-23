(ns mrm.evaluator
  (:require
   [mrm.object :as object]
   [mrm.parser :as parser]
   [mrm.lexer :as lex]))

(defn ast-type [node]
  (:ast-type node))

(defmulti evaluate ast-type)
(defmulti eval-prefix-expression (fn [operator _] operator))

(def trueObj (object/create-boolean-object true))
(def falseObj (object/create-boolean-object false))
(def nullObj (object/create-null-object))

(defn native-bool-to-bool-object [value]
  (if value
    trueObj
    falseObj))

(defn eval-integer-infix-expression
  [left right operator]
  (cond
    (= operator "+") (object/create-integer-object (+ (:value left) (:value right)))
    (= operator "-") (object/create-integer-object (- (:value left) (:value right)))
    (= operator "*") (object/create-integer-object (* (:value left) (:value right)))
    (= operator "/") (object/create-integer-object (/ (:value left) (:value right)))
    (= operator "<") (native-bool-to-bool-object
                      (< (:value left) (:value right)))
    (= operator ">")
    (native-bool-to-bool-object
     (> (:value left) (:value right)))
    (= operator "==")
    (native-bool-to-bool-object
     (= (:value left) (:value right)))
    (= operator "!=")
    (native-bool-to-bool-object
     (not (= (:value left) (:value right))))
    :else nullObj))

(defn is-truthy [evaluated-expr]
  (cond
    (= nullObj evaluated-expr) false
    (= trueObj evaluated-expr) true
    (= falseObj evaluated-expr) false
    :else true))

(defmethod evaluate :expression-statement
  [node]
  (println "Expression statemnet" node)
  (evaluate (:expression node)))

(defmethod evaluate :block-statement
  [node]
  (let [last-val (evaluate (last (:statements node)))]
    last-val))

(defmethod evaluate :if-expression
  [node]
  (println "IF expression")
  (let [condition (evaluate (:condition node))
        condition-met (is-truthy condition)
        _ (println "CONDITION " condition)
        _ (println "condition met " condition-met)]
    (cond
      condition-met (do (println "CONDITION met evaluate -> " (:consequence node)) (evaluate (:consequence node)))
      (and (not condition-met) (not (nil? (:alternative node))))
      (evaluate (:alternative node))
      :else
      nullObj)))

(defmethod evaluate :boolean-literal
  [node]
  (if (= "true" (:value node))
    trueObj
    falseObj))

(defmethod evaluate :return-statement
  [node]
  (let [return-expression (evaluate (:return-expression node))]
    (throw (ex-info "return encountered"
                    {:type :return-statement
                     :cause :encountered
                     :expression return-expression}))))

(defmethod evaluate :infix-expression
  [node]
  (let [left (evaluate (:left node))
        right (evaluate (:right node))
        operator (:operator node)]
    (cond
      (and (= :integer (:type left)) (= :integer (:type right)))
      (eval-integer-infix-expression left right operator)
      (= operator "==") (native-bool-to-bool-object (= left right))
      (= operator "!=") (native-bool-to-bool-object (not (= left right)))
      :else nullObj)))

(defmethod evaluate :integer-literal
  [node]
  (object/create-integer-object (Long/parseLong (apply str (:value node)))))

(defmethod evaluate :default
  [node]
  (println "eval default" node))

(defmethod evaluate :prefix-expression
  [node]
  (let [right (evaluate (:right node))
        operator (:operator node)]
    (eval-prefix-expression operator right)))

(defmethod eval-prefix-expression "!"
  [_ right]
  (cond
    (= trueObj right) falseObj
    (= falseObj right) trueObj
    (= nullObj right) trueObj
    :else falseObj))

(defmethod eval-prefix-expression "-"
  [_ right]
  (if (not (= :integer (:type right)))
    nullObj
    (let [value (:value right)]
      (object/create-integer-object (- value)))))

(defn eval-statements [nodes]
  (println nodes)
  (try
    (reduce (fn [_ new]
              (println "NEWS-----" new)
              (evaluate new))
            nil
            nodes)
    (catch clojure.lang.ExceptionInfo e
      (if (=  :return-statement (-> e ex-data :type))
        (-> e ex-data :expression)))))

(defn eval-program [nodes]
  (eval-statements nodes))

; ((:inspect (eval-program (parser/parse-program (lex/new-lexer "if (5 * 5 + 10 > 34) {99;11;11;} else { 100 };")))))

(comment
  (ns-unmap *ns* 'evaulate)
  (ns-unmap *ns* 'ast-type))
