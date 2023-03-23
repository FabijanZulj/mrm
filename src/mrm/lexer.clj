(ns mrm.lexer)

(def type-to-length-literal-mapping
  {:equals {:token-length 2 :literal "=="}
   :assign {:token-length 2 :literal "="}
   :left-paren {:token-length 1 :literal "("}
   :right-paren {:token-length 1 :literal ")"}
   :left-brace {:token-length 1 :literal "{"}
   :right-brace {:token-length 1 :literal "}"}
   :plus {:token-length 1 :literal "+"}
   :true {:token-length 4 :literal "true"}
   :minus {:token-length 1 :literal "-"}
   :eof {:token-length 0 :literal ""}
   :bang {:token-length 1 :literal "!"}
   :semicolon {:token-length 1 :literal ";"}
   :asterisk {:token-length 1 :literal "*"}
   :slash {:token-length 1 :literal "/"}
   :lt {:token-length 1 :literal "<"}
   :gt {:token-length 1 :literal ">"}
   :not-eq {:token-length 2 :literal "!="}
   :fn {:token-length 2 :literal "fn"}
   :comma {:token-length 1 :literal ","}
   :let {:token-length 3 :literal "let"}
   :false {:token-length 5 :literal "false"}
   :if {:token-length 2 :literal "if"}
   :else {:token-length 4 :literal "else"}
   :return {:token-length 6 :literal "return"}
   :illegal {:token-length 0 :literal ""}})

(defn make-token [token-type location]
  (merge
   {:location location
    :token-type token-type}
   (get type-to-length-literal-mapping token-type)))

(def keywords {"let" (partial make-token :let)
               "fn" (partial make-token :fn)
               "true" (partial make-token :true)
               "false" (partial make-token :false)
               "if" (partial make-token :if)
               "else" (partial make-token :else)
               "return" (partial make-token :return)})

(defn count-whitespace [string-seq]
  (count (take-while #(if (or (= %1 " ") (= %1 \space)) true false) string-seq)))

(count-whitespace '(\space \space \space 1 2 3 4))

(defn create-identifier [literal length position]
  (if-let
   [token-without-position (get keywords (apply str literal))]
    (token-without-position position)
    {:token-type :identifier
     :literal (apply str literal)
     :token-length length
     :position position}))

(defn create-numeric [literal length position]
  (if (first (filter #(= % \.) literal))
    {:token-type :decimal
     :literal literal
     :token-length length
     :position position}
    {:token-type :integer
     :literal literal
     :token-length length
     :position position}))

(defn get-literal-from-lexer [lexer]
  (:literal (:lexed-item lexer)))

(defn make-new-lexer-state [input curr-index lexed-item prev-tokens]
  (let [token-length (:token-length lexed-item)]
    {:input input
     :curr-index (+ curr-index token-length (count-whitespace
                                             (nthrest input (+ curr-index token-length))))
     :lexed-item lexed-item
     ; :all-prev-tokens (conj prev-tokens lexed-item)
     }))

(defn new-lexer [input]
  {:curr-index 0
   :input (vec input)
   :lexed-item nil})

;TODO remove all prev tokens

(defn tokenize-single [lexer]
  (let [input (:input lexer)
        curr-index (:curr-index lexer)
        curr-char (get input curr-index)
        all-prev-tokens (:all-prev-tokens lexer)
        rest-input (nthrest input curr-index)
        next-char (nth input (+ curr-index (count-whitespace rest-input) 1) nil)]
    (cond
      (= curr-char nil)   (make-new-lexer-state input curr-index (make-token :eof curr-index) all-prev-tokens)
      (= curr-char \;)    (make-new-lexer-state input curr-index (make-token :semicolon curr-index) all-prev-tokens)
      (and
       (= curr-char \=)
       (= next-char \=)) (make-new-lexer-state input curr-index (make-token :equals curr-index) all-prev-tokens)
      (and
       (= curr-char \!)
       (= next-char \=)) (make-new-lexer-state input curr-index (make-token :not-eq curr-index) all-prev-tokens)

      (= curr-char \=)    (make-new-lexer-state input curr-index (make-token :assign curr-index) all-prev-tokens)
      (= curr-char \()    (make-new-lexer-state input curr-index (make-token :left-paren curr-index) all-prev-tokens)
      (= curr-char \))    (make-new-lexer-state input curr-index (make-token :right-paren curr-index)  all-prev-tokens)
      (= curr-char \{)    (make-new-lexer-state input curr-index (make-token :left-brace curr-index) all-prev-tokens)
      (= curr-char \})    (make-new-lexer-state input curr-index (make-token :right-brace curr-index) all-prev-tokens)
      (= curr-char \-)    (make-new-lexer-state input curr-index (make-token :minus curr-index) all-prev-tokens)
      (= curr-char \+)    (make-new-lexer-state input curr-index (make-token :plus curr-index) all-prev-tokens)
      (= curr-char \!)    (make-new-lexer-state input curr-index (make-token :bang curr-index) all-prev-tokens)
      (= curr-char \,)    (make-new-lexer-state input curr-index (make-token :comma curr-index) all-prev-tokens)
      (= curr-char \>)    (make-new-lexer-state input curr-index (make-token :gt curr-index) all-prev-tokens)
      (= curr-char \<)    (make-new-lexer-state input curr-index (make-token :lt curr-index) all-prev-tokens)
      (= curr-char \/)    (make-new-lexer-state input curr-index (make-token :slash curr-index) all-prev-tokens)
      (= curr-char \*)    (make-new-lexer-state input curr-index (make-token :asterisk curr-index) all-prev-tokens)
      (Character/isLetter curr-char)
      (let [ident (take-while #(Character/isLetter %1) rest-input)]
        (make-new-lexer-state input curr-index (create-identifier ident (count ident) curr-index) all-prev-tokens))

      (Character/isDigit curr-char)
      (let [digit (take-while #(or (= %1 \.) (Character/isDigit %1)) rest-input)]
        (make-new-lexer-state input curr-index (create-numeric digit (count digit) curr-index) all-prev-tokens))
      :else
      {:token-type :illegal
       :literal ""
       :token-length 0})))

(defn next-token [lexer]
  (tokenize-single lexer))

(next-token (new-lexer "true"))
