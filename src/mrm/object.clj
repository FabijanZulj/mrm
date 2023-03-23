(ns mrm.object)

(defn create-integer-object [value]
  {:type :integer
   :inspect (fn [] value)
   :value value})

(defn create-boolean-object [value]
  {:type :boolean
   :inspect (fn [] value)
   :value value})

(defn create-null-object []
  {:type :null
   :inspect (fn [] "null")})
