(ns mrm.mrm
  "mrm")

(defn exec
  "Invoke me with clojure -X mrm.mrm/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m mrm.mrm"
  [& args]
  (println "-main with" args))

(-main)
