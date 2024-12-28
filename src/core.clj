(ns core
  (:require
   [clojure.java.io :as io])
  (:import
   [java.io File]
   [java.nio ByteBuffer ByteOrder]))

(defn ->shorts
  [bytes]
  (let [numbers (quot (alength bytes) 2)
        buffer (.. (ByteBuffer/wrap bytes)
                   (order ByteOrder/LITTLE_ENDIAN))]
    (map #(.getShort buffer (* % 2)) (range numbers))))

(defn read-binary-file
  [file-path]
  (let [file (File. file-path)]
    (with-open [stream (io/input-stream file)]
      (let [buffer (byte-array (.length file))]
        (.read stream buffer)
        (->shorts buffer)))))

(def program (vec (read-binary-file "resources/challenge.bin")))

(defn halt
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn set
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn push
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn pop
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn eq
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn gt
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn jmp
  [{:keys [pointer] :as state}]
  (let [a (get program (inc pointer))]
    (assoc state :pointer (int a))))

(defn jt
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn jf
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn add
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn mult
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn mod
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn and
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn or
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn not
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn rmem
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn wmem
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn call
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn ret
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn out
  [{:keys [pointer] :as state}]
  (let [a (get program (inc pointer))]
    (print (char a))
    (assoc state :pointer (+ pointer 2))))

(defn in
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defn noop
  [state]
  (update state :pointer inc))

(def get-instruction
  {6 jmp
   7 jt
   19 out
   21 noop})

(defn initial-state
  []
  {:pointer 0})

(defn process-instruction
  [{:keys [pointer] :as state}]
  ((get-instruction (get program pointer)) state))

(defn run
  [max initial-state]
  (loop [idx 0
         state initial-state]
    (when (< idx max)
      (recur (inc idx) (process-instruction state)))))

(defn -main
  []
  (try
    (run 200 (initial-state))
    (catch Exception e
      (let [{:keys [pointer] :as state} (ex-data e)]\
        (println (ex-message e))
        (println (pr-str {:opcode (get program pointer)
                          :range (subvec program (dec pointer) (+ pointer 5))
                          :state state}))))))

(-main)
