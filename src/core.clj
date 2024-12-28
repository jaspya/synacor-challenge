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

(defn jmp
  [{:keys [pointer] :as state}]
  (let [a (get program (inc pointer))]
    (assoc state :pointer (int a))))

(defn out
  [{:keys [pointer] :as state}]
  (let [a (get program (inc pointer))]
    (print (char a))
    (assoc state :pointer (+ pointer 2))))

(defn noop
  [state]
  (update state :pointer inc))

(defn log-instruction
  [{:keys [pointer] :as state}]
  (println (pr-str (-> state
                       (assoc :opcode (get program pointer))
                       (assoc :range (subvec program (dec pointer) (+ pointer 3))))))
  (throw (ex-info "unknown instruction" state)))

(def get-instruction
  {6 jmp
   19 out
   21 noop})

(defn initial-state
  []
  {:pointer 0})

(defn process-instruction
  [{:keys [pointer] :as state}]
  ((get-instruction (get program pointer) log-instruction) state))

(defn run
  [max initial-state]
  (loop [idx 0
         state initial-state]
    (when (< idx max)
      (recur (inc idx) (process-instruction state)))))

(run 200 (initial-state))
  