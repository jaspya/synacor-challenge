#!/usr/bin/env bb

(ns run
  (:require [clojure.java.io :as io])
  (:import [java.io File]
           [java.nio ByteBuffer ByteOrder]))

(defn ->shorts
  [bytes]
  (let [numbers (quot (alength bytes) 2)
        buffer (.. (ByteBuffer/wrap bytes)
                   (order ByteOrder/LITTLE_ENDIAN))]
    (map #(bit-and (.getShort buffer (* % 2)) 0xFFFF) (range numbers))))

(defn read-binary-file
  [file-path]
  (let [file (File. file-path)]
    (with-open [stream (io/input-stream file)]
      (let [buffer (byte-array (.length file))]
        (.read stream buffer)
        (->shorts buffer)))))

(def program (vec (read-binary-file "challenge.bin")))

(def max-literal 32768)

(defn add-arguments
  [state n]
  (reduce (fn [{:keys [registers pointer] :as state} _idx]
            (let [raw-value (get program pointer)
                  register? (>= raw-value max-literal)
                  reg (when register? (- raw-value max-literal))
                  argv (if register? (get registers reg) raw-value)]
              (-> state
                  (update :reg conj reg)
                  (update :argv conj argv)
                  (update :pointer inc))))
          (-> state
              (assoc :reg [])
              (assoc :argv [])
              (update :pointer inc))
          (range n)))

(defmulti instruction
  (fn [{:keys [pointer]}] (get program pointer)))

(defmethod instruction 0
  [_state]
  (flush)
  (System/exit 0))

(defmethod instruction 1
  [state]
  (let [{[a] :reg [_ b] :argv :as state} (add-arguments state 2)]
    (assoc-in state [:registers a] b)))

(defmethod instruction 2
  [state]
  (let [{[a] :argv :as state} (add-arguments state 1)]
    (update state :stack conj a)))

(defmethod instruction 3
  [state]
  (let [{[a] :reg stack :stack :as state} (add-arguments state 1)]
    (-> state
        (assoc-in [:registers a] (peek stack))
        (update :stack pop))))

(defmethod instruction 4
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (if (= b c) 1 0))))

(defmethod instruction 5
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 6
  [state]
  (let [{[a] :argv :as state} (add-arguments state 1)]
    (assoc state :pointer a)))

(defmethod instruction 7
  [state]
  (let [{[a b] :argv :as state} (add-arguments state 2)]
    (cond-> state
      (not (zero? a)) (assoc :pointer b))))

(defmethod instruction 8
  [state]
  (let [{[a b] :argv :as state} (add-arguments state 2)]
    (cond-> state
      (zero? a) (assoc :pointer b))))

(defmethod instruction 9
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (mod (+ b c) 32768))))

(defmethod instruction 10
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 11
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 12
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 13
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 14
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 15
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 16
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 17
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 18
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 19
  [state]
  (let [{[a] :argv :as state} (add-arguments state 1)]
    (print (char a))
    state))

(defmethod instruction 20
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 21
  [state]
  (update state :pointer inc))

(defn run
  [max initial-state]
  (loop [idx 0
         state initial-state]
    (when (< idx max)
      (recur (inc idx) (instruction state)))))

(defn -main
  []
  (try
    (run 5000 {:registers [0 0 0 0 0 0 0 0]
               :stack []
               :pointer 0})
    (catch Exception e
      (let [{:keys [pointer] :as state} (ex-data e)]
        (println (ex-message e))
        (println (pr-str {:opcode (get program pointer)
                          :range (subvec program (dec pointer) (+ pointer 5))
                          :state state}))))))

(-main)
