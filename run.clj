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

(defn get-arguments
  [{:keys [registers pointer]} n]
  (for [idx (range n)
        :let [value (get program (+ (inc pointer) idx))]]
    (if (>= value max-literal)
      (get registers (- value max-literal))
      value)))

(defmulti instruction
  (fn [{:keys [pointer]}] (get program pointer)))

(defmethod instruction 0
  [_state]
  (flush)
  (System/exit 0))

(defmethod instruction 1
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 2
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 3
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 4
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 5
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 6
  [state]
  (let [[a] (get-arguments state 1)]
    (assoc state :pointer (int a))))

(defmethod instruction 7
  [{:keys [pointer] :as state}]
  (let [[a b] (get-arguments state 2)]
    (if (zero? a)
      (assoc state :pointer (+ pointer 3))
      (assoc state :pointer (int b)))))

(defmethod instruction 8
  [{:keys [pointer] :as state}]
  (let [[a b] (get-arguments state 2)]
    (if (zero? a)
      (assoc state :pointer (int b))
      (assoc state :pointer (+ pointer 3)))))

(defmethod instruction 9
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

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
  [{:keys [pointer] :as state}]
  (let [[a] (get-arguments state 1)]
    (print (char a))
    (assoc state :pointer (+ pointer 2))))

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
    (run 200 {:registers [0 0 0 0 0 0 0 0]
              :pointer 0})
    (catch Exception e
      (let [{:keys [pointer] :as state} (ex-data e)]
        (println (ex-message e))
        (println (pr-str {:opcode (get program pointer)
                          :range (subvec program (dec pointer) (+ pointer 5))
                          :state state}))))))

(-main)
