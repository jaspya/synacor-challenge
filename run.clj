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

(defonce program (atom (vec (read-binary-file "challenge.bin"))))

(def max-literal 32768)

(defn add-arguments
  [state n]
  (reduce (fn [{:keys [registers pointer] :as state} _idx]
            (let [raw-value (get @program pointer)
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

(defn halt
  []
  (flush)
  (System/exit 0))

(defmulti instruction
  (fn [{:keys [pointer]}] (get @program pointer)))

(defmethod instruction 0
  [_state]
  (halt))

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
    (when (empty? stack)
      (throw (ex-info "EmptyStackException" state)))
    (-> state
        (assoc-in [:registers a] (peek stack))
        (update :stack pop))))

(defmethod instruction 4
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (if (= b c) 1 0))))

(defmethod instruction 5
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (if (> b c) 1 0))))

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
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (mod (* b c) 32768))))

(defmethod instruction 11
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (rem b c))))

(defmethod instruction 12
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (bit-and b c))))

(defmethod instruction 13
  [state]
  (let [{[a] :reg [_ b c] :argv :as state} (add-arguments state 3)]
    (assoc-in state [:registers a] (bit-or b c))))

(defmethod instruction 14
  [state]
  (let [{[a] :reg [_ b] :argv :as state} (add-arguments state 2)]
    (assoc-in state [:registers a] (bit-and-not (dec max-literal) b))))

(defmethod instruction 15
  [state]
  (let [{[a] :reg [_ b] :argv :as state} (add-arguments state 2)]
    (assoc-in state [:registers a] (get @program b))))

(defmethod instruction 16
  [state]
  (let [{[a b] :argv :as state} (add-arguments state 2)]
    (swap! program assoc a b)
    state))

(defmethod instruction 17
  [state]
  (let [{[a] :argv pointer :pointer :as state} (add-arguments state 1)]
    (-> state 
        (update :stack conj pointer)
        (assoc :pointer a))))

(defmethod instruction 18
  [{:keys [stack] :as state}]
  (if (empty? stack)
    (halt)
    (-> state
        (assoc :pointer (peek stack))
        (update :stack pop))))

(defmethod instruction 19
  [state]
  (let [{[a] :argv :as state} (add-arguments state 1)
        c (char a)]
    (print c)
    (when (= c \newline) (flush))
    state))

(defmethod instruction 20
  [state]
  (throw (ex-info "UnsupportedOperationException" state)))

(defmethod instruction 21
  [state]
  (update state :pointer inc))

(defn run
  [initial-state]
  (loop [state initial-state]
    (recur (instruction state))))

(defn -main
  []
  (try
    (run {:registers [0 0 0 0 0 0 0 0]
          :stack []
          :pointer 0})
    (catch Exception e
      (let [{:keys [pointer] :as state} (ex-data e)]
        (println (ex-message e))
        (println (pr-str {:opcode (get @program pointer)
                          :range (subvec @program (dec pointer) (+ pointer 5))
                          :state state}))))))

(-main)
