(ns color-grid-clj.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import (java.security MessageDigest)
           (java.util BitSet)))

(def width 10)
(defn setup []
  ; Set frame rate to 20 frames per second. (slow, because no animations)
  (q/frame-rate 20)
  ; initial state contains a blank string, an empty list of entered strings, and count 0
  {:s ""
   :names []
   :frame 0})

(defn update-state [state]
  ; all updates are actually handled by keypresses
  state)



(defn hash-string
  "hashes a string into a byte array"
  [s]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.digest digest (.getBytes s))))


(defn hash-to-bitsets
  "Turns a list of strings into a list of bitsets of their hashes"
  [names]
  (map #(BitSet/valueOf (hash-string %)) names))


(defn hash-to-colors
  "Turns a list of strings into a 16-by-16 grid of RGB triplets, based on their hashes"
  [names]
  (into [] (for [i (range 16)
                 :let [bs (hash-to-bitsets names)]]
             (into [] (for [j (range 16)]
                        (into [] (map #(if (.get (nth bs %) (+ (* width i) j))
                                         255
                                         0)
                                      [0 1 2])))))))


(defn draw-state [state]
  (cond (< (:frame state) 3)  (do (q/background 200) ; entering text
                                  (q/text (:s state) 50 50))
        (= 3 (:frame state))  (do (let [colmap (hash-to-colors (:names state))] ; hashing and showing grid
                                    (doseq [i (range 16)
                                            j (range 16)]
                                      (apply q/fill (get-in colmap [i j]))
                                      (q/rect (* i width) (* j width) width width))))
        :else (do (q/background 0))) ; clear



  (defn key-press [old-state event]
    (let [k (event :key)
          k-char  (str (event :raw-key))
          k-code (event :raw-key)]
      (cond
        (or (= k-code processing.core.PConstants/ENTER) (= k-code processing.core.PConstants/RETURN))
        (assoc old-state
               :s ""
               :names (conj (:names old-state)
                            (:s old-state))
               :frame (mod (inc (:frame old-state)) 4))
        (= k-code processing.core.PConstants/BACKSPACE)
        (let [olds (:s old-state)]
          (assoc old-state :s (subs olds
                                    0
                                    (if (= 0 (count olds))
                                      0
                                      (dec (count olds))))))
        :else
        (assoc-in old-state [:s] (str (:s old-state) k-char))))))


(q/defsketch color-grid-clj
  :title "HASHGRID"
  :size [(* 16 width) (* 16 width)]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-typed key-press
  :middleware [m/fun-mode])
