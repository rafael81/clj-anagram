(ns
    #^{:author "ChangMin Jeon",
       :doc "generate anagram.
       http://github.com/rafael81/clj-anagram"}
  anagram.core
  (:use [clojure.contrib.repl-utils :only (show source)]	
        [anagram.spell-corrector]))

(defn make-indexed-word [word]
  (apply hash-map (interleave (range (count word)) word)))

(defn make-random-index [n]
  (shuffle (range n)))

(defn rand-word [word]
  (let [word-count (count word) indexed-arr (make-random-index word-count)]
    (apply str (for [idx indexed-arr] ((make-indexed-word word) idx)))))
			 
(defn test-rand-words [word count]
  (take count (iterate rand-word word)))

(def ref-rand-words (ref ()))

(defn alter-work-thread [word words]
      (.start (Thread. (fn [] (dosync (alter ref-rand-words conj (correct word words)))))))

(def atom-rand-words (atom ()))

(defn reset-atom-rand-words [] (reset! atom-rand-words ()))

(def ref-atom-rand-words (ref atom-rand-words))

(defn atom-work-thread [word words]
      (.start (Thread. (fn [] (swap! atom-rand-words conj (correct word words))))))

(defn test-atom-work-thread [word count]
  (map atom-work-thread (test-rand-words word count) (repeat *nwords*)))

(defn test-correct-words [word count]
  (let [rand-words (test-rand-words word count)]
    (filter string? (for [elt rand-words] (let [correct-word (correct elt *nwords*)]
				     (if-not (= elt correct-word)
				       correct-word))))))

(defn is-anagram? [src dst]
  (let [pred-list (map #(.contains dst (str %)) src)]
    (and (= (count src) (count dst)) (every? true? pred-list)
	 dst)))

(defn test-main-anagram [word count]
  (distinct (filter string? (map is-anagram? (repeat word) (test-correct-words word count)))))

