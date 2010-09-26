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

;;; thread code using atom

(def agent-rand-words (agent ()))

(defn agent-work-thread [word words number]
      (.start (Thread. (fn [] (let [correct-word (correct word words)]
				(if-not (= word correct-word)
				  (send agent-rand-words conj correct-word))))))
      (println (format "make thread %d" number)))

(defn test-agent-work-thread [word count]
  (map agent-work-thread (test-rand-words word count) (repeat *nwords*) (range 1 (+ count 1))))

;;;; thread code using atom

(defn is-anagram? [src dst]
  (let [pred-list (map #(.contains dst (str %)) src)]
    (and (= (count src) (count dst)) (every? true? pred-list)
	 dst)))

(defn test-main-anagram [word cnt]
  (dorun (test-agent-work-thread word cnt)) ;; to prevent map's return 
  (await agent-rand-words)
  (distinct (filter string? (map is-anagram? (repeat word) @agent-rand-words))))

