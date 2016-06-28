;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns flowing-geyser
  (:require [gorilla-plot.core :as plot]))
;; @@

;; @@
(reduce * (repeat 10 2))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1024</span>","value":"1024"}
;; <=

;; @@
(reduce str (repeat 4 "a"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;aaaa&quot;</span>","value":"\"aaaa\""}
;; <=

;; @@
(repeatedly 20 #(rand-int 100))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>70</span>","value":"70"},{"type":"html","content":"<span class='clj-unkown'>80</span>","value":"80"},{"type":"html","content":"<span class='clj-unkown'>56</span>","value":"56"},{"type":"html","content":"<span class='clj-unkown'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-unkown'>59</span>","value":"59"},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>94</span>","value":"94"},{"type":"html","content":"<span class='clj-unkown'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-unkown'>57</span>","value":"57"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>70</span>","value":"70"},{"type":"html","content":"<span class='clj-unkown'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>59</span>","value":"59"},{"type":"html","content":"<span class='clj-unkown'>75</span>","value":"75"},{"type":"html","content":"<span class='clj-unkown'>54</span>","value":"54"},{"type":"html","content":"<span class='clj-unkown'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-unkown'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-unkown'>52</span>","value":"52"},{"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>21</span>","value":"21"}],"value":"(70 80 56 43 59 7 94 25 57 5 70 17 59 75 54 29 27 52 8 21)"}
;; <=

;; @@
Math/PI
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>3.141592653589793</span>","value":"3.141592653589793"}
;; <=

;; @@
(defn calculate-pi
  "Calculates Pi using the approximation 4 * (1 - 1/3 + 1/5 - 1/7 + ...)"
  [iterations]
  (let [odd-numbers (filter odd? (iterate inc 1))]
    (* 4.0
       (apply + (map / (cycle [1 -1]) (take iterations odd-numbers))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/calculate-pi</span>","value":"#'user/calculate-pi"}
;; <=

;; @@
(calculate-pi 1000000)
;; @@

;; @@
(defn sum-last-2 
   ([] (sum-last-2 1 2)) 
   ([n m] (cons n (lazy-seq (sum-last-2 m (+ n m))))))
;; @@

;; @@
(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))
;; @@

;; @@
(take 20 (sieve (iterate inc 2)))
;; @@

;; @@
(def prime-numbers
  ((fn f [x]
     (cons x
       (lazy-seq
         (f (first
              (drop-while
                (fn [n]
                  (some #(zero? (mod n %))
                    (take-while #(<= (* % %) n) prime-numbers)))
                (iterate inc (inc x))))))))below
2))
;; @@

;; @@
(defn count-substring [txt sub]
  (count (re-seq (re-pattern sub) txt)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/count-substring</span>","value":"#'user/count-substring"}
;; <=

;; @@
(count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "ACATCT" )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>185</span>","value":"185"}
;; <=

;; @@
(count-substring "GCGCG" "GCG")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}
;; <=

;; @@
(re-seq (re-pattern "GCG") "GCGCG")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;GCG&quot;</span>","value":"\"GCG\""}],"value":"(\"GCG\")"}
;; <=

;; @@
(re-pattern "GCG")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&quot;GCG&quot;</span>","value":"#\"GCG\""}
;; <=

;; @@
(re-seq #"\d{2}" "clojure 111.1.0")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;11&quot;</span>","value":"\"11\""}],"value":"(\"11\")"}
;; <=

;; @@
(re-seq #"GC" "GCGCG")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;GC&quot;</span>","value":"\"GC\""},{"type":"html","content":"<span class='clj-string'>&quot;GC&quot;</span>","value":"\"GC\""}],"value":"(\"GC\" \"GC\")"}
;; <=

;; @@
(filter #{"GCG"} "GCGCG")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}
;; <=

;; @@
(require '[clojure.string :as s])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(->> (s/split "ggfdhdfand" #"\D{3}") ; split string on non-letter characters
     (map s/lower-case) ; for case-insensitive search
     (filter (partial = "and"))
     count)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}
;; <=

;; @@
(s/split "ggfdhdfand" #"\W{3}")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;ggfdhdfand&quot;</span>","value":"\"ggfdhdfand\""}],"value":"[\"ggfdhdfand\"]"}
;; <=

;; @@
(loop (subs "ddsfsdgs" 1 3))
;; @@

;; @@
(defn my-re-seq [re string]
         "Something like re-seq"
         (let [matcher (re-matcher re string)]

           (loop [match (re-find matcher) ;loop starts with 2 set arguments
                  result []]
             (if-not match
               result
               (recur (re-find matcher)    ;loop with 2 new arguments
                      (conj result match))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/my-re-seq</span>","value":"#'user/my-re-seq"}
;; <=

;; @@
(my-re-seq #"\d{2}" "0123456789")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;01&quot;</span>","value":"\"01\""},{"type":"html","content":"<span class='clj-string'>&quot;23&quot;</span>","value":"\"23\""},{"type":"html","content":"<span class='clj-string'>&quot;45&quot;</span>","value":"\"45\""},{"type":"html","content":"<span class='clj-string'>&quot;67&quot;</span>","value":"\"67\""},{"type":"html","content":"<span class='clj-string'>&quot;89&quot;</span>","value":"\"89\""}],"value":"[\"01\" \"23\" \"45\" \"67\" \"89\"]"}
;; <=

;; @@
(loop [x 10]
  (when (> x 1)
    (println x)
    (recur (- x 2))))
;; @@
;; ->
;;; 10
;;; 8
;;; 6
;;; 4
;;; 2
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(loop [iter 1
       acc  0]
  (if (> iter 10)
    (println acc)
    (recur (inc iter) (+ acc iter))))
;; @@
;; ->
;;; 55
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def loop-sub 
  (fn [sub txt]
    (loop [it 0 x 0]
      (when (< x (- (count txt) (count sub)))
        (recur (+ it (if (= (subs txt x (+ x (count sub))) sub) 1 0)) (+ x 1) )))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/loop-sub</span>","value":"#'user/loop-sub"}
;; <=

;; @@
(defn lpsb [sub txt]
  (loop [it 0 x 0]
    (when (< x (- (count txt) (count sub)))
      (recur (+ it (if (= (subs txt x (+ x (count sub))) sub) 1 0)) (+ x 1) ))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/lpsb</span>","value":"#'user/lpsb"}
;; <=

;; @@
(defn lpsb [txt sub]
  (loop [it 0 x 0]
    (if (> x (- (count txt) (count sub)))
      it
      (recur (+ it (if (= (subs txt x (+ x (count sub))) sub) 1 0)) (+ x 1) ))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/lpsb</span>","value":"#'user/lpsb"}
;; <=

;; @@
(lpsb "gdfgd" "d")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
(if (= (subs "gdgdfhd" 1 (+ 1 (count "fg"))) "fg") 1 0)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}
;; <=

;; @@
(loop-sub "gdfhfhf" "gd")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def factorial
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc cnt))
; in loop cnt will take the value (dec cnt)
; and acc will take the value (* acc cnt)
))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/factorial</span>","value":"#'user/factorial"}
;; <=

;; @@
(def tl
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (+ acc 1))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/tl</span>","value":"#'user/tl"}
;; <=

;; @@
(tl 4)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}
;; <=

;; @@
(loop [res [0 1]]
        (if (>= (count res) 10)
          res
          (recur (conj res (+' (inc (last res)) (dec (last (butlast res))))))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-long'>34</span>","value":"34"}],"value":"[0 1 1 2 3 5 8 13 21 34]"}
;; <=

;; @@
(reduce str (rest (reduce str (rest "gfd"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"}
;; <=

;; @@
(+ 2 4 6 6 3 2 6 2 7 1 8 4 2 7)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>60</span>","value":"60"}
;; <=

;; @@
(reduce * (repeat 10 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>59049</span>","value":"59049"}
;; <=

;; @@
(str (first "gdfdsdf") (first (rest "gdfdsdf")))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;gd&quot;</span>","value":"\"gd\""}
;; <=

;; @@
(re-find (re-pattern "\\d+(.)") "abc123def23") 
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;123d&quot;</span>","value":"\"123d\""},{"type":"html","content":"<span class='clj-string'>&quot;d&quot;</span>","value":"\"d\""}],"value":"[\"123d\" \"d\"]"}
;; <=

;; @@
(some #(when (even? %) %) '(1 2 3 4))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
(defn a[b,c] (= b c))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/a</span>","value":"#'user/a"}
;; <=

;; @@
(a "fdgfd" "ds")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
;; <=

;; @@
(some #(= "d" %) (seq "fdgdhfdg"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(seq "fdgdhfdg")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(\\f \\d \\g \\d \\h \\f \\d \\g)</span>","value":"(\\f \\d \\g \\d \\h \\f \\d \\g)"}
;; <=

;; @@
(defn subsumes [main sub]
  (some
   (partial = (seq sub))
   (partition (count sub) 1 main)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/subsumes</span>","value":"#'user/subsumes"}
;; <=

;; @@
(subsumes "Hello" "ll") ;;=>> true
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(partition 4 1 "gffdhfd")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\g</span>","value":"\\g"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"}],"value":"(\\g \\f \\f \\d)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"},{"type":"html","content":"<span class='clj-char'>\\h</span>","value":"\\h"}],"value":"(\\f \\f \\d \\h)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"},{"type":"html","content":"<span class='clj-char'>\\h</span>","value":"\\h"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"}],"value":"(\\f \\d \\h \\f)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"},{"type":"html","content":"<span class='clj-char'>\\h</span>","value":"\\h"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"}],"value":"(\\d \\h \\f \\d)"}],"value":"((\\g \\f \\f \\d) (\\f \\f \\d \\h) (\\f \\d \\h \\f) (\\d \\h \\f \\d))"}
;; <=

;; @@
(seq "fdfh")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(\\f \\d \\f \\h)</span>","value":"(\\f \\d \\f \\h)"}
;; <=

;; @@
(= (seq "fdg") (partition 3 "gfhfgfjfgfdg"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
;; <=

;; @@
(reduce + (map #(if % 1 0) (map #(= (seq "aba") %) (partition 3 1 "gfhfgfjfgfdgfdgababa"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
(partition 3 1 "ababa")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"}],"value":"(\\a \\b \\a)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"}],"value":"(\\b \\a \\b)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"}],"value":"(\\a \\b \\a)"}],"value":"((\\a \\b \\a) (\\b \\a \\b) (\\a \\b \\a))"}
;; <=

;; @@
(int 0)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}
;; <=

;; @@
(= 1 2)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
;; <=

;; @@
(int (boolean 1))
;; @@

;; @@
(if false 1 0)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}
;; <=

;; @@
(reduce + (map #(if % 1 0) (map #(= (seq "ACATCT") %) (partition (count "ACATCT") 1 (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt")))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>185</span>","value":"185"}
;; <=

;; @@
(count (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1108251</span>","value":"1108251"}
;; <=

;; **
;;; ## Function with partition of string to many many many group :)
;;; Bad way, but fun))
;; **

;; @@
(defn count-substring-with-overlaps [txt sub]
  "Function to count number of occuriences of sub in txt with overplaps, i.e. you'll get 2 aba in ababa"
  (reduce + (map #(if % 1 0) (map #(= (seq sub) %) (partition (count sub) 1 txt)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/count-substring-with-overlaps</span>","value":"#'user/count-substring-with-overlaps"}
;; <=

;; @@
(time (count-substring-with-overlaps (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "ATAT"))
;; @@
;; ->
;;; &quot;Elapsed time: 5619.661408 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-long'>3632</span>","value":"3632"}
;; <=

;; @@
(defn count-substring [txt sub]
  "Function to count number of occuriences of sub in txt, but without overlaps"
  (count (re-seq (re-pattern sub) txt)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/count-substring</span>","value":"#'user/count-substring"}
;; <=

;; @@
(time (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAAAAA"))
;; @@
;; ->
;;; &quot;Elapsed time: 22.151325 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>21</span>","value":"21"}
;; <=

;; @@
(time (+ (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAA") 
            (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAA") 
            (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAAA") 
            (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAAAA")))
;; @@
;; ->
;;; &quot;Elapsed time: 89.019686 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-long'>9598</span>","value":"9598"}
;; <=

;; @@
(count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "A{8}T{2}") 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}
;; <=

;; @@
(time (- (+ (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AA") 
            (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAA") 
            (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAA") 
            (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAAAA")) 
         (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "AAAAAA")))
;; @@
;; ->
;;; &quot;Elapsed time: 351.236827 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-long'>93417</span>","value":"93417"}
;; <=

;; **
;;; # !!!TODO
;;; It's definetly good idea to check overlaps only if you found the occurience. You don't need to partition all string to groups and cycle over it. So concatenation of two written functions will do the right thing!
;; **

;; **
;;; ##Simple realization, with loop
;; **

;; @@
(defn count-substring-with-overlaps-loop [txt sub]
  "Function to count number of occuriences of sub in txt using loop, like python"
  (loop [it 0 x 0]
    (let [lentxt (count txt) lensub (count sub)]
    (if (> x (- lentxt lensub))
      it
      (recur (+ it (if (= (subs txt x (+ x lensub)) sub) 1 0)) (+ x 1) )))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/count-substring-with-overlaps-loop</span>","value":"#'user/count-substring-with-overlaps-loop"}
;; <=

;; @@
(time (count-substring-with-overlaps-loop (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "ATAT" ))
;; @@
;; ->
;;; &quot;Elapsed time: 512.515107 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-long'>3632</span>","value":"3632"}
;; <=

;; **
;;; #TODO!!!
;;; 
;;; Rewrite my functions with regexs. Write recursive regex func with some: take firsr match, then apply func to substr thet is str - match and so on. And write function subs function with some regex cases
;; **

;; @@
(+ 1 2)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}
;; <=

;; @@
(re-find #"aa" "aaabaa")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;aa&quot;</span>","value":"\"aa\""}
;; <=

;; @@
(use '[clojure.string :only (replace-first)])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(replace-first "aaaa" #"aa" "bb" )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;bbaa&quot;</span>","value":"\"bbaa\""}
;; <=

;; @@
(.indexOf "fhfghgfjf" "fg")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}
;; <=

;; @@
(.indexOf (subs "fhfghgfjf" (.indexOf "fhfghgfjf" "fg")) "fg")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}
;; <=

;; @@
(if 0 "a" "b")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;a&quot;</span>","value":"\"a\""}
;; <=

;; @@
(subs "fdgfdh" 2)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;gfdh&quot;</span>","value":"\"gfdh\""}
;; <=

;; @@
(.indexOf "dgdfd" "fd")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}
;; <=

;; @@
(subs "dgdfd" (+ 1 (.indexOf "dgdfd" "fd")))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;d&quot;</span>","value":"\"d\""}
;; <=

;; @@
(defn sub-from-index [txt sub]
  (subs txt (+ 1 (.indexOf txt sub))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/sub-from-index</span>","value":"#'user/sub-from-index"}
;; <=

;; @@
(sub-from-index "hfjgfj" "gf")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;fj&quot;</span>","value":"\"fj\""}
;; <=

;; **
;;; ## Recursion is recursion is recursion is recursion is recursion )))
;; **

;; @@
(defn sub-rec [txt sub]
(loop [t txt i 0]
  (let [ind (.indexOf t sub)]
  (if (= -1 ind)
    i
    (recur (subs t (+ 1 ind)) (+ i 1))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/sub-rec</span>","value":"#'user/sub-rec"}
;; <=

;; @@
(time (sub-rec (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "ATAT" ))
;; @@
;; ->
;;; &quot;Elapsed time: 3053.252783 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-long'>3632</span>","value":"3632"}
;; <=

;; @@
(defn count-substring-with-overlaps-loop [txt sub]
  "Function to count number of occuriences of sub in txt using loop, like python"
  (loop [it 0 x 0]
    (if (> x (- (count txt) (count sub)))
      it
      (recur (+ it (if (= (subs txt x (+ x (count sub))) sub) 1 0)) (+ x 1) ))))
;; @@

;; @@
(loop [i 6]
  (if (zero? i)
    i
    (recur (dec i))
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}
;; <=

;; @@
(sub-rec "gdfgfdg" "dg")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}
;; <=

;; @@
(if (= -1 (.indexOf "fdgd" "fd")) "yes" "no")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;no&quot;</span>","value":"\"no\""}
;; <=

;; @@
(.indexOf "fdgd" "gd")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}
;; <=

;; @@
(count (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1108251</span>","value":"1108251"}
;; <=

;; @@
(defn count-substring [txt sub]
  "Function to count occuriencies of substring in string with regular expressions"
  (count (re-seq (re-pattern sub) txt)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/count-substring</span>","value":"#'user/count-substring"}
;; <=

;; @@
(time (count-substring (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt") "(?=ATAT)"))
;; @@
;; ->
;;; &quot;Elapsed time: 85.22592 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>3632</span>","value":"3632"}
;; <=

;; @@
(subs (slurp "/home/azheltov/Downloads/PatternCount.txt") 7 (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 26))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;TAACAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTGAGCCTTTGAGCCTTTTAGCCTTTCAGCCTTTAGCCTTTAAGCCTTTCCGCATCGAGCCTTTCAGCCTTTCGTAGCCTTTCGAGCCTTTAGCCTTTCAGCCTTTAGAGCCTTTAAGCCTTTAGTCGATGTAGCCTTTAGCCTTTAGCCTTTAGCCTTTGCAGCCTTTAGTAGGCAAGCCTTTTAGCCTTTGAGCCTTTCGAGCCTTTCTCGCTAGCCTTTAGCCTTTGGTGAGCCTTTTAGCCTTTAGCCTTTTCGCAGCCTTTTGAGCCTTTCTTGTTTGAATGGCAAGAGCCTTTTCGAGCCTTTAGCCTTTGAGCCTTTCAGCCTTTAAAAGCCTTTCGTTAGCCTTTAGCCTTTATCGAGCCTTTAAGCCTTTTATGCAAAGCCTTTGAGCCTTTAGCCTTTCAGCCTTTCAGCCTTTCATTGACAAGCCTTTCAGCCTTTAGCCTTTAGCCTTTCTCAGCCTTTGAGCCTTTGAGCCTTTGTCGAGCCTTTTTTCAGAGCCTTTTAGCCTTTAGCCTTTGAGCCTTTAGCCTTTAGCCTTTTACGAGCCTTTGCAAGCCTTTCAGCCTTTCCAAGCCTTTAGCCTTTGCTTTAGCCTTTCATGGGATAGCCTTTAGCCTTTATTAAGCCTTTTTTATCAAGCCTTTGTAGCCTTTAAGCCTTTCCAGCCTTTAGGAGCCTTTGTATAGCCTTTTGAGCCTTTCTACAGTAAAGCCTTTTTTGGTCAGCCTTTCTAGCCTTTGATAGCCTTTCTGAAGCCTTTGGCGGAGCCTTTCTGTTAACAGCCCAGCCTTTCTCATAGCCTTTGCGGTATCAGCCTTTGCAGCCTTTCTGGAGCGATAGCCTTTCAGCCTTTCCGAGCCTTTTTCAGAGCCTTTGAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTCCGAGCCTTTTCAGCCTTTACAGCCTTTTTAGCCTTTGAGCCTTTCACAGCCTTTGAGCTAGCCTTTAAGTTAAAGCCTTTAGCCTTTCAGCCTTTACATTAGCCTTTTAGCCTTTTCAGCCTTTAGAGCCTTTAGCCTTTGCTGAAGCCTTTAGTAGCCTTTAGCCTTTGCGAAGCCTTTCTGGTGCAACAAGTGAAGCCTTTGCCCTAGCCTTTGCTAGCCTTTCCGAGCCTTTGTCGATATAGCCTTTAGCCTTTAGAAAGCCTTTAGCCTTTGCTAGCCTTTATAGCCTTTAGCCTTTAGCCTTTCCCAGCCTTTAGCCTTTATCCTAAGCCTTTAGCCTTTTCCAGAAGCAGCCTTTTGATCAGAGCCTTTCTTCGGACTGCTCCCAGCCTTTAGCCTTTCAGCCTTTAGCCTTTAGCCTTTCAGCCTTTAGCCTTTTCTAGCCTTTAGCCTTTGTGTAGCCTTTCTAGCCTTTACGAGCCTTTGCCCAGCCTTTCCCAGCCTTTGAGAGCCTTTACCATATAGCCTTTACATAAGCCTTTGATGAGCCTTTAGCCTTTCGAGCCTTTCAGCCTTTACTCCAGCCTTTATAGCCTTTATATAGCCTTTCCTGTTAGGCCGTCGGTGCAGCCTTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTGCTAGCCTTTCCAGCCTTTACAGCCTTTTACCGAGCCTTTCCATCAGCCTTTAGCCTTTATATCTCTGATCGGGTAGCCTTTCGGCTAGCCTTTGGTAGCCTTTTTCAGCCTTTATGTAAAGCCTTTGTAGCCTTTGATGTGAGCCTTTAGAAGCCTTTGTCAGCCTTTTAGCCTTTAGCCTTTAGCCTTTTTAAGCCTTTTACAAGCCTTTACTCAGAGCCTTTACGAGCCTTTTAGCCTTTGCAGCCTTTTAGCCTTTTGATGAGCCTTTGCGGAGCCTTTTCTTTCAGCCTTTTGGCAGCCTTTAGCCTTTGTACAGCCTTTTTGAGCACAGCCTTTCGCGAAAGAGCCTTTATCAGCCTTTAAGCCTTTGCTAGCCTTTAGCCTTTTGAGCCTTTAGCCTTTGTATCTGTCTATCATCGAGCCTTTCTAAGCCTTTGCGGAAGCCTTTAGCCTTTGTCAGCCTTTCAAAGCCTTTAGCCTTTTATTCAAGCCTTTGAACCATAGCCTTTGGCAGCCTTTCAAGCCTTTGACGACAGCCTTTAGCCTTTCATTAGCCTTTTAGGAGGCTCATCCGTCTAGCCTTTAAATAGCCTTTAGCCTTTATAGCCTTTAAGCCTTTAGCCTTTTAAGCCTTTGAGAGCCTTTAAAGCCTTTAAACCAAGCCTTTGCGAAGCCTTTAGCCTTTAGCCTTTCGCAGCCTTTAGCCTTTCGAGCCTTTTGTAGCCTTTTGGAGAGCCTTTGGGCAAGCCTTTAGTATAAGCCTTTAGCCTTTTAGCCTTTCAAGCCTTTAGCCTTTAAGCCTTTGGCACAGCCTTTTGAGCCTTTCAGGAGCCTTTATTGGTGAGCCTTTAGTATAGCCTTTTCAGCCTTTGGCAGCCTTTAATGAAAGCCTTTGCTCAGCCTTTTTCAGCCTTTACAGCCTTTCAAGCCTTTAGCCACAAGCCTTTAGCCTTTAGCCTTTCAGCCTTTGCGAGCCTTTGTAGCCTTTAAGCCTTTCAGCCTTTAGCCTTTAGCCTTTTTCAAGCCTTTTCAGCCTTTCAAAGCCTTTCAAGCCTTTGAAGCCTTTCTAAGCCTTTGAGCCTTTGAGCCTTTGAGCCTTTAGCCTTTGTTCCTAGCCTTTATAGCCTTTTAGGCAGCCTTTCAGAGCCTTTTAAGCCTTTAGCCTTTCAGAAAGAGCCTTTAGCCCAGCCTTTTGATTAGCCTTTAGGGAACAGCCTTTAGCCTTTTAAGCCTTTGGTATACAATCAACGCAGCCTTTAGCCTTTAAGCCTTTTGGAGCCTTTCAGACTGATCCCAGCCTTTCAGCCTTTCTCAGCCTTTAAGCCTTTCTCCAAGCCTTTTGAGCCTTTTCGAGCCTTTAGTGAGCCTTTTGAAGCCTTTGTTTAGCCTTTTGTATAGGGTAGCCTTTAGCCTTTCCGGAAGCCTTTTGTAGCCTTTAAGCCTTTTGTCCGGGAAAGCCTTTGTAAGCCTTTAATGCAGCCTTTCCTATAGCCTTTAAGCCTTTCAGCCTTTTGGAGCCTTTTCTCAGCCTTTAGCCTTTCGCCAGCCTTTCTCCCGAGCAGCCTTTTAGAAAAAGCCTTTTAGCCTTTTACCGTGGACAGCCTTTCACGAGCCTTTACAGGCTAGCCTTTAGCCTTTGCTAGCCTTTTCCCAGCCTTTTGAGCCTTTAAGCCTTTCTAAGTTCTACGCTTGGGCTAAAGCCTTTAGCCTTTAAGCCTTTCAGCCTTTTGCAGCCTTTATATAACTTGAGCCTTTAGCCTTTAGCCTTTATAGCCTTTAGCCTTTTAGCCTTTTATATCCCTTAAGCCTTTGTAAGCCTTTAGCCTTTAAGCCTTTACGAGGAAAGCCTTTCATGCAGCCTTTAGCCTTTAGCCTTTGAGCCTTTCCAGCCTTTCAGCCTTTCAGCCTTTAGCCTTTAGCCTTTAGCCTTTTAGCCTTTATGAGCCTTTATAGCCTTTAGCCTTTTCACCAGCCTTTCCAGATGCACAAGCCTTTCAGCCTTTAGCCTTTCGAGCCTTTGGCTTATAGCCTTTCATCAGCCTTTCTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTTCTAGCCTTTCAGCCTTTAGCCTTTTCGAAGCCTTTAGCCTTTTTAGCCTTTAGCTCAGCCTTTAGCCTTTATCTAACAGCCTTTAGCCTTTAGCCTTTAAAGCCTTTATGTCCAATTCTAACAGCCTTTAGCCTTTAAAGCCTTTGCAGCCTTTGAGCCTTTTAGCCTTTGAAGCCTTTAGCCTTTGTCAGCCTTTCCAGCCTTTTAGCCTTTAGCAGCCTTTAGTACGCCAGCCTTTAGCCTTTGTATAAGCCTTTAGCCTTTAGCCTTTCCACTAGCCTTTAGCCTTTAGAGGAGCGATAGCCTTTCAGCCTTTAGAAAGCCTTTGTTGCTGCTAGCCTTTGGGTTCTCAGCCTTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTTGTAGCCTTTTACATAGGATTGATTCAAAAGCCTTTTTGAGCCTTTCTGCATTAGCCTTTTCCTCTAGCCTTTAGCCTTTCGCAGCCTTTAGCCTTTTAGAGCCTTTAGATAGCCTTTCGCGACAGCCTTTTGTTTAGCCTTTAGCCTTTGTTAGCCTTTGAGCCTTTGAGCCTTTTAGCCTTTCCTAGCCTTTCAGCCTTTCCAAAGCCTTTGACAGGGTGTAGCCTTTCTAGCCTTTTTAGCCTTTAGCCTTTAAACTTAAGCCTTTTTAGCCTTTAGCCTTTCAACCCAGCCTTTAGCCTTTTAAGCCTTTAGCCTTTAGCCTTTTTAGAAGCCTTTTAGCCTTTAGCCTTTGGAGCCTTTCAGATCTCAGCCTTTTCGAGCCTTTTAGCCTTTTCAGAAAAGTAGCCTTTTTAGCAGCCTTTTAAAGCCTTTGGAGCCTTTAGCCTTTAGCCTTTGTAGCCTTTTCCCAAAAGCCTTTACAGCCTTTGTGAGCCTTTTAGTTCGTTTGAGCCTTTCCAGCCTTTCAGCCTTTAGCCTTTATAGCCTTTTGCGAGAAGCCTTTAAGCCTTTAGCCTTTTGACGTTCTAGAGCCTTTGGAGCCTTTCACGCGAGCCTTTCAAGCCTTTGACTCCGCAGCCTTTTCGCGACCAGCCTTTGCCGTGCCAGCCTTTAGCCTTTCAACACAGCCTTTAGCCTTTGGGCCGCAGAGCCTTTGAGTAGCCTTTAGCCTTTGACAGCCTTTAGCCTTTCTAGCCTTTGCAGCCTTTGTCTAGGTAGCCTTTAGCCTTTAGCCTTTCTAGCCTTTTAGCCTTTAGCCTTTTGAGCCTTTTGGAAGCCTTTCAGCCTTTAGCCTTTCGCGAGCCTTTGAGCCTTTACCCAGCCTTTACGGAGCCTTTAGCCTTTCCCATAGCCTTTAGCCTTTCCAGCCTTTAGCCTTTTAGCCTTTCAAATCTAAGCCTTTCGCATATATGGTAGCCTTTAGCCTTTAGCCTTTATGGTCCTTCAGTTTGAGCCTTTTAGAGCCTTTAAAGGAGCCTTTGTAAGACGAAGGTAGCCTTTAGCCTTTGCCAGCCTTTTTAGCCTTTAGCCTTTAAAAAGCCTTTGAGCCTTTAGCCTTTAGCCTTTGAGCCTTTAGCCTTTTCTCCTAGCCTTTCATAGCCTTTGAGCCTTTAGCCTTTTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTGGAGGTCAGCCTTTATGTTAAAGCCTTTAGTTCCCAGCCTTTCAGCCTTTAGCCTTTAGCCTTTGAGCCTTTCAGCCTTTTAGCCTTTCAGCCTTTCAGCCTTTGAAGCCTTTTGTAGCCTTTGCCCGAGCCTTTAGCCTTTAGCCTTTCCCAACCCTGATCCGTAGCCTTTGGGCTGATCCTGAGCCTTTTCAGCCTTTAAGCCTTTAGCCTTTAGCCTTTGAGAAGCCTTTAGCCTTTCAGCCTTTAACAGCCTTTAAGCCTTTATAGCCTTTAGCCAGCCTTTGCAGCCTTTCAGTAGCCTTTAGCCTTTAGCCTTTCTAGCCTTTCTTGGAGCCTTTCCCAGCCTTTAAGAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTAGCCTTTTCGTAGCCTTTGACCATTGTCAGCCTTTCTACTGAGCCTTTCATAGCCTTTTTTAGCCTTTCTAGCAGCCTTTGGAGCCTTTAGAAGAGCCTTTAGCCTTTTAAGCCTTTGAGCCTTTAACACAAGCCTTTATCTGGGCCGCGAGCCTTTTCAACCTAACTACAGCCTTTCTAAGCCTTTAGCCTTTAGCCTTTCAGCCTTTTAGCCTTTACCGAGCCTTTGCGGGAAGCCTTTAAAGAGCCTTTAGAAAAAGCCTTTGGGATAGCCTTTCCAGCCTTTCCAGCCTTTTTAGCCTTTTCCTCAAGATTTAGCCTTTGATGAAGCCTTTGAGCCTTTAGCCTTTCATTGAGCCTTTTAAGCCTTTCAGCCTTTTCTCATCAGCCTTTCACAGCCTTTCTACAGCCTTTAGCCTTTAGCCTTTGGAGCCTTTTCGCCCCGAGCCTTTAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTGTAGCCTTTAGAGCCTTTGCTTAGCCTTTAGCCTTTAGTAGCCTTTAGATAGCCTTTTCTGGGAGCCTTTACAGCCTTTAGCCTTTAGCCTTTAGCCTTTTAAAGCCTTTCCCCAAAGCCTTTGTTGAGCCTTTAGCCTTTACAGTCTAGCCTTTAGCCTTTCAAGCCTTTACCTTAGCCTTTGGCAGCCTTTCTAGCCTTTAGCCTTTTCAGCCTTTAGCCTTTAAGCCTTTAGCCTTTTCGAGCCTTTGAGCCTTTAAGCCTTTATAAAAAGCCTTTAGCCTTTAAGCCTTTACCAGCCTTTAGCCTTTCAGCCTTTTATCGGAAAGCCTTTAAGCCTTTTAGCCTTTCAGCCTTTGAGCCTTTCAGCCTTTAGCCTTTGGCAAAGCCTTTTTGCAGCCTTTGGAAGCCTTTAGCCTTTTTCAAGCCTTTCAGCCTTTAGCCTTTGCACGTATTAGGAAGCCTTTTACTCTAAGCCTTTATCAGCCTTTAGCCTTTAGCCTTTAAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTACGGTCAGCCTTTGGTAGCCTTTTCAGCCTTTAAGCCTTTAAGCCTTTGAGCCTTTAGCCTTTAGCCTTTGAGCCTTTAAGAGCCTTTCAGCCTTTTTTAGCCTTTTAGCCTTTGAGCCTTTCCTAGCCTTTCAAGCCTTTGAGCCTTTCGAAGCCTTTTAGCCTTTAGCCTTTAGCCTTTATGGAGCCTTTAGCCTTTAGCGGAGCCTTTGAGCCTTTACAGAGCCTTTAGCCTTTAGCCTTTTAAGCCTTTTGCAGCCTTTCAAAGAGCCTTTAGCCTTTACGGAGCCTTTAGCCTTTAAGCCTTTCTCACTAGCCTTTTTAGCCTTTGAGCCTTTATGACGAAGCCTTTAGCCTTTTGTCGTGACCTGAGCCTTTAGCCTTTACAGCCTTTCAGCCTTTAGCCTTTCTTAAAAGCCTTTTAGCCTTTTTGAGCCTTTACAGCCTTTCGAGCCTTTGAGCCTTTCCCAGCCTTTGAAGCCTTTTGGACAGAGCCTTTGCTAGCCTTTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTACTTAGCCTTTTAGCCTTTATGGATAGCCTTTAGCCTTTGAGAGCCTTTGCCTAGCCTTTGAAGCCTTTTTAGCCTTTAACGAGCCTTTAGCCTTTAGCCTTTAGCCTTTAAGCCTTTAGCCTTTCGAGCCTTTCTCAGCCTTTGTAGCCTTTAGCCTTTAGAGCAGCCTTTAGCCTTTCCAGCCTTTAGCCTTTTCAGCCTTTAGCCTTTCAGCCTTTGCCCCGAGCACGTAGCCTTTACAGCCTTTAGCCTTTAGCCTTTTAGCCTTTACAGCCTTTTGAGCCTTTAGCCTTTGAAAGCCTTTTGAAGAGCCTTTCAGCCTTTCTTACTAGCCTTTGCAGCCTTTTAGCCTTTCCGAGCCTTTGATAGCCTTTGTCGGTAAGCCTTTGTAGAGCCTTTAGCCTTTAAGCCTTTGGTAAAGAGCCTTTTCAACAGCCTTTCGGAGCCTTTCGCTACAAGCCTTTTGGCCTAGCCTTTAGCCTTTCAGCCTTTCAAGAGCCTTTAGCCTTTCGCAGCCTTTATAGCCTTTCAGCCTTTCAGCCTTTAGCCTTTAGAGCCTTTGAGCCTTTCGTTATCTAAGCCTTTACTCCATAGCCTTTGAGCCTTTAGCCTTTGTCAGTCGAGCCTTTGTTCTTGAGCCTTTAGCCTTTGCAGCCTTTAGCCTTTTGTTTGTGGAGCCTTTAGCCTTTGAATACAGCCTTTAGCCTTTAGCCTTTAGCCTTTCTAGCCTTTCAGCAGCCTTTGTAGCCTTTGAACCAGCCTTTAGCCTTTTAGCCTTTTCCTTAGCCTTTCCAGCCTTTTAGTGAGCCTTTAGCCTTTGCACCAGCCTTTAGCCTTTAGCCTTTCAGCCTTTAGCCTTTCGAGCCTTTTAGCCTTTGAACAGCCTTTTGAGCCTTTGACGATATGAGCCTTTAGCCTTTTGTAGCCTTTTTTAGCCTTTGAACAGCCTTTGGAGTCAAGCCTTTACGCAGCCTTTCCAGCCTTTCAGCCTTTAGCCTTTGGTCAGCCTTTTCAGAGCCTTTGCGGTTAGCCTTTGAATAGCCTTTAAAGCCTTTCTCAGCCTTTGTAAGCCTTTAGCCTTTTAGCCTTTGTGAGCCTTTCAGCCTTTCCGAGCCTTTAGCCTTTGCCTACGGAAGCCTTTAGCCTTTGCTATCAGCTTGAGCCTTTTAGCCTTTAGTAGCAGCCTTTTAGCCTTTTAGCCTTTCAGCCTTTCTCTAGCCTTTAGCCTTTATCCGAGCCTTTACCAGCCTTTGAGCCTTTAGCCTTTATAGCCTTTATACGTAGCTAGCCTTTAGCCTTTAGAGCCTTTACCCTGTACCAGCCTTTAAGCCTTTCTCGTGAAGCCTTTAGCCTTTGAGCCTTTCGAGCCTTTAGCCTTTAGCCTTTAAGCCTTTTTGTGTGAGCCTTTAGCCTTTGGGGAGCCTTTAGCCTTTCAGCCTTTTAGCCTTTTCAAGCCTTTAGCCTTTAGCCTTTTGAGCCTTTAAAGCCTTTAGCCTTTAGGTAGCAAGCCTTTCGTTATAGCCTTTTATAAGCCTTTTTTAATGAGCCTTTAGCCTTTAGCCTTTGAGCAGCCTTTAGCCTTTAGTAGCCTTTTGATATTAGCCTTTCAGCCTTTAGCCTTTCCCCGAGCCTTTGTTAGAGCCTTTGCAGCCTTTGGAGCCTTTAGCCTTTCGGAGCCTTTAGCCTTTGGGACAGCCTTTAGCCTTTAGCCTTTGAAGCCTTTTGCAGCCTTTAAGATAGCCTTTGAGCCTTTTCAGCCTTTACAGCCTTTAAGCCTTTAGCCTTTGAGCCTTTGAGCCTTTTGAGCCTTTTAGCCTTTGTTGCAGCCTTTAGCCTTTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTGAGCCTTTGAGCCTTTTAGCCTTTAGCCTTTGAGCCTTTTGGACAGCCTTTCTGAGCCTTTCGTAGCCTTTACCGCAAGCCTTTATAGCCTTTGAAGAGGAGCCTTTATAGCCTTTCAGAAGCCTTTTAAGCCTTTTCGCAGCCTTTTATCAGCCTTTAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTAGCCTTTACAAGCCTTTAGCCTTTAGCCTTTATCAAGCCTTTCTAGCCTTTGAGCCTTTGTGAGCCTTTGTGTCAGCCTTTCAAGCCTTTTTAAGTACAGCCTTTACTCAGCCTTTATAGCCTTTGTCGTAAGCCTTTAGCCTTTAGCCTTTGAAAAGCCTTTACGCACAGACAAGTAGCCTTTCAGCCTTTAAGCCTTTGAGTATGTCCTTGAGCCTTTAAAAGAGCCTTTGGTAGCCTTTAGCCTTTAGCCTTTTATAGCCTTTAAGCCTTTAAGCCTTT&quot;</span>","value":"\"TAACAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTGAGCCTTTGAGCCTTTTAGCCTTTCAGCCTTTAGCCTTTAAGCCTTTCCGCATCGAGCCTTTCAGCCTTTCGTAGCCTTTCGAGCCTTTAGCCTTTCAGCCTTTAGAGCCTTTAAGCCTTTAGTCGATGTAGCCTTTAGCCTTTAGCCTTTAGCCTTTGCAGCCTTTAGTAGGCAAGCCTTTTAGCCTTTGAGCCTTTCGAGCCTTTCTCGCTAGCCTTTAGCCTTTGGTGAGCCTTTTAGCCTTTAGCCTTTTCGCAGCCTTTTGAGCCTTTCTTGTTTGAATGGCAAGAGCCTTTTCGAGCCTTTAGCCTTTGAGCCTTTCAGCCTTTAAAAGCCTTTCGTTAGCCTTTAGCCTTTATCGAGCCTTTAAGCCTTTTATGCAAAGCCTTTGAGCCTTTAGCCTTTCAGCCTTTCAGCCTTTCATTGACAAGCCTTTCAGCCTTTAGCCTTTAGCCTTTCTCAGCCTTTGAGCCTTTGAGCCTTTGTCGAGCCTTTTTTCAGAGCCTTTTAGCCTTTAGCCTTTGAGCCTTTAGCCTTTAGCCTTTTACGAGCCTTTGCAAGCCTTTCAGCCTTTCCAAGCCTTTAGCCTTTGCTTTAGCCTTTCATGGGATAGCCTTTAGCCTTTATTAAGCCTTTTTTATCAAGCCTTTGTAGCCTTTAAGCCTTTCCAGCCTTTAGGAGCCTTTGTATAGCCTTTTGAGCCTTTCTACAGTAAAGCCTTTTTTGGTCAGCCTTTCTAGCCTTTGATAGCCTTTCTGAAGCCTTTGGCGGAGCCTTTCTGTTAACAGCCCAGCCTTTCTCATAGCCTTTGCGGTATCAGCCTTTGCAGCCTTTCTGGAGCGATAGCCTTTCAGCCTTTCCGAGCCTTTTTCAGAGCCTTTGAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTCCGAGCCTTTTCAGCCTTTACAGCCTTTTTAGCCTTTGAGCCTTTCACAGCCTTTGAGCTAGCCTTTAAGTTAAAGCCTTTAGCCTTTCAGCCTTTACATTAGCCTTTTAGCCTTTTCAGCCTTTAGAGCCTTTAGCCTTTGCTGAAGCCTTTAGTAGCCTTTAGCCTTTGCGAAGCCTTTCTGGTGCAACAAGTGAAGCCTTTGCCCTAGCCTTTGCTAGCCTTTCCGAGCCTTTGTCGATATAGCCTTTAGCCTTTAGAAAGCCTTTAGCCTTTGCTAGCCTTTATAGCCTTTAGCCTTTAGCCTTTCCCAGCCTTTAGCCTTTATCCTAAGCCTTTAGCCTTTTCCAGAAGCAGCCTTTTGATCAGAGCCTTTCTTCGGACTGCTCCCAGCCTTTAGCCTTTCAGCCTTTAGCCTTTAGCCTTTCAGCCTTTAGCCTTTTCTAGCCTTTAGCCTTTGTGTAGCCTTTCTAGCCTTTACGAGCCTTTGCCCAGCCTTTCCCAGCCTTTGAGAGCCTTTACCATATAGCCTTTACATAAGCCTTTGATGAGCCTTTAGCCTTTCGAGCCTTTCAGCCTTTACTCCAGCCTTTATAGCCTTTATATAGCCTTTCCTGTTAGGCCGTCGGTGCAGCCTTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTGCTAGCCTTTCCAGCCTTTACAGCCTTTTACCGAGCCTTTCCATCAGCCTTTAGCCTTTATATCTCTGATCGGGTAGCCTTTCGGCTAGCCTTTGGTAGCCTTTTTCAGCCTTTATGTAAAGCCTTTGTAGCCTTTGATGTGAGCCTTTAGAAGCCTTTGTCAGCCTTTTAGCCTTTAGCCTTTAGCCTTTTTAAGCCTTTTACAAGCCTTTACTCAGAGCCTTTACGAGCCTTTTAGCCTTTGCAGCCTTTTAGCCTTTTGATGAGCCTTTGCGGAGCCTTTTCTTTCAGCCTTTTGGCAGCCTTTAGCCTTTGTACAGCCTTTTTGAGCACAGCCTTTCGCGAAAGAGCCTTTATCAGCCTTTAAGCCTTTGCTAGCCTTTAGCCTTTTGAGCCTTTAGCCTTTGTATCTGTCTATCATCGAGCCTTTCTAAGCCTTTGCGGAAGCCTTTAGCCTTTGTCAGCCTTTCAAAGCCTTTAGCCTTTTATTCAAGCCTTTGAACCATAGCCTTTGGCAGCCTTTCAAGCCTTTGACGACAGCCTTTAGCCTTTCATTAGCCTTTTAGGAGGCTCATCCGTCTAGCCTTTAAATAGCCTTTAGCCTTTATAGCCTTTAAGCCTTTAGCCTTTTAAGCCTTTGAGAGCCTTTAAAGCCTTTAAACCAAGCCTTTGCGAAGCCTTTAGCCTTTAGCCTTTCGCAGCCTTTAGCCTTTCGAGCCTTTTGTAGCCTTTTGGAGAGCCTTTGGGCAAGCCTTTAGTATAAGCCTTTAGCCTTTTAGCCTTTCAAGCCTTTAGCCTTTAAGCCTTTGGCACAGCCTTTTGAGCCTTTCAGGAGCCTTTATTGGTGAGCCTTTAGTATAGCCTTTTCAGCCTTTGGCAGCCTTTAATGAAAGCCTTTGCTCAGCCTTTTTCAGCCTTTACAGCCTTTCAAGCCTTTAGCCACAAGCCTTTAGCCTTTAGCCTTTCAGCCTTTGCGAGCCTTTGTAGCCTTTAAGCCTTTCAGCCTTTAGCCTTTAGCCTTTTTCAAGCCTTTTCAGCCTTTCAAAGCCTTTCAAGCCTTTGAAGCCTTTCTAAGCCTTTGAGCCTTTGAGCCTTTGAGCCTTTAGCCTTTGTTCCTAGCCTTTATAGCCTTTTAGGCAGCCTTTCAGAGCCTTTTAAGCCTTTAGCCTTTCAGAAAGAGCCTTTAGCCCAGCCTTTTGATTAGCCTTTAGGGAACAGCCTTTAGCCTTTTAAGCCTTTGGTATACAATCAACGCAGCCTTTAGCCTTTAAGCCTTTTGGAGCCTTTCAGACTGATCCCAGCCTTTCAGCCTTTCTCAGCCTTTAAGCCTTTCTCCAAGCCTTTTGAGCCTTTTCGAGCCTTTAGTGAGCCTTTTGAAGCCTTTGTTTAGCCTTTTGTATAGGGTAGCCTTTAGCCTTTCCGGAAGCCTTTTGTAGCCTTTAAGCCTTTTGTCCGGGAAAGCCTTTGTAAGCCTTTAATGCAGCCTTTCCTATAGCCTTTAAGCCTTTCAGCCTTTTGGAGCCTTTTCTCAGCCTTTAGCCTTTCGCCAGCCTTTCTCCCGAGCAGCCTTTTAGAAAAAGCCTTTTAGCCTTTTACCGTGGACAGCCTTTCACGAGCCTTTACAGGCTAGCCTTTAGCCTTTGCTAGCCTTTTCCCAGCCTTTTGAGCCTTTAAGCCTTTCTAAGTTCTACGCTTGGGCTAAAGCCTTTAGCCTTTAAGCCTTTCAGCCTTTTGCAGCCTTTATATAACTTGAGCCTTTAGCCTTTAGCCTTTATAGCCTTTAGCCTTTTAGCCTTTTATATCCCTTAAGCCTTTGTAAGCCTTTAGCCTTTAAGCCTTTACGAGGAAAGCCTTTCATGCAGCCTTTAGCCTTTAGCCTTTGAGCCTTTCCAGCCTTTCAGCCTTTCAGCCTTTAGCCTTTAGCCTTTAGCCTTTTAGCCTTTATGAGCCTTTATAGCCTTTAGCCTTTTCACCAGCCTTTCCAGATGCACAAGCCTTTCAGCCTTTAGCCTTTCGAGCCTTTGGCTTATAGCCTTTCATCAGCCTTTCTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTTCTAGCCTTTCAGCCTTTAGCCTTTTCGAAGCCTTTAGCCTTTTTAGCCTTTAGCTCAGCCTTTAGCCTTTATCTAACAGCCTTTAGCCTTTAGCCTTTAAAGCCTTTATGTCCAATTCTAACAGCCTTTAGCCTTTAAAGCCTTTGCAGCCTTTGAGCCTTTTAGCCTTTGAAGCCTTTAGCCTTTGTCAGCCTTTCCAGCCTTTTAGCCTTTAGCAGCCTTTAGTACGCCAGCCTTTAGCCTTTGTATAAGCCTTTAGCCTTTAGCCTTTCCACTAGCCTTTAGCCTTTAGAGGAGCGATAGCCTTTCAGCCTTTAGAAAGCCTTTGTTGCTGCTAGCCTTTGGGTTCTCAGCCTTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTTGTAGCCTTTTACATAGGATTGATTCAAAAGCCTTTTTGAGCCTTTCTGCATTAGCCTTTTCCTCTAGCCTTTAGCCTTTCGCAGCCTTTAGCCTTTTAGAGCCTTTAGATAGCCTTTCGCGACAGCCTTTTGTTTAGCCTTTAGCCTTTGTTAGCCTTTGAGCCTTTGAGCCTTTTAGCCTTTCCTAGCCTTTCAGCCTTTCCAAAGCCTTTGACAGGGTGTAGCCTTTCTAGCCTTTTTAGCCTTTAGCCTTTAAACTTAAGCCTTTTTAGCCTTTAGCCTTTCAACCCAGCCTTTAGCCTTTTAAGCCTTTAGCCTTTAGCCTTTTTAGAAGCCTTTTAGCCTTTAGCCTTTGGAGCCTTTCAGATCTCAGCCTTTTCGAGCCTTTTAGCCTTTTCAGAAAAGTAGCCTTTTTAGCAGCCTTTTAAAGCCTTTGGAGCCTTTAGCCTTTAGCCTTTGTAGCCTTTTCCCAAAAGCCTTTACAGCCTTTGTGAGCCTTTTAGTTCGTTTGAGCCTTTCCAGCCTTTCAGCCTTTAGCCTTTATAGCCTTTTGCGAGAAGCCTTTAAGCCTTTAGCCTTTTGACGTTCTAGAGCCTTTGGAGCCTTTCACGCGAGCCTTTCAAGCCTTTGACTCCGCAGCCTTTTCGCGACCAGCCTTTGCCGTGCCAGCCTTTAGCCTTTCAACACAGCCTTTAGCCTTTGGGCCGCAGAGCCTTTGAGTAGCCTTTAGCCTTTGACAGCCTTTAGCCTTTCTAGCCTTTGCAGCCTTTGTCTAGGTAGCCTTTAGCCTTTAGCCTTTCTAGCCTTTTAGCCTTTAGCCTTTTGAGCCTTTTGGAAGCCTTTCAGCCTTTAGCCTTTCGCGAGCCTTTGAGCCTTTACCCAGCCTTTACGGAGCCTTTAGCCTTTCCCATAGCCTTTAGCCTTTCCAGCCTTTAGCCTTTTAGCCTTTCAAATCTAAGCCTTTCGCATATATGGTAGCCTTTAGCCTTTAGCCTTTATGGTCCTTCAGTTTGAGCCTTTTAGAGCCTTTAAAGGAGCCTTTGTAAGACGAAGGTAGCCTTTAGCCTTTGCCAGCCTTTTTAGCCTTTAGCCTTTAAAAAGCCTTTGAGCCTTTAGCCTTTAGCCTTTGAGCCTTTAGCCTTTTCTCCTAGCCTTTCATAGCCTTTGAGCCTTTAGCCTTTTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTGGAGGTCAGCCTTTATGTTAAAGCCTTTAGTTCCCAGCCTTTCAGCCTTTAGCCTTTAGCCTTTGAGCCTTTCAGCCTTTTAGCCTTTCAGCCTTTCAGCCTTTGAAGCCTTTTGTAGCCTTTGCCCGAGCCTTTAGCCTTTAGCCTTTCCCAACCCTGATCCGTAGCCTTTGGGCTGATCCTGAGCCTTTTCAGCCTTTAAGCCTTTAGCCTTTAGCCTTTGAGAAGCCTTTAGCCTTTCAGCCTTTAACAGCCTTTAAGCCTTTATAGCCTTTAGCCAGCCTTTGCAGCCTTTCAGTAGCCTTTAGCCTTTAGCCTTTCTAGCCTTTCTTGGAGCCTTTCCCAGCCTTTAAGAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTAGCCTTTTCGTAGCCTTTGACCATTGTCAGCCTTTCTACTGAGCCTTTCATAGCCTTTTTTAGCCTTTCTAGCAGCCTTTGGAGCCTTTAGAAGAGCCTTTAGCCTTTTAAGCCTTTGAGCCTTTAACACAAGCCTTTATCTGGGCCGCGAGCCTTTTCAACCTAACTACAGCCTTTCTAAGCCTTTAGCCTTTAGCCTTTCAGCCTTTTAGCCTTTACCGAGCCTTTGCGGGAAGCCTTTAAAGAGCCTTTAGAAAAAGCCTTTGGGATAGCCTTTCCAGCCTTTCCAGCCTTTTTAGCCTTTTCCTCAAGATTTAGCCTTTGATGAAGCCTTTGAGCCTTTAGCCTTTCATTGAGCCTTTTAAGCCTTTCAGCCTTTTCTCATCAGCCTTTCACAGCCTTTCTACAGCCTTTAGCCTTTAGCCTTTGGAGCCTTTTCGCCCCGAGCCTTTAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTGTAGCCTTTAGAGCCTTTGCTTAGCCTTTAGCCTTTAGTAGCCTTTAGATAGCCTTTTCTGGGAGCCTTTACAGCCTTTAGCCTTTAGCCTTTAGCCTTTTAAAGCCTTTCCCCAAAGCCTTTGTTGAGCCTTTAGCCTTTACAGTCTAGCCTTTAGCCTTTCAAGCCTTTACCTTAGCCTTTGGCAGCCTTTCTAGCCTTTAGCCTTTTCAGCCTTTAGCCTTTAAGCCTTTAGCCTTTTCGAGCCTTTGAGCCTTTAAGCCTTTATAAAAAGCCTTTAGCCTTTAAGCCTTTACCAGCCTTTAGCCTTTCAGCCTTTTATCGGAAAGCCTTTAAGCCTTTTAGCCTTTCAGCCTTTGAGCCTTTCAGCCTTTAGCCTTTGGCAAAGCCTTTTTGCAGCCTTTGGAAGCCTTTAGCCTTTTTCAAGCCTTTCAGCCTTTAGCCTTTGCACGTATTAGGAAGCCTTTTACTCTAAGCCTTTATCAGCCTTTAGCCTTTAGCCTTTAAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTAGCCTTTACGGTCAGCCTTTGGTAGCCTTTTCAGCCTTTAAGCCTTTAAGCCTTTGAGCCTTTAGCCTTTAGCCTTTGAGCCTTTAAGAGCCTTTCAGCCTTTTTTAGCCTTTTAGCCTTTGAGCCTTTCCTAGCCTTTCAAGCCTTTGAGCCTTTCGAAGCCTTTTAGCCTTTAGCCTTTAGCCTTTATGGAGCCTTTAGCCTTTAGCGGAGCCTTTGAGCCTTTACAGAGCCTTTAGCCTTTAGCCTTTTAAGCCTTTTGCAGCCTTTCAAAGAGCCTTTAGCCTTTACGGAGCCTTTAGCCTTTAAGCCTTTCTCACTAGCCTTTTTAGCCTTTGAGCCTTTATGACGAAGCCTTTAGCCTTTTGTCGTGACCTGAGCCTTTAGCCTTTACAGCCTTTCAGCCTTTAGCCTTTCTTAAAAGCCTTTTAGCCTTTTTGAGCCTTTACAGCCTTTCGAGCCTTTGAGCCTTTCCCAGCCTTTGAAGCCTTTTGGACAGAGCCTTTGCTAGCCTTTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTACTTAGCCTTTTAGCCTTTATGGATAGCCTTTAGCCTTTGAGAGCCTTTGCCTAGCCTTTGAAGCCTTTTTAGCCTTTAACGAGCCTTTAGCCTTTAGCCTTTAGCCTTTAAGCCTTTAGCCTTTCGAGCCTTTCTCAGCCTTTGTAGCCTTTAGCCTTTAGAGCAGCCTTTAGCCTTTCCAGCCTTTAGCCTTTTCAGCCTTTAGCCTTTCAGCCTTTGCCCCGAGCACGTAGCCTTTACAGCCTTTAGCCTTTAGCCTTTTAGCCTTTACAGCCTTTTGAGCCTTTAGCCTTTGAAAGCCTTTTGAAGAGCCTTTCAGCCTTTCTTACTAGCCTTTGCAGCCTTTTAGCCTTTCCGAGCCTTTGATAGCCTTTGTCGGTAAGCCTTTGTAGAGCCTTTAGCCTTTAAGCCTTTGGTAAAGAGCCTTTTCAACAGCCTTTCGGAGCCTTTCGCTACAAGCCTTTTGGCCTAGCCTTTAGCCTTTCAGCCTTTCAAGAGCCTTTAGCCTTTCGCAGCCTTTATAGCCTTTCAGCCTTTCAGCCTTTAGCCTTTAGAGCCTTTGAGCCTTTCGTTATCTAAGCCTTTACTCCATAGCCTTTGAGCCTTTAGCCTTTGTCAGTCGAGCCTTTGTTCTTGAGCCTTTAGCCTTTGCAGCCTTTAGCCTTTTGTTTGTGGAGCCTTTAGCCTTTGAATACAGCCTTTAGCCTTTAGCCTTTAGCCTTTCTAGCCTTTCAGCAGCCTTTGTAGCCTTTGAACCAGCCTTTAGCCTTTTAGCCTTTTCCTTAGCCTTTCCAGCCTTTTAGTGAGCCTTTAGCCTTTGCACCAGCCTTTAGCCTTTAGCCTTTCAGCCTTTAGCCTTTCGAGCCTTTTAGCCTTTGAACAGCCTTTTGAGCCTTTGACGATATGAGCCTTTAGCCTTTTGTAGCCTTTTTTAGCCTTTGAACAGCCTTTGGAGTCAAGCCTTTACGCAGCCTTTCCAGCCTTTCAGCCTTTAGCCTTTGGTCAGCCTTTTCAGAGCCTTTGCGGTTAGCCTTTGAATAGCCTTTAAAGCCTTTCTCAGCCTTTGTAAGCCTTTAGCCTTTTAGCCTTTGTGAGCCTTTCAGCCTTTCCGAGCCTTTAGCCTTTGCCTACGGAAGCCTTTAGCCTTTGCTATCAGCTTGAGCCTTTTAGCCTTTAGTAGCAGCCTTTTAGCCTTTTAGCCTTTCAGCCTTTCTCTAGCCTTTAGCCTTTATCCGAGCCTTTACCAGCCTTTGAGCCTTTAGCCTTTATAGCCTTTATACGTAGCTAGCCTTTAGCCTTTAGAGCCTTTACCCTGTACCAGCCTTTAAGCCTTTCTCGTGAAGCCTTTAGCCTTTGAGCCTTTCGAGCCTTTAGCCTTTAGCCTTTAAGCCTTTTTGTGTGAGCCTTTAGCCTTTGGGGAGCCTTTAGCCTTTCAGCCTTTTAGCCTTTTCAAGCCTTTAGCCTTTAGCCTTTTGAGCCTTTAAAGCCTTTAGCCTTTAGGTAGCAAGCCTTTCGTTATAGCCTTTTATAAGCCTTTTTTAATGAGCCTTTAGCCTTTAGCCTTTGAGCAGCCTTTAGCCTTTAGTAGCCTTTTGATATTAGCCTTTCAGCCTTTAGCCTTTCCCCGAGCCTTTGTTAGAGCCTTTGCAGCCTTTGGAGCCTTTAGCCTTTCGGAGCCTTTAGCCTTTGGGACAGCCTTTAGCCTTTAGCCTTTGAAGCCTTTTGCAGCCTTTAAGATAGCCTTTGAGCCTTTTCAGCCTTTACAGCCTTTAAGCCTTTAGCCTTTGAGCCTTTGAGCCTTTTGAGCCTTTTAGCCTTTGTTGCAGCCTTTAGCCTTTAGCCTTTTAGCCTTTAGCCTTTAGCCTTTGAGCCTTTGAGCCTTTTAGCCTTTAGCCTTTGAGCCTTTTGGACAGCCTTTCTGAGCCTTTCGTAGCCTTTACCGCAAGCCTTTATAGCCTTTGAAGAGGAGCCTTTATAGCCTTTCAGAAGCCTTTTAAGCCTTTTCGCAGCCTTTTATCAGCCTTTAGCCTTTAGCCTTTTAGCCTTTCAGCCTTTAGCCTTTACAAGCCTTTAGCCTTTAGCCTTTATCAAGCCTTTCTAGCCTTTGAGCCTTTGTGAGCCTTTGTGTCAGCCTTTCAAGCCTTTTTAAGTACAGCCTTTACTCAGCCTTTATAGCCTTTGTCGTAAGCCTTTAGCCTTTAGCCTTTGAAAAGCCTTTACGCACAGACAAGTAGCCTTTCAGCCTTTAAGCCTTTGAGTATGTCCTTGAGCCTTTAAAAGAGCCTTTGGTAGCCTTTAGCCTTTAGCCTTTTATAGCCTTTAAGCCTTTAAGCCTTT\""}
;; <=

;; @@
(subs (slurp "/home/azheltov/Downloads/PatternCount.txt") (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 24) (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 15) )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;AGCCTTTAG&quot;</span>","value":"\"AGCCTTTAG\""}
;; <=

;; @@
(count-substring (subs (slurp "/home/azheltov/Downloads/PatternCount.txt") 7 (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 26)) (re-pattern (str "(?=" (subs (slurp "/home/azheltov/Downloads/PatternCount.txt") (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 24) (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 15) ) ")")))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>294</span>","value":"294"}
;; <=

;; @@
(subs (slurp "/home/azheltov/Downloads/PatternCount.txt") (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 15) (- (count (slurp "/home/azheltov/Downloads/PatternCount.txt")) 1) )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;\\r\\nOutput\\r\\n294\\r&quot;</span>","value":"\"\\r\\nOutput\\r\\n294\\r\""}
;; <=

;; @@
(subs (slurp "/home/azheltov/Downloads/dataset_2_6.txt") (- (count (slurp "/home/azheltov/Downloads/dataset_2_6.txt")) 10) (- (count (slurp "/home/azheltov/Downloads/dataset_2_6.txt")) 1) )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;GGGTGTCGG&quot;</span>","value":"\"GGGTGTCGG\""}
;; <=

;; @@
(subs (slurp "/home/azheltov/Downloads/dataset_2_6.txt") 0 (- (count (slurp "/home/azheltov/Downloads/dataset_2_6.txt")) 11))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;CTAGGGTGTCGACTCGGGTGTCGGGTGTCGTGTTACTAGGGTGTCGGGGGTGTCTAGGGGTGTCTTTGGGTGTCGGGTGTCGACTGGGTGTCCTGGGTGTCCGGGGTGTCGGGTGTCCTTGGGTTGGGTGTCGGGTGTCGGGTGTCACCGGGTGTCGCATCTCGGGTGTCGGGTGTCGGTGGGTGTCGGGTGTCGGGTGTCGGGTGTCCAGGGTGTCGGGTGTCGCCGGGTGTCGGGTGTCCGGGTGTCGGGTGTCGGGTGTCGGGTGTCGGCATGGGTGTCAAGCAGGGTGTCTTTTGAGGGTGTCGGGTGTCTCGGGGTGTCAGGGTGTCGGGTGTCGGGTGTCCGGGCTGGGTGTCCGGGTGTCGAGGAGGGTGTCGGGGTGTCCGGGTGTCGTTACGGGTGGGTGTCCGGGTGTCTGGGTGTCGGGTGTCTCCATCAGGGTGTCCGGGTGTCGGGTGTCTTGGTGTGGGGTGTCTGGGTGTCGGGTGTCGCGCCGGGTGTCCGGGGGTGTCTGGGTGTCGCCACGACGGGTGTCGGGTGTCCATGAGGGTGTCCTTAGAGGGGTGTCAGGTGGGTGTCGGGTGTCAGAGTCAGGGTGTCAACGGGTGTCGGGTGTCACGGGTGTCCTGCGACGGGGGTGTCCAGGGGTGTCGGGTGTCCGAGGGTGTCGGGTGTCACAGATGGGTGTCTGAGGGGTGTCGGAGGGGGTGTCGCCGGGTGTCGGGTGTCCTGCCGGGTGTCCTGCTGGGTGTCGGGTGTCGGGTGTCAGGGTGGCTGGGTGTCATCGGGAGGGTGTCAAGACGGGTGTCAATAGGGTGTCCGAGGGTGTCCGGGGTGTCGGGTGTCAGGGTGTCAGGCGGGTGTCGTGGGTGTCTGGGTGTCGCTCCGGGTGTCTCAGGGTGTCGGGTGTCAGGGTGTCCTCATATGGGGTGTCTCGTCGTTCAGGGTGTCAAGGGTGTCCGCCGGGTGTCCGGGTGTCACTAGGGTGTCTTAGGGTGTCGGGGTGTCGGGTGTCGGGTGTCGGGGTGTCTTTCGGGTGTCTAAGAGGGTGTC&quot;</span>","value":"\"CTAGGGTGTCGACTCGGGTGTCGGGTGTCGTGTTACTAGGGTGTCGGGGGTGTCTAGGGGTGTCTTTGGGTGTCGGGTGTCGACTGGGTGTCCTGGGTGTCCGGGGTGTCGGGTGTCCTTGGGTTGGGTGTCGGGTGTCGGGTGTCACCGGGTGTCGCATCTCGGGTGTCGGGTGTCGGTGGGTGTCGGGTGTCGGGTGTCGGGTGTCCAGGGTGTCGGGTGTCGCCGGGTGTCGGGTGTCCGGGTGTCGGGTGTCGGGTGTCGGGTGTCGGCATGGGTGTCAAGCAGGGTGTCTTTTGAGGGTGTCGGGTGTCTCGGGGTGTCAGGGTGTCGGGTGTCGGGTGTCCGGGCTGGGTGTCCGGGTGTCGAGGAGGGTGTCGGGGTGTCCGGGTGTCGTTACGGGTGGGTGTCCGGGTGTCTGGGTGTCGGGTGTCTCCATCAGGGTGTCCGGGTGTCGGGTGTCTTGGTGTGGGGTGTCTGGGTGTCGGGTGTCGCGCCGGGTGTCCGGGGGTGTCTGGGTGTCGCCACGACGGGTGTCGGGTGTCCATGAGGGTGTCCTTAGAGGGGTGTCAGGTGGGTGTCGGGTGTCAGAGTCAGGGTGTCAACGGGTGTCGGGTGTCACGGGTGTCCTGCGACGGGGGTGTCCAGGGGTGTCGGGTGTCCGAGGGTGTCGGGTGTCACAGATGGGTGTCTGAGGGGTGTCGGAGGGGGTGTCGCCGGGTGTCGGGTGTCCTGCCGGGTGTCCTGCTGGGTGTCGGGTGTCGGGTGTCAGGGTGGCTGGGTGTCATCGGGAGGGTGTCAAGACGGGTGTCAATAGGGTGTCCGAGGGTGTCCGGGGTGTCGGGTGTCAGGGTGTCAGGCGGGTGTCGTGGGTGTCTGGGTGTCGCTCCGGGTGTCTCAGGGTGTCGGGTGTCAGGGTGTCCTCATATGGGGTGTCTCGTCGTTCAGGGTGTCAAGGGTGTCCGCCGGGTGTCCGGGTGTCACTAGGGTGTCTTAGGGTGTCGGGGTGTCGGGTGTCGGGTGTCGGGGTGTCTTTCGGGTGTCTAAGAGGGTGTC\""}
;; <=

;; @@
(slurp "/home/azheltov/Downloads/dataset_2_6.txt")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;CTAGGGTGTCGACTCGGGTGTCGGGTGTCGTGTTACTAGGGTGTCGGGGGTGTCTAGGGGTGTCTTTGGGTGTCGGGTGTCGACTGGGTGTCCTGGGTGTCCGGGGTGTCGGGTGTCCTTGGGTTGGGTGTCGGGTGTCGGGTGTCACCGGGTGTCGCATCTCGGGTGTCGGGTGTCGGTGGGTGTCGGGTGTCGGGTGTCGGGTGTCCAGGGTGTCGGGTGTCGCCGGGTGTCGGGTGTCCGGGTGTCGGGTGTCGGGTGTCGGGTGTCGGCATGGGTGTCAAGCAGGGTGTCTTTTGAGGGTGTCGGGTGTCTCGGGGTGTCAGGGTGTCGGGTGTCGGGTGTCCGGGCTGGGTGTCCGGGTGTCGAGGAGGGTGTCGGGGTGTCCGGGTGTCGTTACGGGTGGGTGTCCGGGTGTCTGGGTGTCGGGTGTCTCCATCAGGGTGTCCGGGTGTCGGGTGTCTTGGTGTGGGGTGTCTGGGTGTCGGGTGTCGCGCCGGGTGTCCGGGGGTGTCTGGGTGTCGCCACGACGGGTGTCGGGTGTCCATGAGGGTGTCCTTAGAGGGGTGTCAGGTGGGTGTCGGGTGTCAGAGTCAGGGTGTCAACGGGTGTCGGGTGTCACGGGTGTCCTGCGACGGGGGTGTCCAGGGGTGTCGGGTGTCCGAGGGTGTCGGGTGTCACAGATGGGTGTCTGAGGGGTGTCGGAGGGGGTGTCGCCGGGTGTCGGGTGTCCTGCCGGGTGTCCTGCTGGGTGTCGGGTGTCGGGTGTCAGGGTGGCTGGGTGTCATCGGGAGGGTGTCAAGACGGGTGTCAATAGGGTGTCCGAGGGTGTCCGGGGTGTCGGGTGTCAGGGTGTCAGGCGGGTGTCGTGGGTGTCTGGGTGTCGCTCCGGGTGTCTCAGGGTGTCGGGTGTCAGGGTGTCCTCATATGGGGTGTCTCGTCGTTCAGGGTGTCAAGGGTGTCCGCCGGGTGTCCGGGTGTCACTAGGGTGTCTTAGGGTGTCGGGGTGTCGGGTGTCGGGTGTCGGGGTGTCTTTCGGGTGTCTAAGAGGGTGTC\\nGGGTGTCGG\\n&quot;</span>","value":"\"CTAGGGTGTCGACTCGGGTGTCGGGTGTCGTGTTACTAGGGTGTCGGGGGTGTCTAGGGGTGTCTTTGGGTGTCGGGTGTCGACTGGGTGTCCTGGGTGTCCGGGGTGTCGGGTGTCCTTGGGTTGGGTGTCGGGTGTCGGGTGTCACCGGGTGTCGCATCTCGGGTGTCGGGTGTCGGTGGGTGTCGGGTGTCGGGTGTCGGGTGTCCAGGGTGTCGGGTGTCGCCGGGTGTCGGGTGTCCGGGTGTCGGGTGTCGGGTGTCGGGTGTCGGCATGGGTGTCAAGCAGGGTGTCTTTTGAGGGTGTCGGGTGTCTCGGGGTGTCAGGGTGTCGGGTGTCGGGTGTCCGGGCTGGGTGTCCGGGTGTCGAGGAGGGTGTCGGGGTGTCCGGGTGTCGTTACGGGTGGGTGTCCGGGTGTCTGGGTGTCGGGTGTCTCCATCAGGGTGTCCGGGTGTCGGGTGTCTTGGTGTGGGGTGTCTGGGTGTCGGGTGTCGCGCCGGGTGTCCGGGGGTGTCTGGGTGTCGCCACGACGGGTGTCGGGTGTCCATGAGGGTGTCCTTAGAGGGGTGTCAGGTGGGTGTCGGGTGTCAGAGTCAGGGTGTCAACGGGTGTCGGGTGTCACGGGTGTCCTGCGACGGGGGTGTCCAGGGGTGTCGGGTGTCCGAGGGTGTCGGGTGTCACAGATGGGTGTCTGAGGGGTGTCGGAGGGGGTGTCGCCGGGTGTCGGGTGTCCTGCCGGGTGTCCTGCTGGGTGTCGGGTGTCGGGTGTCAGGGTGGCTGGGTGTCATCGGGAGGGTGTCAAGACGGGTGTCAATAGGGTGTCCGAGGGTGTCCGGGGTGTCGGGTGTCAGGGTGTCAGGCGGGTGTCGTGGGTGTCTGGGTGTCGCTCCGGGTGTCTCAGGGTGTCGGGTGTCAGGGTGTCCTCATATGGGGTGTCTCGTCGTTCAGGGTGTCAAGGGTGTCCGCCGGGTGTCCGGGTGTCACTAGGGTGTCTTAGGGTGTCGGGGTGTCGGGTGTCGGGTGTCGGGGTGTCTTTCGGGTGTCTAAGAGGGTGTC\\nGGGTGTCGG\\n\""}
;; <=

;; @@
(count-substring (subs (slurp "/home/azheltov/Downloads/dataset_2_6.txt") 0 (- (count (slurp "/home/azheltov/Downloads/dataset_2_6.txt")) 11)) (str "(?=" (subs (slurp "/home/azheltov/Downloads/dataset_2_6.txt") (- (count (slurp "/home/azheltov/Downloads/dataset_2_6.txt")) 10) (- (count (slurp "/home/azheltov/Downloads/dataset_2_6.txt")) 1) ) ")"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>39</span>","value":"39"}
;; <=

;; @@
(def txt (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/txt</span>","value":"#'user/txt"}
;; <=

;; @@
()
;; @@

;; @@
(subs txt 2 5)
(count-substring txt (str "(?="  (subs txt 2 5) ")"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>22080</span>","value":"22080"}
;; <=

;; @@
(set (partition 3 1 txt))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"},{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"},{"type":"html","content":"<span class='clj-char'>\\A</span>","value":"\\A"}],"value":"(\\C \\C \\A)"}
;; <=

;; @@
(set (partition 3 1 "fgdfhfjfgj"))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\j</span>","value":"\\j"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"}],"value":"(\\f \\j \\f)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\h</span>","value":"\\h"}],"value":"(\\d \\f \\h)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\g</span>","value":"\\g"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"}],"value":"(\\f \\g \\d)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\h</span>","value":"\\h"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"}],"value":"(\\f \\h \\f)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\h</span>","value":"\\h"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\j</span>","value":"\\j"}],"value":"(\\h \\f \\j)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\g</span>","value":"\\g"},{"type":"html","content":"<span class='clj-char'>\\j</span>","value":"\\j"}],"value":"(\\f \\g \\j)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\j</span>","value":"\\j"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"},{"type":"html","content":"<span class='clj-char'>\\g</span>","value":"\\g"}],"value":"(\\j \\f \\g)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\g</span>","value":"\\g"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"},{"type":"html","content":"<span class='clj-char'>\\f</span>","value":"\\f"}],"value":"(\\g \\d \\f)"}],"value":"#{(\\f \\j \\f) (\\d \\f \\h) (\\f \\g \\d) (\\f \\h \\f) (\\h \\f \\j) (\\f \\g \\j) (\\j \\f \\g) (\\g \\d \\f)}"}
;; <=

;; @@
(require '[clojure.string :as str])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(str/split "gdfhfd" #"((?<=.).(?=.))")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;g&quot;</span>","value":"\"g\""},{"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""},{"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""},{"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""},{"type":"html","content":"<span class='clj-string'>&quot;d&quot;</span>","value":"\"d\""}],"value":"[\"g\" \"\" \"\" \"\" \"d\"]"}
;; <=

;; @@
(clojure.string/replace "gdfhfd" #"((?<=.).(?=.))" "$1#$1")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;gd#df#fh#hf#fd&quot;</span>","value":"\"gd#df#fh#hf#fd\""}
;; <=

;; @@
(map #(count-substring txt (str "(?=" % ")")) (set (clojure.string/split (clojure.string/replace "gdfhfd" #"(?=(\w{3}))." "$1#") #"#")))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"(0 0 0 0 0)"}
;; <=

;; @@
(filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace "gdfhfd" #"(?=(\w{3}))." "$1#") #"#")))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;fhf&quot;</span>","value":"\"fhf\""},{"type":"html","content":"<span class='clj-string'>&quot;gdf&quot;</span>","value":"\"gdf\""},{"type":"html","content":"<span class='clj-string'>&quot;dfh&quot;</span>","value":"\"dfh\""},{"type":"html","content":"<span class='clj-string'>&quot;hfd&quot;</span>","value":"\"hfd\""}],"value":"(\"fhf\" \"gdf\" \"dfh\" \"hfd\")"}
;; <=

;; @@
(set (clojure.string/split (clojure.string/replace "gdfhfd" #"(?=(\w{3}))." "$1#") #"#"))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;fhf&quot;</span>","value":"\"fhf\""},{"type":"html","content":"<span class='clj-string'>&quot;gdf&quot;</span>","value":"\"gdf\""},{"type":"html","content":"<span class='clj-string'>&quot;fd&quot;</span>","value":"\"fd\""},{"type":"html","content":"<span class='clj-string'>&quot;dfh&quot;</span>","value":"\"dfh\""},{"type":"html","content":"<span class='clj-string'>&quot;hfd&quot;</span>","value":"\"hfd\""}],"value":"#{\"fhf\" \"gdf\" \"fd\" \"dfh\" \"hfd\"}"}
;; <=

;; @@
(filter #(= (count %) 3) #{"fhf" "gdf" "fd" "dfh" "hfd"})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;fhf&quot;</span>","value":"\"fhf\""},{"type":"html","content":"<span class='clj-string'>&quot;gdf&quot;</span>","value":"\"gdf\""},{"type":"html","content":"<span class='clj-string'>&quot;dfh&quot;</span>","value":"\"dfh\""},{"type":"html","content":"<span class='clj-string'>&quot;hfd&quot;</span>","value":"\"hfd\""}],"value":"(\"fhf\" \"gdf\" \"dfh\" \"hfd\")"}
;; <=

;; @@
(filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#")))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;CTG&quot;</span>","value":"\"CTG\""},{"type":"html","content":"<span class='clj-string'>&quot;GAG&quot;</span>","value":"\"GAG\""},{"type":"html","content":"<span class='clj-string'>&quot;CTC&quot;</span>","value":"\"CTC\""},{"type":"html","content":"<span class='clj-string'>&quot;TGT&quot;</span>","value":"\"TGT\""},{"type":"html","content":"<span class='clj-string'>&quot;GTG&quot;</span>","value":"\"GTG\""},{"type":"html","content":"<span class='clj-string'>&quot;TCA&quot;</span>","value":"\"TCA\""},{"type":"html","content":"<span class='clj-string'>&quot;CAA&quot;</span>","value":"\"CAA\""},{"type":"html","content":"<span class='clj-string'>&quot;AGC&quot;</span>","value":"\"AGC\""},{"type":"html","content":"<span class='clj-string'>&quot;GAA&quot;</span>","value":"\"GAA\""},{"type":"html","content":"<span class='clj-string'>&quot;TCG&quot;</span>","value":"\"TCG\""},{"type":"html","content":"<span class='clj-string'>&quot;ACA&quot;</span>","value":"\"ACA\""},{"type":"html","content":"<span class='clj-string'>&quot;TGC&quot;</span>","value":"\"TGC\""},{"type":"html","content":"<span class='clj-string'>&quot;GCC&quot;</span>","value":"\"GCC\""},{"type":"html","content":"<span class='clj-string'>&quot;CTT&quot;</span>","value":"\"CTT\""},{"type":"html","content":"<span class='clj-string'>&quot;TTC&quot;</span>","value":"\"TTC\""},{"type":"html","content":"<span class='clj-string'>&quot;GAT&quot;</span>","value":"\"GAT\""},{"type":"html","content":"<span class='clj-string'>&quot;GGC&quot;</span>","value":"\"GGC\""},{"type":"html","content":"<span class='clj-string'>&quot;GCT&quot;</span>","value":"\"GCT\""},{"type":"html","content":"<span class='clj-string'>&quot;CTA&quot;</span>","value":"\"CTA\""},{"type":"html","content":"<span class='clj-string'>&quot;CGC&quot;</span>","value":"\"CGC\""},{"type":"html","content":"<span class='clj-string'>&quot;TAG&quot;</span>","value":"\"TAG\""},{"type":"html","content":"<span class='clj-string'>&quot;TAC&quot;</span>","value":"\"TAC\""},{"type":"html","content":"<span class='clj-string'>&quot;TAA&quot;</span>","value":"\"TAA\""},{"type":"html","content":"<span class='clj-string'>&quot;ACC&quot;</span>","value":"\"ACC\""},{"type":"html","content":"<span class='clj-string'>&quot;CGT&quot;</span>","value":"\"CGT\""},{"type":"html","content":"<span class='clj-string'>&quot;ATC&quot;</span>","value":"\"ATC\""},{"type":"html","content":"<span class='clj-string'>&quot;GGG&quot;</span>","value":"\"GGG\""},{"type":"html","content":"<span class='clj-string'>&quot;CGG&quot;</span>","value":"\"CGG\""},{"type":"html","content":"<span class='clj-string'>&quot;CCT&quot;</span>","value":"\"CCT\""},{"type":"html","content":"<span class='clj-string'>&quot;CAG&quot;</span>","value":"\"CAG\""},{"type":"html","content":"<span class='clj-string'>&quot;AAC&quot;</span>","value":"\"AAC\""},{"type":"html","content":"<span class='clj-string'>&quot;ACG&quot;</span>","value":"\"ACG\""},{"type":"html","content":"<span class='clj-string'>&quot;ACT&quot;</span>","value":"\"ACT\""},{"type":"html","content":"<span class='clj-string'>&quot;TTT&quot;</span>","value":"\"TTT\""},{"type":"html","content":"<span class='clj-string'>&quot;TTG&quot;</span>","value":"\"TTG\""},{"type":"html","content":"<span class='clj-string'>&quot;AAG&quot;</span>","value":"\"AAG\""},{"type":"html","content":"<span class='clj-string'>&quot;TAT&quot;</span>","value":"\"TAT\""},{"type":"html","content":"<span class='clj-string'>&quot;TGG&quot;</span>","value":"\"TGG\""},{"type":"html","content":"<span class='clj-string'>&quot;AGT&quot;</span>","value":"\"AGT\""},{"type":"html","content":"<span class='clj-string'>&quot;ATG&quot;</span>","value":"\"ATG\""},{"type":"html","content":"<span class='clj-string'>&quot;CGA&quot;</span>","value":"\"CGA\""},{"type":"html","content":"<span class='clj-string'>&quot;CCA&quot;</span>","value":"\"CCA\""},{"type":"html","content":"<span class='clj-string'>&quot;TCC&quot;</span>","value":"\"TCC\""},{"type":"html","content":"<span class='clj-string'>&quot;TCT&quot;</span>","value":"\"TCT\""},{"type":"html","content":"<span class='clj-string'>&quot;GTT&quot;</span>","value":"\"GTT\""},{"type":"html","content":"<span class='clj-string'>&quot;CCC&quot;</span>","value":"\"CCC\""},{"type":"html","content":"<span class='clj-string'>&quot;CAC&quot;</span>","value":"\"CAC\""},{"type":"html","content":"<span class='clj-string'>&quot;AGA&quot;</span>","value":"\"AGA\""},{"type":"html","content":"<span class='clj-string'>&quot;ATA&quot;</span>","value":"\"ATA\""},{"type":"html","content":"<span class='clj-string'>&quot;GTA&quot;</span>","value":"\"GTA\""},{"type":"html","content":"<span class='clj-string'>&quot;GCG&quot;</span>","value":"\"GCG\""},{"type":"html","content":"<span class='clj-string'>&quot;GTC&quot;</span>","value":"\"GTC\""},{"type":"html","content":"<span class='clj-string'>&quot;AA\\n&quot;</span>","value":"\"AA\\n\""},{"type":"html","content":"<span class='clj-string'>&quot;GAC&quot;</span>","value":"\"GAC\""},{"type":"html","content":"<span class='clj-string'>&quot;TTA&quot;</span>","value":"\"TTA\""},{"type":"html","content":"<span class='clj-string'>&quot;CCG&quot;</span>","value":"\"CCG\""},{"type":"html","content":"<span class='clj-string'>&quot;GGA&quot;</span>","value":"\"GGA\""},{"type":"html","content":"<span class='clj-string'>&quot;GGT&quot;</span>","value":"\"GGT\""},{"type":"html","content":"<span class='clj-string'>&quot;AGG&quot;</span>","value":"\"AGG\""},{"type":"html","content":"<span class='clj-string'>&quot;TGA&quot;</span>","value":"\"TGA\""},{"type":"html","content":"<span class='clj-string'>&quot;ATT&quot;</span>","value":"\"ATT\""},{"type":"html","content":"<span class='clj-string'>&quot;AAA&quot;</span>","value":"\"AAA\""},{"type":"html","content":"<span class='clj-string'>&quot;AAT&quot;</span>","value":"\"AAT\""},{"type":"html","content":"<span class='clj-string'>&quot;CAT&quot;</span>","value":"\"CAT\""},{"type":"html","content":"<span class='clj-string'>&quot;GCA&quot;</span>","value":"\"GCA\""}],"value":"(\"CTG\" \"GAG\" \"CTC\" \"TGT\" \"GTG\" \"TCA\" \"CAA\" \"AGC\" \"GAA\" \"TCG\" \"ACA\" \"TGC\" \"GCC\" \"CTT\" \"TTC\" \"GAT\" \"GGC\" \"GCT\" \"CTA\" \"CGC\" \"TAG\" \"TAC\" \"TAA\" \"ACC\" \"CGT\" \"ATC\" \"GGG\" \"CGG\" \"CCT\" \"CAG\" \"AAC\" \"ACG\" \"ACT\" \"TTT\" \"TTG\" \"AAG\" \"TAT\" \"TGG\" \"AGT\" \"ATG\" \"CGA\" \"CCA\" \"TCC\" \"TCT\" \"GTT\" \"CCC\" \"CAC\" \"AGA\" \"ATA\" \"GTA\" \"GCG\" \"GTC\" \"AA\\n\" \"GAC\" \"TTA\" \"CCG\" \"GGA\" \"GGT\" \"AGG\" \"TGA\" \"ATT\" \"AAA\" \"AAT\" \"CAT\" \"GCA\")"}
;; <=

;; @@
(map #(count-substring txt (str "(?=" % ")")) (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#"))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>17690</span>","value":"17690"},{"type":"html","content":"<span class='clj-unkown'>13431</span>","value":"13431"},{"type":"html","content":"<span class='clj-unkown'>14441</span>","value":"14441"},{"type":"html","content":"<span class='clj-unkown'>15369</span>","value":"15369"},{"type":"html","content":"<span class='clj-unkown'>16556</span>","value":"16556"},{"type":"html","content":"<span class='clj-unkown'>23447</span>","value":"23447"},{"type":"html","content":"<span class='clj-unkown'>26912</span>","value":"26912"},{"type":"html","content":"<span class='clj-unkown'>20678</span>","value":"20678"},{"type":"html","content":"<span class='clj-unkown'>18157</span>","value":"18157"},{"type":"html","content":"<span class='clj-unkown'>15802</span>","value":"15802"},{"type":"html","content":"<span class='clj-unkown'>16327</span>","value":"16327"},{"type":"html","content":"<span class='clj-unkown'>21219</span>","value":"21219"},{"type":"html","content":"<span class='clj-unkown'>17354</span>","value":"17354"},{"type":"html","content":"<span class='clj-unkown'>21188</span>","value":"21188"},{"type":"html","content":"<span class='clj-unkown'>19294</span>","value":"19294"},{"type":"html","content":"<span class='clj-unkown'>21397</span>","value":"21397"},{"type":"html","content":"<span class='clj-unkown'>17414</span>","value":"17414"},{"type":"html","content":"<span class='clj-unkown'>21071</span>","value":"21071"},{"type":"html","content":"<span class='clj-unkown'>10033</span>","value":"10033"},{"type":"html","content":"<span class='clj-unkown'>20580</span>","value":"20580"},{"type":"html","content":"<span class='clj-unkown'>9756</span>","value":"9756"},{"type":"html","content":"<span class='clj-unkown'>12566</span>","value":"12566"},{"type":"html","content":"<span class='clj-unkown'>17742</span>","value":"17742"},{"type":"html","content":"<span class='clj-unkown'>17012</span>","value":"17012"},{"type":"html","content":"<span class='clj-unkown'>13225</span>","value":"13225"},{"type":"html","content":"<span class='clj-unkown'>21259</span>","value":"21259"},{"type":"html","content":"<span class='clj-unkown'>9523</span>","value":"9523"},{"type":"html","content":"<span class='clj-unkown'>12911</span>","value":"12911"},{"type":"html","content":"<span class='clj-unkown'>11206</span>","value":"11206"},{"type":"html","content":"<span class='clj-unkown'>17254</span>","value":"17254"},{"type":"html","content":"<span class='clj-unkown'>20905</span>","value":"20905"},{"type":"html","content":"<span class='clj-unkown'>13492</span>","value":"13492"},{"type":"html","content":"<span class='clj-unkown'>16028</span>","value":"16028"},{"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"},{"type":"html","content":"<span class='clj-unkown'>26868</span>","value":"26868"},{"type":"html","content":"<span class='clj-unkown'>19825</span>","value":"19825"},{"type":"html","content":"<span class='clj-unkown'>14272</span>","value":"14272"},{"type":"html","content":"<span class='clj-unkown'>19920</span>","value":"19920"},{"type":"html","content":"<span class='clj-unkown'>14476</span>","value":"14476"},{"type":"html","content":"<span class='clj-unkown'>18746</span>","value":"18746"},{"type":"html","content":"<span class='clj-unkown'>15793</span>","value":"15793"},{"type":"html","content":"<span class='clj-unkown'>21063</span>","value":"21063"},{"type":"html","content":"<span class='clj-unkown'>11028</span>","value":"11028"},{"type":"html","content":"<span class='clj-unkown'>15047</span>","value":"15047"},{"type":"html","content":"<span class='clj-unkown'>19768</span>","value":"19768"},{"type":"html","content":"<span class='clj-unkown'>10105</span>","value":"10105"},{"type":"html","content":"<span class='clj-unkown'>18397</span>","value":"18397"},{"type":"html","content":"<span class='clj-unkown'>14077</span>","value":"14077"},{"type":"html","content":"<span class='clj-unkown'>15060</span>","value":"15060"},{"type":"html","content":"<span class='clj-unkown'>12115</span>","value":"12115"},{"type":"html","content":"<span class='clj-unkown'>20090</span>","value":"20090"},{"type":"html","content":"<span class='clj-unkown'>10330</span>","value":"10330"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>10990</span>","value":"10990"},{"type":"html","content":"<span class='clj-unkown'>17128</span>","value":"17128"},{"type":"html","content":"<span class='clj-unkown'>13125</span>","value":"13125"},{"type":"html","content":"<span class='clj-unkown'>10753</span>","value":"10753"},{"type":"html","content":"<span class='clj-unkown'>15699</span>","value":"15699"},{"type":"html","content":"<span class='clj-unkown'>11035</span>","value":"11035"},{"type":"html","content":"<span class='clj-unkown'>23352</span>","value":"23352"},{"type":"html","content":"<span class='clj-unkown'>22334</span>","value":"22334"},{"type":"html","content":"<span class='clj-unkown'>30606</span>","value":"30606"},{"type":"html","content":"<span class='clj-unkown'>22080</span>","value":"22080"},{"type":"html","content":"<span class='clj-unkown'>19650</span>","value":"19650"},{"type":"html","content":"<span class='clj-unkown'>21376</span>","value":"21376"}],"value":"(17690 13431 14441 15369 16556 23447 26912 20678 18157 15802 16327 21219 17354 21188 19294 21397 17414 21071 10033 20580 9756 12566 17742 17012 13225 21259 9523 12911 11206 17254 20905 13492 16028 31901 26868 19825 14272 19920 14476 18746 15793 21063 11028 15047 19768 10105 18397 14077 15060 12115 20090 10330 1 10990 17128 13125 10753 15699 11035 23352 22334 30606 22080 19650 21376)"}
;; <=

;; @@
(apply max (map #(count-substring txt (str "(?=" % ")")) (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#")))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"}
;; <=

;; @@
(def mytmp (zipmap (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#"))) (map #(count-substring txt (str "(?=" % ")")) (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#")))) ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/mytmp</span>","value":"#'user/mytmp"}
;; <=

;; @@
(val (apply max-key val mytmp))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"}
;; <=

;; @@
(filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;TTT&quot;</span>","value":"\"TTT\""}],"value":"(\"TTT\")"}
;; <=

;; @@
(count (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}
;; <=

;; @@
(map list (repeat (count (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp))) (val (apply max-key val mytmp))) (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"},{"type":"html","content":"<span class='clj-string'>&quot;TTT&quot;</span>","value":"\"TTT\""}],"value":"(31901 \"TTT\")"}],"value":"((31901 \"TTT\"))"}
;; <=

;; @@
#{(val (apply max-key val mytmp))}
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"}],"value":"#{31901}"}
;; <=

;; @@
(def hm {:foo "bar" :fooo "bar"})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/hm</span>","value":"#'user/hm"}
;; <=

;; @@
(filter (comp #{"bar"} hm) (keys hm))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:foo</span>","value":":foo"},{"type":"html","content":"<span class='clj-keyword'>:fooo</span>","value":":fooo"}],"value":"(:foo :fooo)"}
;; <=

;; @@
(apply max-key val {:a 3 :b 11 :c 11 :d 9})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:c</span>","value":":c"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"}],"value":"[:c 11]"}
;; <=

;; @@
(zipmap '(1 2 3) '(4 5 6))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[1 4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[2 5]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[3 6]"}],"value":"{1 4, 2 5, 3 6}"}
;; <=

;; @@
(apply max '(1 2 3 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}
;; <=

;; @@
(def tmp (apply zipmap '(("gd" "as" "op") ("1" "2" "3"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/tmp</span>","value":"#'user/tmp"}
;; <=

;; @@
(keys tmp)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(&quot;gd&quot; &quot;as&quot; &quot;op&quot;)</span>","value":"(\"gd\" \"as\" \"op\")"}
;; <=

;; @@
(zipmap [:a :b :c] [1 2 3 4])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:a</span>","value":":a"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:a 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:b</span>","value":":b"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[:b 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:c</span>","value":":c"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[:c 3]"}],"value":"{:a 1, :b 2, :c 3}"}
;; <=

;; @@
(def mytmp (zipmap (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#"))) (map #(count-substring txt (str "(?=" % ")")) (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt #"(?=(\w{3}))." "$1#") #"#")))) ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/mytmp</span>","value":"#'user/mytmp"}
;; <=

;; @@
(re-pattern (str "(?=(\\w{" "5" "}))."))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&quot;(?=(\\w{5})).&quot;</span>","value":"#\"(?=(\\w{5})).\""}
;; <=

;; @@
 (map list (repeat (count (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp))) (val (apply max-key val mytmp))) (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"},{"type":"html","content":"<span class='clj-string'>&quot;TTT&quot;</span>","value":"\"TTT\""}],"value":"(31901 \"TTT\")"}],"value":"((31901 \"TTT\"))"}
;; <=

;; @@
(def mytmp2 (zipmap (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" 3 "})).")) "$1#") #"#"))) (map #(count-substring txt (str "(?=" % ")")) (filter #(= (count %) 3) (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" 3 "})).")) "$1#") #"#")))) ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/mytmp2</span>","value":"#'user/mytmp2"}
;; <=

;; @@
(defn kmers [txt k]
  (let [mytmp 
        (zipmap
          (filter 
            #(= (count %) k) 
            (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))
          (map #(count-substring txt (str "(?=" % ")"))
               (filter 
                 #(= (count %) k) 
                 (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))) )]
    (map list (repeat 
                (count (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp))) 
                (val (apply max-key val mytmp)))
         (filter (comp #{(val (apply max-key val mytmp))} mytmp) (keys mytmp)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/kmers</span>","value":"#'user/kmers"}
;; <=

;; @@
(time (kmers txt 6))
;; @@
;; ->
;;; &quot;Elapsed time: 274286.833943 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>866</span>","value":"866"},{"type":"html","content":"<span class='clj-string'>&quot;ATTTTT&quot;</span>","value":"\"ATTTTT\""}],"value":"(866 \"ATTTTT\")"}],"value":"((866 \"ATTTTT\"))"}
;; <=

;; @@
(defn kmers [txt k]
  "Kmer function"
  (let [theset 
        (filter 
            #(= (count %) k) 
                 (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))
        mytmp 
        (zipmap
          theset
          (pmap #(count-substring txt (str "(?=" % ")"))
               theset) )]
    (let [apmax (val (apply max-key val mytmp)) thefil (filter (comp #{apmax} mytmp) (keys mytmp))]
    (pmap list (repeat 
                (count thefil) 
                apmax)
         thefil))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/kmers</span>","value":"#'user/kmers"}
;; <=

;; @@
(time (kmers txt 1))
;; @@
;; ->
;;; &quot;Elapsed time: 1974.330533 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>294711</span>","value":"294711"},{"type":"html","content":"<span class='clj-string'>&quot;T&quot;</span>","value":"\"T\""}],"value":"(294711 \"T\")"}],"value":"((294711 \"T\"))"}
;; <=

;; @@
(require '[clojure.core.reducers :as r])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(r/fold + (r/filter even? (r/map inc [1 1 1 2])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}
;; <=

;; @@
(defn kmers-parallel [txt k]
  "Kmer function"
  (let [theset 
        (filter 
            #(= (count %) k) 
                 (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))
        mytmp 
        (zipmap
          theset
          (pmap #(count-substring txt (str "(?=" % ")"))
               theset) )]
    (let [apmax (val (apply max-key val mytmp)) thefil (filter (comp #{apmax} mytmp) (keys mytmp))]
    (pmap list (repeat 
                (count thefil) 
                apmax)
         thefil))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/kmers-parallel</span>","value":"#'user/kmers-parallel"}
;; <=

;; @@
(time (kmers-parallel txt 1))
;; @@
;; ->
;;; &quot;Elapsed time: 1986.10627 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>294711</span>","value":"294711"},{"type":"html","content":"<span class='clj-string'>&quot;T&quot;</span>","value":"\"T\""}],"value":"(294711 \"T\")"}],"value":"((294711 \"T\"))"}
;; <=

;; @@
6497.35904
;; @@

;; @@
#{4}
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"#{4}"}
;; <=

;; @@
(for [i [1 2 3 4 5 6 7 8]]
  (kmers-parallel txt i))
;; @@

;; @@
(re-pattern (str "(?=\\w{" 2 "})"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&quot;(?=\\w{2})&quot;</span>","value":"#\"(?=\\w{2})\""}
;; <=

;; @@
(defn count-substring [txt sub]
  "Function to count occuriencies of substring in string with regular expressions"
  (count (re-seq (re-pattern sub) txt)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/count-substring</span>","value":"#'user/count-substring"}
;; <=

;; @@
(defn kmers-parallel [txt k]
  "The most freuet kmer function"
  (let [theset 
        (filter 
            #(= (count %) k) 
                 (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))
        mytmp 
        (zipmap
          theset
          (pmap #(count-substring txt (str "(?=" % ")"))
               theset) )]
    (let [apmax (val (apply max-key val mytmp)) thefil (filter (comp #{apmax} mytmp) (keys mytmp))]
    (pmap list (repeat 
                (count thefil) 
                apmax)
         thefil))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/kmers-parallel</span>","value":"#'user/kmers-parallel"}
;; <=

;; @@
(def txt (slurp "/home/azheltov/Downloads/Vibrio_cholerae.txt"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/txt</span>","value":"#'user/txt"}
;; <=

;; @@
(def answer (for [i [1 2 3 4 5 6 7 8]]
  (conj [i] (kmers-parallel txt i))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/answer</span>","value":"#'user/answer"}
;; <=

;; @@
(kmers-parallel txt 9)
;; @@

;; **
;;; ### A lot of T found in Vibrio Cholerae DNA. What does that mean?
;; **

;; @@
answer
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>294711</span>","value":"294711"},{"type":"html","content":"<span class='clj-string'>&quot;T&quot;</span>","value":"\"T\""}],"value":"(294711 \"T\")"}],"value":"((294711 \"T\"))"}],"value":"[1 ((294711 \"T\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>95191</span>","value":"95191"},{"type":"html","content":"<span class='clj-string'>&quot;TT&quot;</span>","value":"\"TT\""}],"value":"(95191 \"TT\")"}],"value":"((95191 \"TT\"))"}],"value":"[2 ((95191 \"TT\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>31901</span>","value":"31901"},{"type":"html","content":"<span class='clj-string'>&quot;TTT&quot;</span>","value":"\"TTT\""}],"value":"(31901 \"TTT\")"}],"value":"((31901 \"TTT\"))"}],"value":"[3 ((31901 \"TTT\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>10206</span>","value":"10206"},{"type":"html","content":"<span class='clj-string'>&quot;TTTT&quot;</span>","value":"\"TTTT\""}],"value":"(10206 \"TTTT\")"}],"value":"((10206 \"TTTT\"))"}],"value":"[4 ((10206 \"TTTT\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>3193</span>","value":"3193"},{"type":"html","content":"<span class='clj-string'>&quot;TTTTT&quot;</span>","value":"\"TTTTT\""}],"value":"(3193 \"TTTTT\")"}],"value":"((3193 \"TTTTT\"))"}],"value":"[5 ((3193 \"TTTTT\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>866</span>","value":"866"},{"type":"html","content":"<span class='clj-string'>&quot;ATTTTT&quot;</span>","value":"\"ATTTTT\""}],"value":"(866 \"ATTTTT\")"}],"value":"((866 \"ATTTTT\"))"}],"value":"[6 ((866 \"ATTTTT\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>326</span>","value":"326"},{"type":"html","content":"<span class='clj-string'>&quot;AAACTCA&quot;</span>","value":"\"AAACTCA\""}],"value":"(326 \"AAACTCA\")"}],"value":"((326 \"AAACTCA\"))"}],"value":"[7 ((326 \"AAACTCA\"))]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>183</span>","value":"183"},{"type":"html","content":"<span class='clj-string'>&quot;AAACTCAA&quot;</span>","value":"\"AAACTCAA\""}],"value":"(183 \"AAACTCAA\")"}],"value":"((183 \"AAACTCAA\"))"}],"value":"[8 ((183 \"AAACTCAA\"))]"}],"value":"([1 ((294711 \"T\"))] [2 ((95191 \"TT\"))] [3 ((31901 \"TTT\"))] [4 ((10206 \"TTTT\"))] [5 ((3193 \"TTTTT\"))] [6 ((866 \"ATTTTT\"))] [7 ((326 \"AAACTCA\"))] [8 ((183 \"AAACTCAA\"))])"}
;; <=

;; @@
(for [i (range 0 8)]
  i)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"}],"value":"(0 1 2 3 4 5 6 7)"}
;; <=

;; @@
(prn '([1 ((294711 "T"))] [2 ((95191 "TT"))] [3 ((31901 "TTT"))] [4 ((10206 "TTTT"))] [5 ((3193 "TTTTT"))] [6 ((866 "ATTTTT"))] [7 ((326 "AAACTCA"))] [8 ((183 "AAACTCAA"))]))
;; @@
;; ->
;;; ([1 ((294711 &quot;T&quot;))] [2 ((95191 &quot;TT&quot;))] [3 ((31901 &quot;TTT&quot;))] [4 ((10206 &quot;TTTT&quot;))] [5 ((3193 &quot;TTTTT&quot;))] [6 ((866 &quot;ATTTTT&quot;))] [7 ((326 &quot;AAACTCA&quot;))] [8 ((183 &quot;AAACTCAA&quot;))])
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def txt (slurp (nth *command-line-args* 0)))

(defn count-substring [txt sub]
  "Function to count occuriencies of substring in string with regular expressions"
  (count (re-seq (re-pattern sub) txt)))

(defn kmers-parallel [txt k]
  "The most freuet kmer function"
  (let [theset 
        (filter 
            #(= (count %) k) 
                 (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))
        mytmp 
        (zipmap
          theset
          (pmap #(count-substring txt (str "(?=" % ")"))
               theset) )]
    (let [apmax (val (apply max-key val mytmp)) thefil (filter (comp #{apmax} mytmp) (keys mytmp))]
    (pmap list (repeat 
                (count thefil) 
                apmax)
         thefil))))

(def answer (for [i (range 1 (nth *command-line-args* 1))]
  (conj [i] (kmers-parallel txt i))))
;; @@

;; @@
(def args '("/home/azheltov/Downloads/Vibrio_cholerae.txt" 2))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/args</span>","value":"#'user/args"}
;; <=

;; @@
(read-string "2")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
(range 1 10)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(1 2 3 4 5 6 7 8 9)</span>","value":"(1 2 3 4 5 6 7 8 9)"}
;; <=

;; @@

;; @@
