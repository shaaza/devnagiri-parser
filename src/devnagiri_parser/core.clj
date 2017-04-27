(ns devnagiri-parser.core
  (:require [clojure.spec :as s]
            [devnagiri-parser.leven :as l]
            [clojure.pprint :refer [pprint]]
            [devnagiri-parser.read-test-data :refer [classifications test-data]]))


(def devnagri-vowel-table
  [
   ["अ"	"a"	"a"	"a"	"a"	"a"	"a"]
   ["आ"	"ā"	"ā"	"A"	["A" "aa"]	"aa"	"A"]
   ["इ"	"i"	"i"	"i"	"i"	"i"	"i"]
   ["ई"	"ī"	"ī"	"I"	["I" "ii"]	"ii"	"I"]
   ["उ"	"u"	"u"	"u"	"u"	"u"	"u"]
   ["ऊ"	"ū"	"ū"	"U" ["U" "uu"]	"uu"	"U"]
   ["ए"	"e"	"ē"	"e"	"e"	"e"	"e"]
   ["ऐ"	"ai"	"ai"	"ai"	"ai"	"ai"	"E"]
   ["ओ"	"o"	"ō"	"o"	"o"	"o"	"o"]
   ["औ"	"au"	"au"	"au"	"au"	"au"	"O"]

   ["ऋ"	"ṛ"	"r̥"	"R"	["RRi" "R^i"]	".r"	"f"]
   ["ॠ"	"ṝ"	"r̥̄"	"RR"	["RRI" "R^I"]	".rr"	"F"]
   ["ऌ"	"ḷ"	"l̥"	"lR"	["LLi" "L^i"]	".l"	"x"]
   ["ॡ"	"ḹ"	"l̥̄"	"lRR"	["LLI" "L^I"]	".ll"	"X"]

   ["अं"	"ṃ"	"ṁ"	"M"	["M" ".n" ".m"]	".m"	"M"]
   ["अः"	"ḥ"	"ḥ"	"H"	"H"	".h"	"H"]
   ["अँ" "--" "--" "--" ".N" "--" "~" ]])


(def transliteration-schemes
  {:devanagari    0
   :iast	  1
   :iso15919	  2
   :harvard-kyoto 3
   :itrans        4
   :velthuis      5
   :slp1          6})


(s/def ::scheme (set (keys transliteration-schemes)))

(def devnagri-consonants [
                          ["क"	"ka"	"ka"	"ka"	"ka"	"ka"	"ka"]
                          ["ख"	"kha"	"kha"	"kha"	"kha"	"kha"	"Ka"]
                          ["ग"	"ga"	"ga"	"ga"	"ga"	"ga"	"ga"]
                          ["घ"	"gha"	"gha"	"gha"	"gha"	"gha"	"Ga"]
                          ["ङ"	"ṅa"	"ṅa"	"Ga"	"~Na"	"\"na"	"Na"]
                          ["च"	"ca"	"ca"	"ca"	"cha"	"ca"	"ca"]
                          ["छ"	"cha"	"cha"	"cha"	"Cha"	"cha"	"Ca"]
                          ["ज"	"ja"	"ja"	"ja"	"ja"	"ja"	"ja"]
                          ["झ"	"jha"	"jha"	"jha"	"jha"	"jha"	"Ja"]
                          ["ञ"	"ña"	"ña"	"Ja"	"~na"	"~na"	"Ya"]
                          ["ट"	"ṭa"	"ṭa"	"Ta"	"Ta"	".ta"	"wa"]
                          ["ठ"	"ṭha"	"ṭha"	"Tha"	"Tha"	".tha"	"Wa"]
                          ["ड"	"ḍa"	"ḍa"	"Da"	"Da"	".da"	"qa"]
                          ["ढ"	"ḍha"	"ḍha"	"Dha"	"Dha"	".dha"	"Qa"]
                          ["ण"	"ṇa"	"ṇa"	"Na"	"Na"	".na"	"Ra"]
                          ["त"	"ta"	"ta"	"ta"	"ta"	"ta"	"ta"]
                          ["थ"	"tha"	"tha"	"tha"	"tha"	"tha"	"Ta"]
                          ["द"	"da"	"da"	"da"	"da"	"da"	"da"]
                          ["ध"	"dha"	"dha"	"dha"	"dha"	"dha"	"Da"]
                          ["न"	"na"	"na"	"na"	"na"	"na"	"na"]
                          ["प"	"pa"	"pa"	"pa"	"pa"	"pa"	"pa"]
                          ["फ"	"pha"	"pha"	"pha"	"pha"	"pha"	"Pa"]
                          ["ब"	"ba"	"ba"	"ba"	"ba"	"ba"	"ba"]
                          ["भ"	"bha"	"bha"	"bha"	"bha"	"bha"	"Ba"]
                          ["म"	"ma"	"ma"	"ma"	"ma"	"ma"	"ma"]
                          ["य"	"ya"	"ya"	"ya"	"ya"	"ya"	"ya"]
                          ["र"	"ra"	"ra"	"ra"	"ra"	"ra"	"ra"]
                          ["ल"	"la"	"la"	"la"	"la"	"la"	"la"]
                          ["व"	"va"	"va"	"va"	["va" "wa"]	"va"	"va"]
                          ["श"	"śa"	"śa"	"za"	"sha"	"\"sa"	"Sa"]
                          ["ष"	"ṣa"	"ṣa"	"Sa"	"Sha"	".sa"	"za"]
                          ["स"	"sa"	"sa"	"sa"	"sa"	"sa"	"sa"]
                          ["ह"	"ha"	"ha"	"ha"	"ha"	"ha"	"ha"]])

(def irregular-consonant-clusters
  [["क्ष"	"kṣa"	"kSa"	["kSa" "kSha" "xa"]	"k.sa"	"kza"]
   ["त्र"	"tra"	"tra"	"tra"	"tra"	"tra"]
   ["ज्ञ"	"jña"	"jJa"	["GYa" "j~na"]	"j~na"	"jYa"]
   ["श्र"	"śra"	"zra"	"shra"	"sra"	"Sra"]])



(def devnagri-table
  (concat devnagri-vowel-table devnagri-consonants))

(defn remove-vector-keys
  [acc elem]
  (cond
    (vector? (first elem)) (apply conj acc (for [x (first elem)]
                                             [x (second elem)]))
    true (conj acc elem)))

(defn add-extra-match
  "Given a string like 'ka', returns ['k' 'ka']"
  [string]
  (if (= \a (last (seq string)))
    [(apply str (butlast string)) string]
    string))

(defn mapping
  [n table]
  (let [devnagiri (map first table)
        latin (map #(nth % n) table)
        latin-enhanced (map add-extra-match latin)
        pairs (partition 2 (interleave latin-enhanced devnagiri))
        pairs-without-vecs (reduce remove-vector-keys [] pairs)]
    (apply hash-map (flatten pairs-without-vecs))))

(defn to-devnagri
  [s m print?]
  (loop [current-parse []
         x s]
    (when print?
      (println current-parse )
      (println x))
    (if (>= 0 (count (seq x)))
      current-parse
      (if-let [three  (get m (apply str (take 3 x)))]
        (recur (conj current-parse three) (apply str (drop 3 x)))
        (if-let [two  (get m (apply str (take 2 x)))]
          (recur (conj current-parse two) (apply str (drop 2 x)))
          (if-let [one  (get m (apply str (take 1 x)))]
            (recur (conj current-parse one) (apply str (drop 1 x)))
            (recur current-parse (apply str (drop 1 x)))))))))


(s/def ::parse-args (s/cat :scheme ::scheme
                           :transliterated-word string?))

(defn parse-devanagari
  ([string] (parse-devanagari :itrans string))
  ([scheme string]
   (let [scheme-no (scheme transliteration-schemes)]
     (apply str (to-devnagri string
                             (mapping scheme-no devnagri-table)
                             false)))))



(def k "kharaharapriyA")




(def ragam-names ["Yadukulakaambhoji" "yadukula kAmbhOji" "yadukulakAmbhOji"
    "yadukula kAmbhOdi" "yadukula kaambhOji"])



;; (map parse-devanagari ragam-names)



(s/def ::coll-of-strings (s/coll-of string?))

(s/def ::devnagri-parse-map (s/map-of string? ::coll-of-strings))

;;; we need to get all the rags from db here



;; (pprint (test-data))

(s/def ::devnagri-string string?)

(s/def ::test-map
  (s/map-of ::devnagri-string
            (s/map-of integer? ::coll-of-strings)))


(defn setmap []
  (let [rt (group-by parse-devanagari (test-data))
        set-count (group-by (comp count second) rt)
        sets (take 2 (sort-by (comp count second) set-count))
        samples (mapcat second sets)
        setmap
        (apply merge (for [[s x] samples]
                       {s {0 x}}))]
    setmap))



(defn deep-merge [a b]
  (merge-with (fn [x y]
                (cond (map? y) (deep-merge x y)
                      (set? y) (set (concat x y))
                      :else y))
              a b))



(defn parse-tests []
  (let [rt (group-by parse-devanagari (test-data))
        set-count (group-by (comp count second) rt)
        sets (take 2 (sort-by (comp count second) set-count))
        samples (mapcat second sets)
        setmap
        (apply merge (for [[s x] samples]
                       {s {0 (set x)}}
                       ))
        single-items (get set-count 1)
        unmatched (atom #{})
        _
        (assert (s/valid? ::test-map setmap))
        results 
        (for [[unmatched-devnagri-term [w] :as um] (take 5 single-items)]
          (for [[dev-match-term match-map] setmap]
            (let [l (l/levenshtein dev-match-term unmatched-devnagri-term)]
              ;; (println "match" l)
              ;; (println unmatched-devnagri-term )
              (if (>= 2 l)
                (update-in match-map [l] (fnil conj []) [w] )
                (swap! unmatched conj um)
                )
              )
            )
          )]

   {:results results
    :failures @unmatched}
   ))


(for [x {:a 1}]
  x)

;; (def all-tests (parse-tests))


(parse-tests )
