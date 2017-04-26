(ns devnagiri-parser.core)

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

(def devnagri-consonants [
                          ["Devanāgarī"	"IAST"	"ISO" "15919"	"Harvard-Kyoto"	"ITRANS"	"Velthuis"	"SLP1"]
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
  (concat devnagri-vowel-table devnagri-consonants irregular-consonant-clusters))

(defn mapping
  [n]
  (let [devnagiri (map first devnagri-table)
        latin (map #(nth % n) devnagri-table)
        pairs (interleave latin devnagiri)]
    (apply hash-map pairs)))



(def m1 (mapping 1))


(println m1)

(defn to-devnagri [s m]
  (loop [current-parse []
         x s]
    (println current-parse )
    (println x)
    (if (>= 0 (count (seq x)))
      current-parse
      (if-let [three  (get m (apply str (take 3 x)))]
        (recur (conj current-parse three) (apply str (drop 3 x)))
        (if-let [two  (get m (apply str (take 2 x)))]
          (recur (conj current-parse two) (apply str (drop 2 x)))
          (if-let [one  (get m (apply str (take 1 x)))]
            (recur (conj current-parse one) (apply str (drop 1 x)))
            (recur (conj current-parse [:fail x]) (apply str (drop 1 x)))
            ) 
          ) 
        ))
    )
  )

(def k "kharaharapriyA")


(apply str (drop 1 "aa"))

(to-devnagri k (mapping 1))

