(ns monkey.utils)

(defn is-letter-or-underscore? [ch]
  (or (Character/isLetter ch)
      (= ch \_)))

(defn should-skip-char? [ch]
  (or (= \newline ch)
      (= \space ch)))

(defn is-special-character? [ch]
  (or (= ch \=) (= ch \;)
      (= ch \() (= ch \))
      (= ch \{) (= ch \})
      (= ch \,) (= ch \+)
      (= ch \*) (= ch \-)
      (= ch \!) (= ch \<) 
      (= ch \>) (= ch \/)
      (= ch 0)))

(defn is-digit? [ch]
  (Character/isDigit ch))
