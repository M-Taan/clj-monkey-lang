(ns monkey.parser
  (:require [monkey.tokens :as tokens]))

(defn expect-peek [tokens target]
  (let [current (first tokens)]
    (if (= (:token current) target)
      [current (rest tokens)]
      (throw (Exception. (str "Parser Error: Expected " target " and got " (:literal current)))))))

;; [TODO] Just temporarly
(defn parse-exprs [[{:keys [token]} :as tokens]]
  (if (= token tokens/+semicolon+)
    (rest tokens)
    (recur (rest tokens))))

(defn parse-let-statement [tokens]
  (let [[ident-token rest-tokens] (expect-peek tokens tokens/+identifier+)
        [_ rest-tokens] (expect-peek rest-tokens tokens/+assign+)
        ;; [TODO] Fix later when parsing exprs
        rest-tokens (parse-exprs rest-tokens)]
    [[tokens/+let+ (:literal ident-token)] rest-tokens]))

(defn parse-statement [[{:keys [token]} :as tokens]]
  (condp = token
    tokens/+let+ (parse-let-statement (rest tokens))
    (throw (Exception. "Parser Error: Can't parse statemenet"))))

(defn parse-program [[current :as tokens]]
  (when-not (= (:token current) tokens/+eof+)
    (let [[stmt rest-tokens](parse-statement tokens)]
      (cons stmt (parse-program rest-tokens)))))
