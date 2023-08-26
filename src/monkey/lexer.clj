(ns monkey.lexer
  (:require
   [monkey.tokens :as tokens]
   [monkey.utils :as utils]))

(defn- read-char [in position]
  (or (and (>= position (count in))
           0)
      (nth in position)))

(defn- read-until [predicate in starting-position]
  (loop [target ""
         position starting-position
         ch (read-char in position)
         done? false]
    (if done?
      {:target target
       :exit-position position}
      (let [next-char (read-char in (inc position))]
        (recur (or (and (= ch 0)
                        target)
                   (str target ch))
               (inc position)
               next-char
               (predicate next-char))))))

(defn- read-identifier-or-keyword [{:keys [in start]}]
  (let [read-value (read-until #(not (utils/is-letter-or-underscore? %)) in start)]
    {:token-with-literal {:token (tokens/match-identifier-or-keyword (:target read-value))
                          :literal (:target read-value)}
     :read-at (:exit-position read-value)
     :position (dec (:exit-position read-value))}))

(defn- read-number [{:keys [in start]}]
  (let [read-value (read-until #(not (utils/is-digit? %)) in start)]
    {:token-with-literal {:token tokens/+integer+
                          :literal (:target read-value)}
     :read-at (:exit-position read-value)
     :position (dec (:exit-position read-value))}))

(defn- special-character->token-with-literal [ch]
  (condp = ch
    \= {:token tokens/+assign+
        :literal ch}
    \; {:token tokens/+semicolon+
        :literal ch}
    \( {:token tokens/+lparen+
        :literal ch}
    \) {:token tokens/+rparen+
        :literal ch}
    \{ {:token tokens/+lbrace+
        :literal ch}
    \} {:token tokens/+rbrace+
        :literal ch}
    \, {:token tokens/+comma+
        :literal ch}
    \+ {:token tokens/+plus+
        :literal ch}
    0 {:token tokens/+eof+
       :literal ""}))

(defn lex [in]
  (loop [read-at 0
         current -1
         tokens []]
    (let [ch (read-char in read-at)
          skip-char? (utils/should-skip-char? ch)]
      (if skip-char?
        (recur (inc read-at)
               read-at
               tokens)
        (let [lexer (cond
                      (utils/is-special-character? ch) {:token-with-literal (special-character->token-with-literal ch)
                                                        :read-at (inc read-at)
                                                        :current read-at}
                      (utils/is-letter-or-underscore? ch) (read-identifier-or-keyword {:in in
                                                                                       :start read-at})
                      (utils/is-digit? ch) (read-number {:in in
                                                         :start read-at})
                      :else
                      {:token-with-literal {:token tokens/+illegal+
                                            :literal ch}
                       :read-at nil
                       :current nil})
              token (get-in lexer [:token-with-literal :token])
              eof-reached? (= token
                              tokens/+eof+)
              illegal-char? (= token
                               tokens/+illegal+)]
          (if (or eof-reached?
                  illegal-char?)
            (or (and eof-reached? tokens)
                (throw (Exception. (str "Illegal character " (get-in lexer [:token-with-literal :literal])))))
            (recur (:read-at lexer)
                   (:positon lexer)
                   (conj tokens (:token-with-literal lexer)))))))))
