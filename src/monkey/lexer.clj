(ns monkey.lexer
  (:require
   [monkey.tokens :as tokens]
   [monkey.utils :as utils]))

(defn- read-char [input position]
  (or (and (>= position (count input))
           0)
      (nth input position)))

(defn- read-identifier-or-keyword [{:keys [input position]}]
  (loop [target ""
         position position
         ch (read-char input position)
         done? false]
    (if done?
      {:token-with-literal {:token (tokens/match-identifier-or-keyword target)
                            :literal target}
       :read-at position
       :position (dec position)}
      (let [next-char (read-char input (inc position))]
        (recur (or (and (= ch 0)
                        target)
                   (str target ch))
               (inc position)
               next-char
               (not (utils/is-letter-or-underscore? next-char)))))))

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
       :literal ""}
    nil))

(defn lex [input]
  (loop [read-at 0
         current -1
         tokens []]
    (let [ch (read-char input read-at)
          skip-char? (utils/should-skip-char? ch)]
      (if skip-char?
        (recur (inc read-at)
               read-at
               tokens)
        (let [lexer (cond
                      (utils/is-special-character? ch) {:token-with-literal (special-character->token-with-literal ch)
                                                        :read-at (inc read-at)
                                                        :current read-at}
                      (utils/is-letter-or-underscore? ch) (read-identifier-or-keyword {:input input
                                                                                       :position read-at})
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
