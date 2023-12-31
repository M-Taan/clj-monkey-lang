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
     :read-at (:exit-position read-value)}))

(defn- read-number [{:keys [in start]}]
  (let [read-value (read-until #(not (utils/is-digit? %)) in start)]
    {:token-with-literal {:token tokens/+integer+
                          :literal (:target read-value)}
     :read-at (:exit-position read-value)}))

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
    \* {:token tokens/+asterik+
        :literal ch}
    \- {:token tokens/+minus+
        :literal ch}
    \/ {:token tokens/+slash+
        :literal ch}
    \! {:token tokens/+bang+
        :literal ch}
    \< {:token tokens/+lt+
        :literal ch}
    \> {:token tokens/+gt+
        :literla ch}
    0 {:token tokens/+eof+
       :literal ""}))

(defn- peak-char [in peak-at]
  (let [ch (read-char in peak-at)]
    (if (utils/should-skip-char? ch)
      (recur in (inc peak-at))
      {:ch ch
       :read-until (inc peak-at)})))

(defn- read-special-character [{:keys [in start]}]
  (let [ch (read-char in start)
        default-map (fn []
                      {:token-with-literal (special-character->token-with-literal ch)
                       :read-at (inc start)})
        peaked-char (peak-char in (inc start))]
    (condp = ch
      \= (or (and (= (:ch peaked-char) \=)
                  {:token-with-literal {:token tokens/+equal+
                                        :literal "=="}
                   :read-at (:read-until peaked-char)})
             (default-map))
      \! (or (and (= (:ch peaked-char) \=)
                  {:token-with-literal {:token tokens/+not-equal+
                                        :literal"!="}
                   :read-at (:read-until peaked-char)})
             (default-map))
      (default-map))))

(defn tokenize
  ([in]
   (tokenize in 0 []))  
  ([in read-at tokens]
   (let [ch (read-char in read-at)
         skip-char? (utils/should-skip-char? ch)]
     (if skip-char?
       (recur in (inc read-at) tokens)
       (let [lexer (cond
                     (utils/is-special-character? ch) (read-special-character {:in in
                                                                               :start read-at})
                     (utils/is-letter-or-underscore? ch) (read-identifier-or-keyword {:in in
                                                                                      :start read-at})
                     (utils/is-digit? ch) (read-number {:in in
                                                        :start read-at})
                     :else
                     {:token-with-literal {:token tokens/+illegal+
                                           :literal ch}
                      :read-at nil})
             token (get-in lexer [:token-with-literal :token])
             eof-reached? (= token
                             tokens/+eof+)
             illegal-char? (= token
                              tokens/+illegal+)]
         (if (or eof-reached?
                 illegal-char?)
           (or (and eof-reached? (conj tokens (:token-with-literal lexer)))
               (throw (Exception. (str "Illegal character " (get-in lexer [:token-with-literal :literal])))))
           (recur in (:read-at lexer) (conj tokens (:token-with-literal lexer)))))))))
