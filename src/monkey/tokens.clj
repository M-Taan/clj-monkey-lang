(ns monkey.tokens)

(def +illegal+ "ILLEGAL")
(def +eof+ "EOF")
(def +identifier+ "IDENTIFIER")
(def +integer+  "INTEGER")
(def +assign+ "=")
(def +plus+ "+")
(def +comma+ ",")
(def +semicolon+ ";")
(def +lparen+ "(")
(def +rparen+ ")")
(def +lbrace+ "{")
(def +rbrace+ "}")
(def +function+ "FUNCTION")
(def +let+ "LET")

(defn match-identifier-or-keyword [identifier]
  (case identifier
    "fn" +function+
    "let" +let+
    +identifier+))
