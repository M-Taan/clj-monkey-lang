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
(def +minus+ "-")
(def +bang+ "!")
(def +asterik+ "*")
(def +slash+ "/")
(def +lt+ "<")
(def +gt+ ">")

;; Keywords
(def +function+ "FUNCTION")
(def +let+ "LET")
(def +return+ "RETURN")
(def +if+ "IF")
(def +else+ "ELSE")
(def +true+ "TRUE")
(def +false+ "false")

(defn match-identifier-or-keyword [identifier]
  (case identifier
    "fn" +function+
    "let" +let+
    "return" +return+
    "if" +if+
    "else" +else+
    "true" +true+
    "false" +false+
    +identifier+))
