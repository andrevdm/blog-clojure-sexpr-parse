(ns cljsexp-instaparse.core
  (:require [instaparse.core :as insta]))


(def parse
  (insta/parser
   "S = (expression <ws?>)*
    expression = list | vector | atom
    list = <'('> <ws?> (expression <ws?>)* <')'>
    vector = <'['> (expression <ws?>)* <']'>
    atom = number | string | name
    number = #'\\d+'
    string = <'\"'> #'[^\\\"]+' <'\"'>
    name = #'[a-zA-Z\\+\\-\\/\\*\\?\\$~!@#%\\^\\&=\\`\\<\\>]([0-9a-zA-Z\\+-\\/\\*\\?\\$~!@#\\^\\&=\\`\\<\\>]*)'
    ws = #'\\s+'"))

(def methods
  {"+" +
   "-" -
   "*" *
   "/" /
   "++" inc
   "--" dec
   "prn" println})

(defmulti run (fn [s] (nth s 0)))
(defmethod run :S [[s & es]] (last (doall (map run es))))
(defmethod run :expression [[e t]] (run t))
(defmethod run :atom [[a t]] (run t))
(defmethod run :number [[n val]] (read-string val))
(defmethod run :string [[s val]] val)
(defmethod run :vector [[v & vs]] (vec (map run vs)))
(defmethod run :name [[n & nn]] (first nn))
(defmethod run :list [[l n & ls]] (let [args (map run ls)]
                                    (apply (methods (run n)) args)))
