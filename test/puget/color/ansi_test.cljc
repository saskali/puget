(ns puget.color.ansi-test
  (:require
    #?(:clj [clojure.test :refer [are deftest is testing]]
       :cljs [cljs.test :refer-macros [are deftest is testing]])
    [puget.color.ansi :as ansi]))


(deftest colored-text
  (let [text "foo"
        color (ansi/sgr text :red)]
    (is (< (count text) (count color)))
    (is (= text (ansi/strip color)))))
