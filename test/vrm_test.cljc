(ns vrm-test
  (:require [clojure.test :refer [deftest is testing]]
            [vrm]))
(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? vrm))))
