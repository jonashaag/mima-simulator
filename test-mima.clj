(ns mima-test
  (:use mima clojure.test clojure.set))


(defn submap? [a b]
  (subset? (set (seq a))
           (set (seq b))))


(defmacro def-op-test [test-name code initial-memory result-memory]
  `(deftest ~test-name
     (let [actual-result-memory# (mima/run-code ~code
                                                ~initial-memory)]
       (if (not (submap? ~result-memory
                         actual-result-memory#))
         (report {:type :fail
                  :message (str ~result-memory
                                " is not a subset of "
                                actual-result-memory#)
                  :expected ~result-memory
                  :actual actual-result-memory#})
         (report {:type :pass})))))
    

(def-op-test LDV
  "LDV 0x0",
  {0x0 0x42},
  {0x0 0x42
   :ACCU 0x42
   :IAR 0x101})


(def-op-test LDC
  "LDC 0x42",
  {},
  {:ACCU 0x42
   :IAR 0x101})


(def-op-test STV
  "LDC 0x42
  STV 0x1337",
  {},
  {0x1337 0x42
   :ACCU 0x42
   :IAR 0x102})


(def-op-test ADD
  "ADD 0x23",
  {0x23 0x1336
   :ACCU 0x1},
  {0x23 0x1336
   :ACCU 0x1337
   :IAR 0x101})


(def-op-test ADD-overflow
  "ADD 0x0",
  {0x0 0xfffff
   :ACCU 0x1},
  {0x0 0xfffff
   :ACCU 0x0
   :IAR 0x101})


(def-op-test AND-OR-XOR
  "AND 0x1000
   STV 0x5
   OR  0x1001
   STV 0x6
   XOR 0x1002",
  {0x1000 0x7
   0x1001 0x2
   0x1002 0x8
   :ACCU 0x11},
  {0x5 0x1
   0x6 0x3
   :ACCU 0xB
   :IAR 0x105})


(def-op-test EQL
  "EQL 0x1000
   STV 0x0
   EQL 0x1001
   STV 0x1",
  {:ACCU 0x42
   0x1000 0x42
   0x1001 0x43},
  {0x0 -1
   0x1 0})


(def-op-test JMP
  "JMP 0x102
   HALT
   LDC 0x42",
  {},
  {:ACCU 0x42})


(def-op-test JMN
  "JMN 0x102
   HALT
   LDC 0x1
   JMN 0x105
   LDC 0x42",
  {:ACCU -1},
  {:ACCU 0x42})


(run-tests)
