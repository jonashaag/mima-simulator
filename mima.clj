(ns mima
  (:use [clojure.string :only (trim split-lines)]))


(def max-int 0xfffff)
(def default-start-addr 0x100)
(def opcode-list [:LDC :LDV :STV :AND :ADD :OR :XOR :EQL :JMP :JMN :HALT :NOT :RAR])


(defn exec-instr [cmd memory]
  (defn mem-read [where]
    (get memory where 0))
  (defn mem-set [where what]
    (assoc memory where what))

  (let [opcode (opcode-list (bit-shift-right cmd 20))
        arg    (bit-and cmd 0x0fffff)]

    (case opcode
      :HALT nil

      :LDV (mem-set :ACCU (mem-read arg))
      :LDC (mem-set :ACCU arg)
      :STV (mem-set arg (mem-read :ACCU))

      :ADD (mem-set :ACCU (mod (+ (mem-read :ACCU)
                                  (mem-read arg))
                               (+ 1 max-int)))
      :AND (mem-set :ACCU (bit-and (mem-read :ACCU)
                                   (mem-read arg)))
      :OR  (mem-set :ACCU (bit-or  (mem-read :ACCU)
                                   (mem-read arg)))
      :XOR (mem-set :ACCU (bit-xor (mem-read :ACCU)
                                   (mem-read arg)))

      :EQL (mem-set :ACCU (case (= (mem-read :ACCU)
                                   (mem-read arg))
                            true -1
                            false 0))

      :JMP (mem-set :IAR arg)

      :JMN (if (pos? (mem-read :ACCU))
             memory
             (mem-set :IAR arg))

      (throw (IllegalArgumentException. (str opcode " " arg))))))


(defn asm
  ([code] (asm code default-start-addr))
  ([code start-addr]
    (defn lookup-opcode [op]
      (.indexOf opcode-list (keyword op)))

    (defn line->binary [line]
      (let [[op arg] (clojure.string/split line #"[ ]+")]
        (bit-or (bit-shift-left (lookup-opcode op) 20)
                (if (empty? arg)
                  0
                  (read-string arg)))))

    (let [compiled-code (map line->binary code)]
      (zipmap (drop start-addr (range))
              compiled-code))))


(defn interpret [memory]
  (if (not (contains? memory :IAR))
    (interpret (assoc memory
                      :IAR
                      default-start-addr))
    (let [next-instr (memory (memory :IAR))
          next-IA (inc (memory :IAR))
          new-memory (exec-instr next-instr
                                 (assoc memory
                                        :IAR
                                        next-IA))]
      (if (nil? new-memory)
        memory
        (interpret new-memory)))))


(defn run-code [code initial-memory]
  (let [lines (map trim
                   (conj (split-lines code)
                         "HALT"))]
    (interpret (into (asm lines)
                     initial-memory))))
