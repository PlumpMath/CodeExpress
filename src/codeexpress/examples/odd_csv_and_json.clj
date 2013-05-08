;; odd.clj
;; an example problem for codeexpress, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns codeexpress.examples.pushgp.odd-csv-and-json
  (:use codeexpress.pushgp.pushgp)
  (:require codeexpress.examples.pushgp.odd))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

;; This version shows how you can print csv and json log files.

(def argmap
  (merge codeexpress.examples.pushgp.odd/argmap
         {:print-csv-logs true
          :print-json-logs true
          :csv-log-filename "log.csv"
          :json-log-filename "log.json"
          :log-fitnesses-for-all-cases false
          :json-log-program-strings false
          }))
