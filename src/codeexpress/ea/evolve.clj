(ns codeexpress.ea.evolve
  (:require [clojure.java.io :as io])
  (:use [codeexpress globals util pushstate random individual evaluate]
        [codeexpress.instructions boolean code common numbers random-instructions string tag zip return]
        [codeexpress.ea breed parent-selection report]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ea evolve

(def evolve-argmap
     (atom (sorted-map :error-function (fn [p] '(0)) ;; pgm -> list of errors (1 per case)
                       :error-threshold 0
                       :population-size 1000
                       :max-points 50 
                       :max-points-in-initial-program 50
                       :atom-generators (concat @registered-instructions
                                                (list 
                                                 (fn [] (lrand-int 100))
                                                 (fn [] (lrand))))
                       :max-generations 1001
                       :max-mutations nil
                       :mutation-probability 0.4
                       :mutation-max-points 20
                       :crossover-probability 0.4
                       :tournament-size 7
                       :trivial-geography-radius 0
                       :evalpush-limit 150
                       :evalpush-time-limit 0
                       :node-selection-method :unbiased
                       :node-selection-leaf-probability 0.1
                       :node-selection-tournament-size 2
                       :reuse-errors true
                       :problem-specific-report default-problem-specific-report
                       :use-single-thread false
                       :random-seed (System/nanoTime)
                       :print-csv-logs false
                       :csv-log-filename "log.csv"
                       :parent-reversion-probability 0.0
                       :tag-limit 10000
                       :initial-population nil
                       :print-errors true
                       :print-history true
                       :save-initial-population false)))

(defn load-push-argmap
  [argmap]
  (doseq [[argkey argval] argmap]
    #_(assert (contains? @evolve-argmap argkey) (str "Argument key " argkey " is not a recognized argument to pushgp."))
    (swap! push-argmap assoc argkey argval)))

(defn reset-globals []
  (doseq [[gname gatom] (filter (fn [[a _]] (.startsWith (name a) "global-")) (ns-publics 'codeexpress.globals))]
    (when (contains? @push-argmap (keyword (.substring (name gname) (count "global-"))))
      (reset! @gatom (get @push-argmap (keyword (.substring (str gname) (count "global-"))))))))

(defn make-agents-and-rng [{:keys [initial-population use-single-thread population-size
                                   max-points-in-initial-program atom-generators random-seed
                                   save-initial-population]}]
  (let [agent-error-handler (fn [agnt except]
                              (.printStackTrace except System/out)
                              (.printStackTrace except)
                              (System/exit 0))]
    {:pop-agents (if initial-population
                   (->> (read-string (slurp (str "data/" initial-population)))
                        (map #(if use-single-thread (atom %) (agent %)))
                        (vec))
                   (let [pa (doall (for [_ (range population-size)]
                                     (make-individual
                                       :program (random-code max-points-in-initial-program atom-generators)
                                       :error-handler agent-error-handler)))
                         f (str "data/" (System/currentTimeMillis) ".ser")]
                     (when save-initial-population
                       (io/make-parents f)
                       (spit f (printable (map individual-string pa))))
                     (vec (map #(if use-single-thread (atom %) (agent %)) pa))))
     :child-agents (vec (doall (for [_ (range population-size)]
                                 ((if use-single-thread atom agent)
                                      (make-individual)
                                      :error-handler agent-error-handler))))
     :rand-gens (vec (doall (for [k (range population-size)]
                              (java.util.Random. (+ random-seed (inc k))))))
     }))

(defn compute-errors [pop-agents rand-gens {:keys [use-single-thread error-function]}]
  (dorun (map #((if use-single-thread swap! send) % evaluate-individual error-function %2)
              pop-agents
              rand-gens))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE ;might this need a dorun?


;; I feel like the printing shoudl be in the main loop, but i'm just cutting and pasting for now
(defn parental-reversion [pop-agents generation {:keys [parent-reversion-probability use-single-thread]}]
  (if (and (> generation 0) (> parent-reversion-probability 0))
    (let [err-fn (if @global-use-rmse :rms-error :total-error)]
      (printf "\nPerforming parent reversion...") (flush)
      (dorun (map #((if use-single-thread swap! send) 
                    % 
                    (fn [i]  
                      (if (or (< (err-fn i) (err-fn (:parent i)))
                              (and (= (err-fn i) (err-fn (:parent i)))
                                   (< (count-points (:program i))
                                      (count-points (:program (:parent i)))))
                              (> (lrand) parent-reversion-probability))
                        (assoc i :parent nil)  ;; don't store whole ancestry
                        (:parent i))))
                  pop-agents))
      (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
      (printf "\nDone performing parental reversion.") (flush))))

(defn report-and-check-for-success
  [pop-agents generation {:keys [error-function print-csv-logs
                                 csv-log-filename max-generations
                                 print-errors print-history
                                 problem-specific-report error-threshold]}]
  (let [best (report (vec (doall (map deref pop-agents))) generation error-function 
                     print-csv-logs
                     csv-log-filename 
                     print-errors print-history
                     problem-specific-report)]
    (cond (<= (:total-error best) error-threshold) best
          (>= generation max-generations) :failure
          :else :continue)))
          
(defn produce-new-offspring
  [pop-agents child-agents rand-gens
   {:keys [population-size trivial-geography-radius use-single-thread]}]                      
  (let [pop (vec (doall (map deref pop-agents))) #_(if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size
                        trivial-geography-radius))]
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
       (nth child-agents i) 
       breed 
       i (nth rand-gens i) pop @push-argmap)))
  (when-not use-single-thread (apply await child-agents))) ;; SYNCHRONIZE

(defn install-next-generation [pop-agents child-agents {:keys [population-size use-single-thread]}]
  (dotimes [i population-size]
    ((if use-single-thread swap! send)
     (nth pop-agents i) (fn [av] (deref (nth child-agents i)))))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn evolve
  "The top-level EA."
  ([] (evolve '()))
  ([args]
     (load-push-argmap args)
     (binding [*thread-local-random-generator* (java.util.Random. (:random-seed @push-argmap))]
       ;; set globals from parameters
       (reset-globals)
       (initial-report) ;; Print the inital report
       (print-params @push-argmap)
       (printf "\nGenerating initial population...\n") (flush)
       (let [{:keys [pop-agents child-agents rand-gens]} (make-agents-and-rng @push-argmap)]
         ;; Main loop
         (loop [generation 0]
           (printf "\n\n-----\nProcessing generation: %s\nComputing errors..." generation)
           (compute-errors pop-agents rand-gens @push-argmap)
           (flush)
           (printf "\nDone computing errors.") (flush)
           ;; possible parent reversion
           (parental-reversion pop-agents generation @push-argmap)
           ;; report and check for success
           (let [outcome (report-and-check-for-success pop-agents generation @push-argmap)]
             (cond (= outcome :failure) (do (printf "\nFAILURE\n") (flush))
                   (= outcome :continue) (do (printf "\nProducing offspring...") (flush)
                                             (produce-new-offspring pop-agents child-agents rand-gens @push-argmap)
                                             (printf "\nInstalling next generation...") (flush)
                                             (install-next-generation pop-agents child-agents @push-argmap)
                                             (recur (inc generation)))
                   :else (let [{:keys [error-function]} @push-argmap]
                           (final-report generation outcome error-function)))))))))
