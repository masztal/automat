(ns automat.core-input-comparator-test
  (:require
    [clojure.test :refer :all]
    [clojure.set :as set]
    [automat.core :as a]
    [criterium.core :as c]))

(deftest test-advance-input-comparator-simple
  (let [fsm' (a/compile [{:a 1} {:a 2} {:a 3}]
                        {:backend :base
                         :input-comparator (fn [pat inp] (= (:a pat) (:b inp)))})
        state1 (a/advance fsm' nil {:b 1} nil)
        state2 (a/advance fsm' state1 {:a 2} nil)]

    (is (not (nil? state1) ))
    (is (nil? (a/advance fsm' state1 {:a 2} nil)))))

(def backends [:base])

(deftest test-advance-input-comparator
  (doseq [backend backends]
    (testing backend
      (are [fsm input-seqs]
        (let [fsm' (a/compile
                     [(a/$ :init) fsm]
                     {:reducers {:init (constantly []), :conj conj, :conj_ conj}
                      :backend backend
                      :input-comparator (fn [pat inp] (= pat inp))})]
          (every?
            (fn [[expected s]]
              (= expected (:value (reduce #(a/advance fsm' %1 %2) nil s))))
            (partition 2 input-seqs)))

        (a/or
          (a/interpose-$ :conj [1 a/any 3])
          [1 2 3])
        [[1 2 3] [1 2 3]
         [1 9 3] [1 9 3]]

        [(a/interpose-$ :conj (a/* a/any))
         (a/interpose-$ :conj [1 2])]
        [[0 1 2] [0 1 2]]

        [(a/interpose-$ :conj (a/* a/any))
         (a/interpose-$ :conj_ [1 2])]
        [[0 1 2] [0 1 2]]

        (a/interpose-$ :conj [1 2 3 4])
        [[1] [1]
         [1 2] [1 2]
         [1 2 3] [1 2 3]
         [1 2 3 4] [1 2 3 4]]

        [1 (a/$ :conj) (a/$ :conj)]
        [[1] [1]]

        [1 a/any (a/$ :conj) 3 4 (a/$ :conj)]
        [[2 4] [1 2 3 4]]

        [(a/or
           (a/interpose-$ :conj [1 2 3])
           [4])
         (a/$ :conj)
         5]
        [[1 2 3] [1 2 3 5]
         [4] [4 5]]

        [1 (a/$ :conj) 2 (a/$ :init) 3 (a/$ :conj) 4 (a/$ :conj)]
        [[1] [1]
         [] [1 2]
         [3] [1 2 3]
         [3 4] [1 2 3 4]]))

    (are [fsm input-seqs]
      (let [fsm' (a/compile
                   [(a/$ :init) fsm]
                   {:reducers {:init (constantly []), :conj conj}
                    :signal inc
                    :backend backend
                    :input-comparator (fn [pat inp] (= pat inp))})]
        (every?
          (fn [[expected s]]
            (= expected (:value (reduce #(a/advance fsm' %1 %2) nil s))))
          (partition 2 input-seqs)))

      (a/interpose-$ :conj [1 2 3 4])
      [[0] [0]])))
