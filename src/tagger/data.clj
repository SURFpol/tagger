(ns tagger.data
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.nippy :as nippy]))

(defn load-raw-data
  [filename]
  (let [source (first (str/split filename #"\."))
        data (-> filename
                 io/resource
                 io/reader
                 (json/parse-stream true))]
    (->> data
         (mapcat :documents)
         (map #(assoc % :source source)))))

(defn load-all-raw-data
  []
  (let [files ["figshare.json"
               "hbovpk.json"
               "leraar24.json"
               "stimuleringsregeling.json"
               "wur.json"]]
    (->> files
         (mapcat load-raw-data))))

(defn write-json-data
  [data out-file]
  (with-open [w (io/writer out-file)]
    (json/generate-stream data w)))

(defn todo-data
  [data]
  (reduce (fn [m doc] (assoc m (:id doc) doc))
    {} data))

(defn persistent-atom
  [init-val filename]
  (let [a (atom init-val)]
    (add-watch a :persister
      (fn [_ _ _ new-state]
        (nippy/freeze-to-file filename new-state)))))

(defn create-initial-state-file
  []
  (let [data (load-all-raw-data)
        todo (todo-data data)
        state {:todo todo
               :done {}
               :current {}}]
    (nippy/freeze-to-file "resources/state" state)))

(comment
  ; write initial state file
  (create-initial-state-file)

  ; test output
  (let [state (nippy/thaw-from-file "resources/state")]
    state)


  nil)
