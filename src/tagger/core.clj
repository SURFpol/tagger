(ns tagger.core
  (:require [tagger.data :refer [persistent-atom create-initial-state-file]]
            [taoensso.nippy :as nippy]
            [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]
            [fn-fx.util :refer [run-later]]
            [clojure.java.io :as io]))

(def main-font (ui/font :family "Helvetica" :size 20))

(defn init-state
  []
  (persistent-atom (nippy/thaw-from-file "resources/state") "resources/state"))

(def ui-state (agent nil))

(defn load-url
  [url]
  (let [engine (.getEngine (.lookup (.getScene @(:root @ui-state)) "#web-browser"))]
    (run-later
     (.load engine url))))

(defn clear-browser
  []
  (let [engine (.getEngine (.lookup (.getScene @(:root @ui-state)) "#web-browser"))
        html "<html><body></body></html"]
    (run-later
     (.loadContent engine html))))

(defui MainWindow
  (render [this state]
    (let [{:keys [id source text language mime_type title url]
           :or {id "" source "" text "" language "" mime_type "" title "" url ""}
           :as doc}
          (-> state :current :doc)
          n-todo (-> state :todo count)
          n-done (-> state :done count)]
      (ui/grid-pane
        :alignment :center
        :hgap 10
        :vgap 10
        :padding (ui/insets
                   :bottom 25
                   :left 25
                   :right 25
                   :top 25)
        :children [(ui/label
                     :text "id:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 0)

                   (ui/text-field
                     :id :id-field
                     :text (if-not (nil? id) id "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 0
                     :editable false)

                   (ui/label
                     :text "source:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 1)

                   (ui/text-field
                     :id :source-field
                     :text (if-not (nil? source) source "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 1
                     :editable false)

                   (ui/label
                     :text "title:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 2)

                   (ui/text-field
                     :id :title-field
                     :text (if-not (nil? title) title "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 2
                     :editable false)

                   (ui/label
                     :text "language:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 3)

                   (ui/text-field
                     :id :language-field
                     :text (if-not (nil? language) language "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 3
                     :editable false)

                   (ui/label
                     :text "mime-type:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 4)

                   (ui/text-field
                     :id :mime-type-field
                     :text (if-not (nil? mime_type) mime_type "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 4
                     :editable false)

                   (ui/label
                     :text "url:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 5)

                   (ui/text-field
                     :id :url-field
                     :text (if-not (nil? url) url "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 5
                     :editable false)

                   (ui/button
                     :id "url-button"
                     :text "-->"
                     :grid-pane/column-index 2
                     :grid-pane/row-index 5
                     :on-action {:event :load-url
                                 :url (if-not (nil? url) url "")})

                   (ui/label
                     :text "text:"
                     :grid-pane/column-index 0
                     :grid-pane/row-index 6)

                   (ui/text-area
                     :id :text-field
                     :text (if-not (nil? text) text "")
                     :grid-pane/column-index 1
                     :grid-pane/row-index 6
                     :min-width 800
                     :min-height 600
                     :wrap-text true
                     :editable false)

                   (ui/h-box
                     :grid-pane/column-index 0
                     :grid-pane/row-index 7
                     :grid-pane/column-span 2
                     :alignment :bottom-right
                     :spacing 10
                     :children [(ui/button
                                  :id "en-button"
                                  :text "Engels"
                                  :on-action {:event :set-language
                                              :language "en"
                                              :doc-id id
                                              :doc doc})
                                (ui/button
                                  :id "nl-button"
                                  :text "Nederlands"
                                  :on-action {:event :set-language
                                              :language "nl"
                                              :doc-id id
                                              :doc doc})])

                   (ui/label
                     :text (str "Done: " n-done "/" (+ n-todo n-done))
                     :grid-pane/column-index 0
                     :grid-pane/row-index 8
                     :grid-pane/column-span 2)

                   (ui/web-view
                     :id "web-browser"
                     :grid-pane/column-index 3
                     :grid-pane/row-index 0
                     :grid-pane/row-span 8)]))))

; The stage
(defui Stage
  (render [this args]
    (ui/stage
      :title "Tagger"
      :shown true
      :scene (ui/scene
               :on-key-pressed {:event :key-pressed
                                :fn-fx/include {:fn-fx/event #{:code :text :alt-down? :control-down? :meta-down? :shift-down? :shortcut-down?}}}
               :root (main-window args)))))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn rand-kv
  [m]
  (let [k (rand-nth (keys m))]
    [k (get m k)]))

(defn next-item
  [state]
  (if-not (empty? (:todo state))
    (let [[id doc] (rand-kv (:todo state))]
      {:id id :doc doc})
    nil))

(defmulti handle-event (fn [_ event] (:event event)))

(defmethod handle-event :set-language
  [state {:keys [language doc-id doc]}]
  (let [new-doc (assoc doc :manual-entry {:language language})
        new-state (-> state
                      (assoc-in [:done doc-id] new-doc)
                      (dissoc-in [:todo doc-id]))]
    (clear-browser)
    (assoc-in new-state [:current] (next-item new-state))))

(defmethod handle-event :load-url
  [state {:keys [url]}]
  (load-url url)
  state)

(defn get-button
  [id]
  (.lookup (.getScene @(:root @ui-state)) id))

(defmethod handle-event :key-pressed
  [state event]
  (let [{:keys [meta-down? text]}
        (-> event :fn-fx/includes :fn-fx/event)]
    (if meta-down?
      (case text
        "e" (.fire (get-button "#en-button"))
        "n" (.fire (get-button "#nl-button"))
        "u" (.fire (get-button "#url-button"))
        nil)))
  state)

(defmethod handle-event :default
  [state event]
  (println "No hander for event " (:type event) event)
  state)

(defn -main []
  (if (not (.exists (io/file "resources/state")))
    (create-initial-state-file))

  (let [;; data-state holds the current state of our docs and is setup to persist to disk
        data-state (init-state)

        ;; handler-fn handles events from ui and updates data-state
        handler-fn (fn [event]
                      (try
                        (swap! data-state handle-event event)
                        (catch Throwable ex
                          (println ex))))]

    ;; ui-state holds the most recent state of the ui
    (send ui-state (fn [old-state] (dom/app (stage @data-state) handler-fn)))

    ;; Every time data-state changes, update UI
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                  (fn [old-ui]
                                    (try
                                      (dom/update-app old-ui (stage @data-state))
                                      (catch Throwable ex
                                        (println ex)))))))

    ;; Update :current of the data-state to the first item to tag
    (if (empty? (:current @data-state))
      (swap! data-state assoc :current (next-item @data-state)))

    :started))

(comment
  (-main)
  nil)
