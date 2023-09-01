(ns editor
  (:require [cljs.pprint]
            [reagent.core :as r]
            [reagent.dom]
            [malli.core :as m]
            [malli.transform :as mt]
            [malli.util :as mu]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default-value multimethod for initializing new entries

(defmulti default-value (fn [schema]
                          (when schema
                            (:type (m/ast schema)))))

(defmethod default-value :default [_] nil)
(defmethod default-value :string  [_] "")
(defmethod default-value :keyword [_] :k)
(defmethod default-value :vector  [_] [])
(defmethod default-value :boolean [_] false)
(defmethod default-value :int     [_] 0)
(defmethod default-value :double   [_] 0)

(defmethod default-value :map [schema]
  (into {}
        (keep (fn [[k properties value]]
                (when-not (:optional properties)
                  [k (default-value value)]))
              (m/children schema))))

(defmethod default-value :map-of [_] {})

(defmethod default-value :or [schema]
  (default-value (first (m/children schema))))

(defmethod default-value :orn [schema]
  (default-value (last (first (m/children schema)))))

(defmethod default-value :enum [schema]
  (first (m/children schema)))

(defmethod default-value :tuple [schema]
  (mapv default-value (m/children schema)))

(defmethod default-value :schema [schema]
  (default-value (m/deref schema)))

(defmethod default-value :malli.core/schema [schema]
  (default-value (m/deref schema)))

(defmethod default-value :ref [schema]
  (default-value (m/deref schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edit multimethod: editor ui per malli schema type

(defmulti edit (fn [schema _value _on-change]
                 (when schema
                   (:type (m/ast schema))))) ;; TODO is there a simpler way to check the type?

(defmethod edit :default [schema value on-change]
  [:div
   [:p "Default"]
   [:p (pr-str (m/ast schema))]
   [:p (pr-str value)]])

 ;; TODO validation errors
(defn input-field [schema value on-change]
  [:input {:type :text
           :value (m/encode schema value mt/string-transformer)
           :on-change #(on-change (m/decode schema (-> % .-target .-value) mt/string-transformer))}])

(defmethod edit :string [schema value on-change]
  [:div.malli-editor-string "\"" [input-field schema value on-change] "\""])

(defmethod edit :int [schema value on-change]
  [:div.malli-editor-int
   [input-field schema value on-change]])

 ;; TODO can't edit 3 -> 3.2 right now
(defmethod edit :double [schema value on-change]
  [:div.malli-editor-double
   [input-field schema value on-change]])

(defmethod edit :keyword [schema value on-change]
  [:div.malli-editor-keyword
   ":" [input-field schema value on-change]])

(defmethod edit :boolean [_schema value on-change]
  [:div.malli-editor-boolean
   [:input {:type :checkbox :checked value :on-change #(on-change (-> % .-target .-checked))}]])

(defmethod edit :enum [schema value on-change]
  [:div.malli-editor-enum
   (into [:select]
         (for [v (m/children schema)]
           [:option {:selected (= value v)
                     :on-click #(on-change v)}
            (pr-str v)]))])

(defn bracket [open close contents]
  [:div.malli-editor-brackets {:style {:display :flex}}
   [:div.malli-editor-bracket-open open]
   [:div.malli-editor-bracket-contents contents]
   [:div.malli-editor-bracket-close {:style {:align-self :flex-end}} close]])

(defn btn [text on-click]
  [:div.malli-editor-btn
   [:a {:href "#" :on-click on-click}
    text]])

(defn btn-plus [on-click]
  [btn "+" on-click])

(defn btn-minus [on-click]
  [btn "–" on-click])

(defmethod edit :maybe [schema value on-change]
  (let [child (first (m/children schema))]
    [:div.malli-editor-maybe {:style {:display :flex}}
     (if (nil? value)
       [:<>
        [btn-plus #(on-change (default-value child))]
        "nil"]
       [:<>
        [btn-minus #(on-change nil)]
        [edit child value on-change]])]))

(defmethod edit :map [schema value on-change]
  (let [map-schema (m/children schema)
        known-keys (->> map-schema
                        ;; mandatory keys first
                        (sort-by #(:optional (second %)))
                        (map first))
        {present-keys true missing-keys false} (group-by #(contains? value %) known-keys)
        extra-value (apply dissoc value known-keys)]
    [:div.malli-editor-map
     [bracket "{" "}"
      [:<>
       (for [k present-keys]
         (let [[_ properties value-schema] (mu/find schema k)]
           ^{:key (str k)}
           [:div.malli-editor-key-value {:style {:display :flex}}
            [:div.malli-editor-key (pr-str k)]
            (when (:optional properties)
              [btn-minus #(on-change (dissoc value k))])
            (when-let [[_ v] (find value k)]
              [:div.malli-editor-value {:style {:margin-left "0.5em"}}
               [edit value-schema v #(on-change (assoc value k %))]])]))
       [:div.malli-editor-add-keys {:style {:display :flex :flex-flow "row wrap"}}
        (for [k missing-keys]
          (let [value-schema (mu/get schema k)]
            ^{:key (str k)}
            [btn (str "<+" (pr-str k) ">") #(on-change (assoc value k (default-value value-schema)))]))]
       (when-not (empty? extra-value)
         [:div.malli-editor-extra-keys
          [:div.malli-editor-extra-keys-title ";; extra keys:"]
          (for [[k v] extra-value]
            ^{:key (str k)}
            [:div {:style {:display :flex}}
             [:div.malli-editor-key (pr-str k)]
             [btn-minus #(on-change (dissoc value k))]
             [:div.malli-editor-value {:style {:margin-left "0.5em"}}
              (pr-str v)]])])]]]))

(defmethod edit :map-of [schema value on-change]
  (let [[key-schema value-schema] (m/children schema)]
    [:div.malli-editor-map-of
     [bracket "{" "}"
      [:div
       (for [[k v] value]
         [:div.malli-editor-key-value {:style {:display :flex}}
          [btn-minus #(on-change (dissoc value k))]
          [:div.malli-editor-key
           [edit key-schema k #(on-change (-> value
                                              (dissoc k)
                                              (assoc % v)))]]
          [:div.malli-editor-value {:style {:margin-left "0.5em"}}
           [edit value-schema v #(on-change (assoc value k %))]]])
       ;; TODO what to do when key is already taken?
       [btn-plus #(on-change (assoc value (default-value key-schema) (default-value value-schema)))]]]]))

(defn- dissocv [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defmethod edit :tuple [schema value on-change]
  [:div.malli-editor-tuple
   [bracket "[" "]"
    (map-indexed (fn [i schema]
                   ^{:key i}
                   [edit schema (nth value i) #(on-change (assoc value i %))])
                 (m/children schema))]])

;; TODO: reordering vectors, adding elements in the middle
(defmethod edit :vector [schema value on-change]
  [:div.malli-editor-vector
   [bracket "[" "]"
    [:<>
     (map-indexed (fn [i v]
                    ^{:key i}
                    [:div {:style {:display :flex}}
                     [btn-minus #(on-change (dissocv value i))]
                     [edit (mu/get schema i) v #(on-change (assoc value i %))]])
                  value)
     [btn-plus #(on-change (conj (or value []) (default-value (mu/get schema 0))))]]]])

(defmethod edit :orn [schema value on-change]
  (let [nom (name (gensym "orn-schema"))
        p (m/parse schema value)
        matched-case (if (= :malli.core/invalid p)
                       ;; just pick the first case if input is invalid
                       ;; TODO figure out what to do here
                       (->> schema m/entries first key)
                       (first p))]
    [:div.malli-editor-orn
     (into [:div.malli-editor-orn-choices ";;"]
           (for [[k _p value-schema] (m/children schema)]
             (let [label (pr-str k)
                   id (str nom "--" label)]
               [:<>
                [:input {:type :radio :id id :name nom
                         :checked (= k matched-case)
                         :on-change (if (= k matched-case)
                                      (constantly nil) ;; silence react warning by always having a callback
                                      #(on-change (default-value value-schema)))}]
                [:label {:for id} label]])))
     [edit (mu/get schema matched-case) value on-change]]))

(defmethod edit :or [schema value on-change]
  (let [nom (name (gensym "or-schema"))
        children (for [c (m/children schema)]
                   {:schema c
                    :valid (m/validate c value)
                    :label (pr-str (:type (m/ast c)))})
        first-valid (first (filter :valid children))]
    [:div.malli-editor-or
     (into [:div.malli-editor-or-choices ";;"]
           (for [c children]
             (let [id (str nom "--" (:label c))]
               [:<>
                [:input {:type :radio :id id :name nom
                         :checked (:valid c)
                         :on-change (if (:valid c)
                                      (constantly nil) ;; silence reagent warning by always having a callback
                                      #(on-change (default-value (:schema c))))}]
                [:label {:for id} (:label c)]])))
     (if first-valid
       [edit (:schema first-valid) value on-change]
       [:p (pr-str value)])]))

(defmethod edit :schema [schema value on-change]
  [edit (m/deref schema) value on-change])

(defmethod edit :malli.core/schema [schema value on-change]
  [edit (m/deref schema) value on-change])

(defmethod edit :ref [schema value on-change]
  [edit (m/deref schema) value on-change])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main entry points

;; TODO: support overriding editors per schema
(defn editor [schema value on-change]
  [:div.malli-editor
   [edit schema value on-change]])

(defn editor-in [schema value path on-change]
  [:div.malli-editor
   [edit (mu/get-in schema path) (get-in value path) #(on-change (assoc-in value path %))]])

(def example-schema
  [:map
   [:name :string]
   [:address
    [:orn
     [:structured [:map
                   [:street :string]
                   [:number {:optional true} :int]]]
     [:raw :string]]]
   [:items
    [:vector
     [:map
      [:item [:enum :fork :spade :pipe]]
      [:price {:optional true} :double]
      [:in-stock {:optional true} :boolean]]]]

   [:instructions [:or
                   :string
                   [:vector :string]]]
   [:metadata [:map-of :keyword :string]]
   [:foo [:tuple :int :string :boolean]]])

(def example-value
  {:name "Old Gaffer"

   :address {:street "Bagshot Row"
             :number 1
             :additional-key "hello"}
   :items [{:item :spade
            :price 3.2
            :in-stock true}
           {:item :pipe
            :price 2.7
            :in-stock false}]
   :instructions ["please" "send" "help"]
   :foo [1 "hello" false]})

(defn example [schema initial-value]
  (let [value (r/atom initial-value)]
    (fn []
      [:div.malli-editor-example
       [:div {:style {:display :flex}}
        [:div
         [:h2 "Editor"]
         [editor schema @value #(reset! value %)]]
        [:div
         [:h2 "Value"]
         [:pre (with-out-str (cljs.pprint/pprint @value))]]
        [:div
         [:h2 "Schema"]
         [:pre (with-out-str (cljs.pprint/pprint schema))]]]])))

(defn ^:dev/after-load init []
  (reagent.dom/render
   [example example-schema example-value]
   (.getElementById js/document "root")))
