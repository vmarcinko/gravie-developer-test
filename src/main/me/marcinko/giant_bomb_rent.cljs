(ns me.marcinko.giant-bomb-rent
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [ajax.core :as ajax]))

(def page-limit 10)

(def state
  "Reagent atom that holds whole state"
  (r/atom {:page          :search
           :search-term   nil
           :rent-cart     []
           :search-result nil}))

(defn game [id name thumb-url]
  {:id        id
   :name      name
   :thumb-url thumb-url})

(defn parse-search-result [response]
  (let [games (map (fn [response-game]
                     (game (:id response-game)
                           (:name response-game)
                           (get-in response-game [:image :thumb_url])))
                   (:results response))]
    {:games             games
     :page-result-count (:number_of_page_results response)
     :total-count       (:number_of_total_results response)
     :offset            (:offset response)}))

(defn search-games! [page]
  (let [search-term (:search-term @state)]
    (.log js/console "Calling API for searching games")
    (ajax/GET
      "https://localhost:8081/api/search/"
      {:params          {:api_key   "46a802bbbb37f41d0151251d76bb3ef805dea50b"
                         :format    "json"
                         :resources "game"
                         :query     search-term
                         :limit     page-limit
                         :page      page}
       :response-format :json                               ; Accept header definition
       :keywords?       true
       :handler         (fn [response]
                          (.log js/console "Received API call response:" (clj->js response))
                          (let [search-result (parse-search-result response)]
                            (swap! state assoc :search-result search-result)))

       :error-handler   (fn [{:keys [status status-text] :as error-response}]
                          (.log js/console "Error while calling API:" error-response)
                          #_(set-error-msg! (if (= status 0)
                                              "Service unavailable at the moment"
                                              (str "Error occurred during HTTP request: " status-text))))}))
  )

(defn- js-event-target-value [ev]
  (-> ev .-target .-value))

(defn search-form []
  (let [text-value (:search-term @state)]
    [:form {:action    "#"
            :on-submit (fn [ev]
                         (.preventDefault ev)
                         (search-games! 1))}
     [:div.form-group
      [:input {:type        "text"
               :class       "form-control"
               :required    true
               :placeholder "Enter search text"
               :name        "search-text-field"
               :value       text-value
               :on-change   (fn [e]
                              (swap! state assoc :search-term (js-event-target-value e)))}]]
     [:button {:type "submit" :class "btn btn-primary active"} "Search"]]
    ))

(defn page-count
  [total-count]
  (+ (int (/ total-count page-limit))
     (if (pos? (rem total-count page-limit)) 1 0)))

(defn page [offset]
  (inc (int (/ offset page-limit))))

(defn paginator [total-count offset]
  (let [page-count (page-count total-count)
        page (page offset)]
    (when (> page-count 1)
      [:nav
       [:ul.pagination
        ;; previous button
        [:li {:class (str "page-item" (when (zero? offset) " disabled"))}
         [:a.page-link
          {:href "#" :on-click (fn [e] (search-games! (dec page))) :tabIndex "-1"}
          "Previous"]]

        ;; page by number
        (for [page-current (range 1 (inc page-count))]
          ^{:key page-current}
          [:li {:class (str "page-item" (when (= page-current page) " active"))}
           [:a.page-link
            {:href "#" :on-click (fn [e] (search-games! page-current))}
            (str page-current)]])

        ;; next button
        [:li {:class (str "page-item" (when (>= (+ offset page-limit) total-count) " disabled"))}
         [:a.page-link
          {:href "#" :on-click (fn [e] (search-games! (inc page)))}
          "Next"]]]]))
  )

(defn add-to-rent-cart! [game]
  (swap! state update :rent-cart conj game))

(defn remove-from-rent-cart! [game]
  (swap! state update :rent-cart
         (fn [cart] (remove #(= game %) cart))))

(defn in-cart? [game]
  (some #(= game %) (:rent-cart @state)))

(defn search-result-list [search-result]
  (when search-result
    [:div
     [:hr]
     [:div.h3 (let [total-count (:total-count search-result)]
                (if (zero? total-count)
                  "No results!"
                  (str "Games found: " total-count)))]
     [paginator (:total-count search-result) (:offset search-result)]
     [:ul.list-group
      (doall (for [game (:games search-result)]
               ^{:key game}
               [:li.list-group-item
                (if (in-cart? game)
                  [:button.btn.btn-danger.btn-secondary.btn-sm
                   {:type     "button"
                    :on-click (fn [e] (remove-from-rent-cart! game))}
                   "Remove from cart"]
                  [:button.btn.btn-success.btn-secondary.btn-sm
                   {:type     "button"
                    :on-click (fn [e] (add-to-rent-cart! game))}
                   "Add to cart"])
                [:div.h4 (:name game)]
                [:img {:src (:thumb-url game)}]
                ]
               ))]]))

(defn switch-to-page! [page]
  (swap! state assoc :page page))

(defn checkout-page []
  [:div.card
   [:div.card-body
    [:h5.card-title "Cart"]
    (if (empty? (:rent-cart @state))
      [:p.card-text "No games in the cart."]
      [:ul
       (doall (for [game (:rent-cart @state)]
                ^{:key game}
                [:li (:name game)]))]
      )
    [:button.btn.btn-success {:type     "button"
                              :disabled (empty? (:rent-cart @state))}
     "Rent"]

    [:button.btn.btn-warning {:type "button"
                              :on-click (fn [_] (switch-to-page! :search))}
     "Back to search"]]]
  )

(defn search-page []
  [:div.row
   ;; left side ..
   [:div.col-6
    [search-form]
    [search-result-list (:search-result @state)]]

   [:div.col-3]

   ;; right side ..
   [:div.col-3
    [:div.card
     [:div.card-body
      [:h5.card-title (str "Cart (" (count (:rent-cart @state)) " games)")]
      [:button.btn.btn-success {:type     "button"
                                :disabled (empty? (:rent-cart @state))
                                :on-click (fn [e] (switch-to-page! :checkout))}
       "Checkout"]]]
    ]
   ])

(defn main-layout []
  [:div.container-fluid
   [:div.row
    [:div.h1 "Giant Bomb Rent"]]

   (case (:page @state)
     :search [search-page]
     :checkout [checkout-page])
   ])

(defn ^:export run []
  (rdom/render [main-layout] (js/document.getElementById "root")))
