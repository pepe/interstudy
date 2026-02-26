(import /environment :export true :prefix "")
(import /templates/app)
(import /templates/admin)


(setdyn *handler-defines* [:view])

(defn make/index
  "Creates index handler with `title`"
  [title colls]
  (fnh /index [appcap]
       [title (admin/capture :colls colls)]))

(defn check-session
  ```
  Checks if user cookie is in the session. If it is found  `next-middleware`
  is called. If the session is not found it exits.
  ```
  [next-middleware]
  (http/cookies
    (fn check-session [req]
      (define :view)
      (def sk (=>header-cookie req))
      (if-let [ck (and sk ((=> :session (?eq sk)) view))]
        (next-middleware (put req :session ck))
        (do
          (if-let [[peer arg] (dyn :spawn-after)]
            (produce (^write-spawn peer arg)))
          (produce Exit)
          (http/not-authorized))))))

(defh /logout
  "Handles lgout"
  []
  (if-let [[peer arg] (dyn :spawn-after)]
    (produce (^write-spawn peer arg)))
  (produce Stop)
  (http/response
    303 ""
    (merge {"Location" "/" "Content-Length" 0})))

(defn <registration/>
  "Contructs htmlgen representation of one `registration`"
  [emhash
   {:fullname fn :email em :home-university hu :faculty fa :timestamp ts}
   enrollment]
  (define :student)
  (def {:courses ecs :timestamp ets :credits ecr} (or enrollment {}))
  [:tr {:id emhash}
   [:td fn] [:td em] [:td hu] [:td fa]
   [:td [:small (if ts (dt/format-date-time ts))]]
   [:td {:class "f-coll"}
    (if ets
      [:small
       [:div (dt/format-date-time ets) " "]
       [:div (length ecs) " for " ecr " credits"]
       [:div
        (if (empty? ecs)
          ""
          (if (= (type (first ecs)) :string)
            (string/join ecs "; ")
            (string/join (seq [c :in ecs] (or (get c :code) (get c :id) (string c))) "; ")))]])
    [:a {:href (string student "/enroll/" (hash em))
         :target "_blank"} "Enroll link"]]]) # 3 Issue

(def- init-ds (json/encode {:search "" :enrolled false}))

(defn <registrations-list/>
  "Contructs htmlgen representation of all `registrations`"
  [registrations enrollments &opt open]
  (def filter-change (string "$search = ''; " (ds/get "/registrations/filter/")))
  [:div {:id "registrations"
         :data-bind init-ds}
   [:details (if open {:open true})
    [:summary
     "Registrations (" (length registrations) ")"]
    [:div {:class "margin-block"}
     (ds/input
       :search :type :search :size 50
       :placeholder "Search in email and fullname"
       :data-on:input__debounce.200ms (ds/post "/registrations/search"))]
    [:div {:class "f-row margin-block"}
     "Filter: "
     [:label "Only enrolled "
      (ds/input :enrolled :type :checkbox
                :data-on:change filter-change)]]
    [:table
     [:thead
      [:tr [:th "Fullname"] [:th "Email"]
       [:th "Home University"] [:th "Faculty"]
       [:th "Registered"] [:th "Enrollment"]]]
     [:tbody
      (seq [registration :in registrations
            :let [emhash (hash (registration :email))]]
        (<registration/> emhash registration (enrollments emhash)))]]]])

(defh /registrations
  "Registrations SSE stream"
  [check-session]
  (ds/hg-stream
    (<registrations-list/> (values (view :registrations)) (view :enrollments))))

(defh /registrations/search
  "Search registrations handler"
  [check-session http/keywordize-body http/json->body]
  (def search (body :search))
  (def =>search
    (=> :registrations
        (>if (always (present? search))
             (=> pairs
                 (>map (=> last (enrich-fuzzy search :email :fullname)))
                 =>filter-sort-score))))
  (ds/hg-stream
    (<registrations-list/> (=>search view) (view :enrollments) true)))

(def?! filterable (?one-of "enrolled"))

(defh /registrations/filter
  "Filtered registrations SSE stream"
  [check-session http/query-params]
  (def c @[])
  (def enrolled
    ((=> :query-params "datastar"
         (>if present? json/decode (always {})) "enrolled")
      req))
  (def registrations
    (if enrolled
      ((=> (<- c (=> :enrollments))
           :registrations
           |(tabseq [[emhash r] :pairs $ :when ((c 0) emhash)]
              emhash r)) view)
      (view :registrations)))
  (ds/hg-stream
    (<registrations-list/> registrations
                           (view :enrollments) true)))

(defn ^prepare-view
  "Initializes view and puts it in the dyn"
  [view-collections]
  (make-event
    {:update
     (fn [_ state]
       (def {:tree tree :session session} state)
       (put state :view
            (merge {:session session}
                   (tabseq [coll :in view-collections]
                     coll (coll tree)))))
     :effect (fn [_ {:view view :student student :guarded-by guarded-by} _]
               (setdyn :spawn-after [guarded-by ""])
               (setdyn :view view)
               (setdyn :student student))}))
