(use ./environment /schema)
(setdyn *handler-defines* [:view])

(def collections/view
  "View collections"
  [:faculties :semesters :active-semester :courses
   :registrations :enrollments])

(defn <course/>
  "Contructs htmlgen representation of one `course`"
  [{:code code :name name :credits credits
    :semester semester :active active :enrolled enrolled}]
  @[[:tr {:id code}
     [:td code]
     [:td name]
     [:td credits]
     [:td semester]
     [:td {:class :active} (if active "x")]
     [:td
      (if enrolled
        [:a {:data-on:click (ds/get "/courses/enrolled/" code)}
         [:span (length enrolled)]])] # Issue #1
     [:td
      [:a {:data-on:click (ds/get "/courses/edit/" code)}
       "Edit"]]]
    [:tr {:id (string code "-enrolled")}]])

(def- init-ds (json/encode {:active false :semester "" :enrolled false}))

(defn <courses-list/>
  "Contructs htmlgen representation of all `courses`"
  [courses &opt open]
  (def filter-change (string "$search = ''; " (ds/get "/courses/filter/")))
  [:div {:id "courses"
         :data-bind init-ds}
   [:details (if open {:open true})
    [:summary
     "Courses (" (length courses) ")"]
    [:div {:class "margin-block"}
     (ds/input
       :search :type :search :size 50
       :placeholder "Search in the course code and name"
       :data-on:input__debounce.200ms
       (string "$active = false; $enrolled = false; $semester = ''; "
               (ds/post "/courses/search")))]
    [:div {:class "f-row margin-block"}
     "Filter: "
     [:label "Only active "
      (ds/input :active :type :checkbox
                :data-on:change filter-change)]
     [:label "Only enrolled "
      (ds/input :enrolled :type :checkbox
                :data-on:change filter-change)]
     [:label "Only Winter semester "
      (ds/input :semester :type :checkbox :value "Winter"
                :data-on:change filter-change)]
     [:label "Only Summer semester "
      (ds/input :semester :type :checkbox :value "Summer"
                :data-on:change filter-change)]]
    [:table
     [:thead
      [:tr [:th "code"] [:th {:class :name} "name"] [:th "credits"]
       [:th "semester"] [:th "active"] [:th "enrolled"] [:th "action"]]]
     [:tbody (seq [course :in courses] (<course/> course))]]]])

(defh /courses
  "Courses SSE stream"
  [check-session]
  (ds/hg-stream (<courses-list/> (view :courses))))

(defn <semesters-list/>
  "Contructs htmlgen representation of all `semesters`"
  [active-semester semesters &opt open]
  [:div {:id "semesters"}
   [:details (if open {:open true})
    [:summary "Semesters"]
    [:a {:data-on:click (ds/get "/semesters/deactivate")}
     "Deactivate"]
    [:table
     [:thead
      [:tr [:th "name"] [:th "active"] [:th "action"]]]
     [:tbody
      (seq [semester :in semesters :let [active? (= semester active-semester)]]
        [:tr
         [:td semester]
         [:td (if active? "x")]
         [:td
          (if-not active?
            [:a {:data-on:click (ds/get "/semesters/activate/" semester)}
             "Activate"])]])]]]])

(defh /semesters
  "Semesters SSE stream"
  [check-session]
  (ds/hg-stream
    (<semesters-list/> (view :active-semester)
                       (view :semesters))))

(defn ^activate
  "Events that activates semester"
  [semester]
  (make-event
    {:effect (fn [_ {:tree tree :view view} _]
               ((>put :active-semester semester) view)
               (:set-active-semester tree semester))
     :watch (^refresh-view :active-semester)}))

(defh /semesters/activate
  "Semesters SSE stream"
  []
  (def semester (params :semester))
  (produce (^activate semester))
  (ds/hg-stream (<semesters-list/> semester (view :semesters) true)))

(defn <course-form/>
  "Course form hg representation"
  [course semesters]
  (def {:code code} course)
  [:tr {:id code :data-signals (json/encode course)}
   [:td code]
   [:td (ds/input :name :type "text" :size 40)]
   [:td (ds/select :credits
                   (seq [c :range [1 6]] [:option c]))]
   [:td
    (ds/select :semester (seq [s :in semesters] [:option s]))]
   [:td
    (ds/input :active :type "checkbox")]
   [:td
    [:button {:data-on:click (ds/post "/courses/save" code)} "Save"]]])

(defh /courses/edit
  "Edit course SSE stream"
  [check-session]
  (def code (params :code))
  (def subject ((=>course/by-code code) view))
  (ds/hg-stream (<course-form/> subject (view :semesters))))

(defn ^save-course
  "Event that saves the course"
  [code course]
  (make-event
    {:effect (fn [_ {:tree tree} _]
               (:save-course tree code course))
     :watch (^refresh-view :courses)}))

(defh /courses/save
  "Save course"
  [check-session http/keywordize-body http/json->body]
  (def code (get params :code))
  (def course
    ((=> (=>course/by-code code)
         (>merge-into body)) view))
  (produce (^save-course code course))
  (ds/hg-stream (<course/> course)))

(define-event Deactivate
  "Events that deactivates semester"
  {:effect (fn [_ {:tree tree} _]
             (:set-active-semester tree false))
   :watch (^refresh-view :active-semester)})

(defh /semesters/deactivate
  "Deactivation handler"
  [check-session]
  (produce Deactivate)
  (ds/hg-stream (<semesters-list/> false (view :semesters) true)))

(def?! filterable (?one-of "semester" "active" "enrolled"))

(defh /courses/filter
  "Filtered courses SSE stream"
  [check-session http/query-params]
  (def finders
    ((=> :query-params "datastar"
         (>if present? json/decode (always {})) pairs
         (>Y (>check-all all
                         (=> first filterable?)
                         (=> last (>check-all some true? present?))))
         (>map (fn [[k v]] (>Y (=> (??? {(keyword k)
                                         (if (true? v) truthy? (?eq v))}))))))
      req))
  (ds/hg-stream
    (<courses-list/> ((=> :courses ;finders) view) true)))

(defn <enrolled/>
  "hg representation of enrolled students"
  [code enrolled]
  [:tr {:id (string code "-enrolled")}
   [:td {:colspan "7"}
    (if-not (empty? enrolled)
      [:a {:data-on:click (ds/get (string "/courses/clear/enrolled/" code))} "close"])
    (string/join (seq [{:email em} :in enrolled] em) ", ")]]) # Issue 2

(defh /courses/enrolled
  "Enrolled students for a course detail"
  [check-session]
  (def code (params :code))
  (def c @[])
  (def enrolled
    ((=> (<- c (=> :registrations))
         (=>course/by-code code) :enrolled
         (>reduce (fn [acc id] (array/push acc ((c 0) id)) acc) @[])) view))
  (ds/hg-stream (<enrolled/> code enrolled) (string "#" code "-enrolled") "outer"))

(defh /courses/clear/enrolled
  "Enrolled students for a course detail"
  [check-session]
  (def code (params :code))
  (def c @[])
  (ds/hg-stream (<enrolled/> code []) (string "#" code "-enrolled") "outer"))

(defh /courses/search
  "Search courses handler"
  [check-session http/keywordize-body http/json->body]
  (def search (body :search))
  (def =>search
    (=> :courses
        (>if (always (present? search))
             (=> pairs
                 (>map (=> last (enrich-fuzzy search :code :name)))
                 =>filter-sort-score))))
  (ds/hg-stream (<courses-list/> (=>search view) true)))

(def routes
  "HTTP routes"
  @{"/" (make/index "Admin" [:semesters :registrations :courses])
    "/logout" /logout
    "/registrations"
    @{"" /registrations
      "/search" /registrations/search
      "/filter/" /registrations/filter}
    "/semesters"
    @{"" /semesters
      "/activate/:semester" /semesters/activate
      "/deactivate" /semesters/deactivate}
    "/courses"
    @{"" /courses
      "/edit/:code" /courses/edit
      "/save/:code" /courses/save
      "/filter/" /courses/filter
      "/enrolled/:code" /courses/enrolled
      "/clear/enrolled/:code" /courses/clear/enrolled
      "/search" /courses/search}})

(defr +:refresh
  "RPC function that refreshes the view"
  [produce-resp ok-resp]
  (def [& what] args)
  (^refresh-view ;what))

(def rpc-funcs
  "RPC functions"
  @{:refresh +:refresh
    :stop close-peers-stop})

(def initial-state
  "Initial state"
  ((=> (=>symbiont-initial-state :admin)
       (>put :routes routes)
       (>update :rpc (update-rpc rpc-funcs))) compile-config))

(define-watch Start
  "Starts the machinery"
  [&]
  [(^prepare-view collections/view) (^register :tree)
   HTTP RPC Ready])

(defn main
  ```
  Main entry into student symbiont.
  ```
  [_ session]
  (-> initial-state
      (put :session session)
      (make-manager on-error)
      (:transact (^connect-peers Start Exit))
      :await)
  (os/exit 0))
