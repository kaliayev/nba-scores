(ns nba-scores.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [nba-scores.format :as f]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clj-http.client :as http])
  (:gen-class))

(def base-url
  "http://stats.nba.com")

(def user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")

(defn format-date
  [m]
  (let [int-month (Integer. (:month m))
        month (if (> 10 int-month)
                (str 0 int-month)
                (str int-month))
        int-day (Integer. (:day m))
        day (if (> 10 int-day)
              (str 0 int-day)
              (str int-day))]
    (str month "%2F" day "%2F" (:year m))))

(defn team-name
  [score-vec]
  (nth score-vec 4))

(defn team-city
  [score-vec]
  (nth score-vec 5))

(defn team-record
  [score-vec]
  (nth score-vec 6))

(defn team-score
  [score-vec]
  (nth score-vec 21))

(defn teams->maps
  [scores]
  (reduce (fn [acc v]
            (conj acc {:abbrev (team-name v)
                       :city (team-city v)
                       :record (team-record v)
                       :score (team-score v)}))
          [] scores))

(defn game-name
  [game-vec]
  (subs (nth game-vec 5) 9))

(defn game-quarter
  [game-vec]
  (nth game-vec 4))

(defn game-quarter-time
  [game-vec]
  (str/trim (nth game-vec 10)))

(defn games->maps
  [games]
  (reduce (fn [acc v]
            (conj acc {:game-data
                       {:game-name (keyword (game-name v))
                        :quarter (game-quarter v)
                        :quarter-time (game-quarter-time v)}}))
          [] games))

(defn build-url
  [{:keys [endpoint query-string]}]
  (format "%s/stats/%s/?%s" base-url endpoint query-string))

(defn get-box
  [date-map]
  (let [date (format-date date-map)
        query-string (str "DayOffset=0&LeagueID=00&GameDate=" date) 
        url (build-url {:query-string query-string
                        :endpoint "scoreboard"}) 
        referer (str base-url "/games/")]
    (http/get url {:headers {"Referer" referer
                             "User-Agent" user-agent}
                   :as :json})))

(defn scoreboard-data
  [date-map]
  (let [raw-data (-> (get-box date-map)
                     :body
                     :resultSets)
        games-data (games->maps  (-> raw-data
                                     (nth 0)
                                     :rowSet))
        teams-data (partition 2 (teams->maps (-> raw-data
                                                (nth 1)
                                                :rowSet)))
        game-team-map (zipmap games-data teams-data)]
    (reduce-kv (fn [acc k v]
                 (conj acc (assoc (:game-data k) :teams (into [] v))))
               [] game-team-map)))

(defn make-scoreboards
  [req]
  (let [date-map (:params req)
        ua (get-in req [:headers "user-agent"])
        curl? (or (re-find #"curl" ua)
                  (re-find #"wget" ua))
        scores-data (scoreboard-data date-map)
        _ (println scores-data)]
    (reduce (fn [acc m]
              (let [header (f/header-frame (str (:quarter m) " - " (:quarter-time m)) curl?)
                    matchup (f/matchup-frame m curl?)]
                (str acc header matchup)))
            "" scores-data)))


(defroutes app-routes
  (GET "/games" req (make-scoreboards req))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))
