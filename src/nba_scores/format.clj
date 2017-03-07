(ns nba-scores.format
  (:require [clojure.string :as str]
            [io.aviso.ansi :as colour]))

(def team-colours
  {:ATL colour/bold-yellow
   :BOS colour/bold-green
   :BKN colour/bold-white
   :CHA colour/bold-cyan
   :CHI colour/bold-red
   :CLE colour/bold-red
   :DAL colour/bold-blue
   :DEN colour/bold-cyan
   :DET colour/bold-blue
   :GSW colour/bold-yellow
   :HOU colour/bold-red
   :IND colour/bold-yellow
   :LAC colour/bold-red 
   :LAL colour/bold-yellow
   :MEM colour/bold-green
   :MIA colour/bold-red
   :MIL colour/bold-green
   :MIN colour/bold-blue
   :NOP colour/bold-red
   :NYK colour/bold-blue
   :OKC colour/bold-cyan
   :ORL colour/bold-blue
   :PHI colour/bold-blue
   :PHX colour/bold-magenta
   :POR colour/bold-red
   :SAC colour/bold-magenta
   :SAS colour/bold-white
   :TOR colour/bold-magenta
   :UTA colour/bold-magenta
   :WAS colour/bold-blue})

(defn cap [text]
  (let [str-l (- (count text) 3)]
    (str "+" (apply str (repeat str-l "-")) "+\n")))

(defn header-frame
  [s curl?]
  (let [source-title (str "| " (str/upper-case s) " |\n") 
        formatted-title (str "| " (colour/bold-magenta (str/upper-case s)) " |\n") 
        cap (cap source-title)]
    (str "\n" cap formatted-title cap)))

(defn format-team-maps
  [[team-map1 team-map2]]
  (let [team1-name (:abbrev team-map1)
        team1-pts (or (:score team-map1) "--")
        team1-colour-fn ((keyword team1-name) team-colours)
        team1-score (str team1-name ": " team1-pts "    ")
        team1-score-colour (str (team1-colour-fn team1-name) ": " team1-pts "    ")
        #_(str (colour/bold-red team1-name) ": " (:score team-map1) "    ")
        team1-rec (str "(" (:record team-map1) ")") 
        team1-space (count team1-score)

        team2-name (:abbrev team-map2)
        team2-pts (or (:score team-map2) "--")
        team2-colour-fn ((keyword team2-name) team-colours)
        team2-score (str team2-name ": " team2-pts)
        team2-score-colour (str (team2-colour-fn team2-name) ": " team2-pts)
        #_(str (colour/bold-blue team2-name) ": " (:score team-map2))
        team2-rec (str "(" (:record team-map2) ")")
        team2-space (count team2-score)

        record-pad (apply str (repeat (- team1-space (count team1-rec)) " "))
        frame-width (+ 2 team1-space team2-space)]
    {:team1-score team1-score :team1-score-colour team1-score-colour :team1-record team1-rec
     :team2-score team2-score :team2-score-colour team2-score-colour :team2-record team2-rec
     :record-pad record-pad :frame-width frame-width}))

(defn matchup-frame
  [game-data
   curl?]
  (let [{:keys [team1-score team1-score-colour team1-record frame-width
                team2-score team2-score-colour team2-record record-pad]
         :as scoreboard-map} (format-team-maps (:teams game-data))
        
        scores (if curl? (str team1-score-colour team2-score-colour)
                   (str team1-score team2-score))
        score-row (str "| " scores " |")
        caps (str "+" (apply str (repeat frame-width "-")) "+")
        record-row (str "| " team1-record record-pad team2-record " |")]
    (str caps "\n"
         score-row "\n"
         record-row "\n"
         caps "\n")))
