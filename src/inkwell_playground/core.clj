(ns inkwell-playground.core
  (:require [quil.core :refer :all]
            inkwell.core))

(def window-size [640 480])

(def bullet-speed 15)

;; This is the state that the sketch starts with. You can return the sketch to
;; this state by pressing F12. See handle-event for how it's done!
(def initial-state {:player [300 400]
                    :enemy [300 100]
                    :bullets []
                    :tick-count 0
                    :firing? false})

;; These functions return new states based on the old state (and some additional
;; arguments). Note that they are pure functions: they do not *modify* the
;; state! They return new values without touching the old value.
(defn update-tick-count [state]
  (update-in state [:tick-count] inc))

(defn set-ship-x [state who x]
  (assoc-in state [who 0] x))

(defn move-enemy [state]
  (let [new-x (+ 320 (* 200 (sin (/ (:tick-count state) 100))))]
    (set-ship-x state :enemy new-x)))

(defn move-bullet [bullet]
  (update-in bullet [1] - bullet-speed))

(defn move-bullets [state]
  (update-in state [:bullets] #(map move-bullet %)))

(defn outside-screen? [thing]
  (neg? (thing 1)))

(defn cull-bullets [state]
  (update-in state [:bullets] #(remove outside-screen? %)))

(defn fire [state]
  (update-in state [:bullets] conj (:player state)))

(defn rapid-fire [state]
  (if (and (:firing? state)
           (zero? (mod (:tick-count state) 5)))
    (fire state)
    state))

(defn start-firing [state]
  (assoc state :firing? true))

(defn stop-firing [state]
  (assoc state :firing? false))

;; Like the functions above, handle-event is a pure function that returns a new
;; state based on the old state and an event. In fact, it mostly just applies
;; the above functions to the old state.
(defn handle-event [state event]
  (case (:type event)
    :key-pressed (case (:key-name event)
                   ;; When F12 is pressed, we return initial-state. This gives
                   ;; us a simple way to reset the sketch to a known state.
                   :f12 initial-state
                   ;; When any other key is pressed, we don't want to change the
                   ;; state, so we just return the old state.
                   state)
    :mouse-moved (let [mouse-x (get-in event [:position 0])]
                   (set-ship-x state :player mouse-x))
    :mouse-pressed (start-firing state)
    :mouse-released (stop-firing state)
    :tick (-> state
            update-tick-count
            move-enemy
            rapid-fire
            move-bullets
            cull-bullets)
    state))

;; See directories:
;; - resources/platformer
;; - resources/space-shooter
;; for more images by Kenney Vleugels (www.kenney.nl)
(def bg-image (delay (load-image "space-shooter/background.jpg")))
(def player-image (delay (load-image "space-shooter/player.png")))
(def enemy-image (delay (load-image "space-shooter/enemyShip.png")))
(def bullet-image (delay (load-image "space-shooter/laserRed.png")))

;; draw is an impure function that takes a state, and calls Quil's drawing
;; functions to update the screen to match the state.
(defn draw [state]
  (no-cursor)

  (image-mode :corner)
  (image @bg-image 0 0)

  (image-mode :center)

  (doseq [bullet (:bullets state)]
    (push-matrix)
    (apply translate bullet)
    (image-mode :center)
    (image @bullet-image 0 0)
    (pop-matrix))

  (push-matrix)
  (apply translate (:player state))
  (image @player-image 0 0)
  (pop-matrix)

  (push-matrix)
  (apply translate (:enemy state))
  (image-mode :center)
  (image @enemy-image 0 0)
  (pop-matrix))

(defn make-sketch! []
  (inkwell.core/make-sketch! {:title "Inkwell Playground"
                              :renderer :p2d
                              :draw #'draw
                              :size window-size
                              :handle-event #'handle-event
                              :initial-state initial-state}))
