(ns inkwell-playground.core
  (:require [quil.core :refer :all]
            inkwell.core))

(def window-size [640 480])

;; This is the state that the sketch starts with. You can return the sketch to
;; this state by pressing F12. See handle-event for how it's done!
(def initial-state {:player [300 400]
                    :enemy [300 100]
                    :tick-count 0})

;; These functions return new states based on the old state (and some additional
;; arguments). Note that they are pure functions: they do not *modify* the
;; state! They return new values without touching the old value.
(defn update-tick-count [state]
  (update-in state [:tick-count] inc))

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
    :tick (update-tick-count state)
    state))

;; See directories:
;; - resources/platformer
;; - resources/space-shooter
;; for more images by Kenney Vleugels (www.kenney.nl)
(def player-image (delay (load-image "platformer/character/front.png")))

;; draw is an impure function that takes a state, and calls Quil's drawing
;; functions to update the screen to match the state.
(defn draw [state]
  (background 0 0 0)
  (apply translate (:player state))
  (image-mode :center)
  (image @player-image 0 0))

(defn make-sketch! []
  (inkwell.core/make-sketch! {:title "Inkwell Playground"
                              :renderer :p2d
                              :draw #'draw
                              :size window-size
                              :handle-event #'handle-event
                              :initial-state initial-state}))
