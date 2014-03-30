(ns inkwell-playground.core
  (:require [quil.core :refer :all]
            inkwell.core))

;; This is the state that the sketch starts with. You can return the sketch to
;; this state by pressing F12. See handle-event for how it's done!
(def initial-state {:alien {:position [100 100]
                            :scale 1
                            :rotation 0}
                    :tick-count 0})

;; These functions return new states based on the old state (and some additional
;; arguments). Note that they are pure functions: they do not *modify* the
;; state! They return new values without touching the old value.
(defn move-alien [state position]
  (assoc-in state [:alien :position] position))

(defn scale-alien [state amount]
  (update-in state [:alien :scale] * amount))

(defn rotate-alien [state amount]
  (update-in state [:alien :rotation] + amount))

(defn update-tick-count [state]
  (update-in state [:tick-count] inc))

;; Like the functions above, handle-event is a pure function that returns a new
;; state based on the old state and an event. In fact, it mostly just applies
;; the above functions to the old state.
(defn handle-event [state event]
  (case (:type event)
    :key-pressed (case (:key-name event)
                   :left (rotate-alien state -0.1)
                   :right (rotate-alien state 0.1)
                   ;; When F12 is pressed, we return initial-state. This gives
                   ;; us a simple way to reset the sketch to a known state.
                   :f12 initial-state
                   ;; When we don't want to change the state, we just return
                   ;; the old state.
                   state)
    :mouse-moved (move-alien state (:position event))
    :mouse-wheel (case (:direction event)
                   :up (scale-alien state 1.1)
                   :down (scale-alien state (/ 1 1.1)))
    :tick (update-tick-count state)
    state))

;; See resources/platformer and resources/space-shooter for more images by
;; Kenney Vleugels (www.kenney.nl)
(def alien-image (delay (load-image "platformer/character/front.png")))

;; draw is an impure function that takes a state, and calls Quil's drawing
;; functions to update the screen to match the state.
(defn draw [state]
  ;; We use the tick count to calculate the color with which we'll clear the
  ;; screen. Because the tick count increases after every frame, each frame
  ;; gets a background color different from the last.
  (background (+ 64 (* 64 (sin (* 0.01 (:tick-count state)))))
              (+ 64 (* 64 (sin (* 0.02 (:tick-count state)))))
              (+ 64 (* 64 (sin (* 0.03 (:tick-count state))))))
  (let [[x y] (get-in state [:alien :position])
        size (get-in state [:alien :scale])
        rotation (get-in state [:alien :rotation])]
    (translate x y)
    (rotate rotation)
    (scale size)
    (image-mode :center)
    (image @alien-image 0 0)))

(defn make-sketch! []
  (inkwell.core/make-sketch! {:title "Inkwell Playground"
                              :renderer :p2d
                              :draw #'draw
                              :handle-event #'handle-event
                              :initial-state initial-state}))
