(ns inkwell-playground.core
  (:require [quil.core :refer :all]
            inkwell.core))

(def window-size [1200 800])

(defn rand-float
  [min max]
  (+ min (* (- max min) (rand))))

(defn vector-length [[x y]]
  (sqrt (+ (* x x) (* y y))))

(defn +v [[x1 y1] [x2 y2]]
  [(+ x1 x2)
   (+ y1 y2)])

(defn rotated
  ([[x y] rotation]
    (rotated [0 0] [x y] rotation))
  ([[x0 y0] [x1 y1] rotation]
    (let [s (sin rotation)
          c (cos rotation)
          dx (- (* x1 c) (* y1 s))
          dy (+ (* x1 s) (* y1 c))]
      [(+ x0 dx) (+ y0 dy)])))

(defn new-meteor [position type]
  {:position position
   :type type
   :rotation 0
   :rotation-speed (case type
                     :big (rand-float 0 0.1)
                     :small (rand-float 0.1 0.2))
   :speed (case type
            :big [(rand-float -1.5 1.5) (rand-float -1.5 1.5)]
            :small [(rand-float -2.5 2.5) (rand-float -2.5 2.5)])
   :size (case type
           :big 50
           :small 18)})

;; This is the state that the sketch starts with. You can return the sketch to
;; this state by pressing F12. See handle-event for how it's done!
(def initial-state {:player {:position [300 400]
                             :speed [0 0]
                             :rotation HALF-PI
                             :rotation-speed 0
                             :moving false
                             :turning nil
                             :side :left
                             :size 45}
                    :meteors [(new-meteor [300 100] :big)
                              (new-meteor [500 300] :big)
                              (new-meteor [1000 700] :big)
                              (new-meteor [800 200] :big)]
                    :bullets []
                    :tick-count 0})

;; These functions return new states based on the old state (and some additional
;; arguments). Note that they are pure functions: they do not *mod

(defn move-item [{:keys [position speed rotation rotation-speed] :as item}]
  (let [[size-x size-y] window-size
        [new-x new-y] (+v position speed)
        new-rotation (mod (+ rotation rotation-speed) TWO-PI)
        ttl (some-> item :ttl dec)]
    (cond-> (assoc item :position [(mod new-x size-x) (mod new-y size-y)]
                        :rotation new-rotation)
            ttl (assoc :ttl ttl))))

(defn update-tick-count [state]
  (update-in state [:tick-count] inc))

(defn update-meteors [state]
  (let [rotation (mod (/ (:tick-count state) 100) TWO-PI)]
    (-> state
      (update-in [:meteors] #(map (fn [meteor] (assoc meteor :rotation rotation)) %))
      (update-in [:meteors] #(map move-item %)))))

(defn set-player-speed-newtonian [state]
  (let [speed (get-in state [:player :speed])
        rotation (get-in state [:player :rotation])
        rotation-speed (get-in state [:player :rotation-speed])
        turning (get-in state [:player :turning])
        moving (get-in state [:player :moving])
        new-speed (if moving
                    (+v speed (rotated [0 -0.2] rotation))
                    speed)
        new-rotation-speed (case turning
                             :left (- rotation-speed 0.001)
                             :right (+ rotation-speed 0.001)
                             rotation-speed)]
    (-> state
      (assoc-in [:player :speed] new-speed)
      (assoc-in [:player :rotation-speed] new-rotation-speed))))

(defn set-player-speed [state]
  (let [speed (vector-length (get-in state [:player :speed]))
        rotation (get-in state [:player :rotation])
        turning (get-in state [:player :turning])
        moving (get-in state [:player :moving])
        new-speed (if moving
                    (min 10 (+ speed 0.2))
                    (max 0 (- speed 0.2)))
        new-rotation-speed (case turning
                             :left (* new-speed -0.01)
                             :right (* new-speed 0.01)
                             0)
        new-speed-vector (rotated [0 (- new-speed)] rotation)]
    (-> state
      (assoc-in [:player :speed] new-speed-vector)
      (assoc-in [:player :rotation-speed] new-rotation-speed))))

(defn move-player [state]
  (update-in state [:player] move-item))

(defn move-bullets [state]
  (update-in state [:bullets] #(map move-item %)))

(defn turn-player [state direction]
  (assoc-in state [:player :turning] direction))

(defn start-stop-player [state moving]
  (assoc-in state [:player :moving] moving))

(defn shoot [state]
  (let [{:keys [side rotation position]} (:player state)
        bullet {:position (case side
                            :left (rotated position [-45 -20] rotation)
                            :right (rotated position [45 -20] rotation))
                :rotation rotation
                :rotation-speed 0
                :speed (rotated [0 -30] rotation)
                :size 0
                :ttl 50}
        new-side (case side
                   :left :right
                   :right :left)]
    (-> state
      (update-in [:bullets] conj bullet)
      (assoc-in [:player :side] new-side))))

(defn cull-bullets [state]
  (update-in state [:bullets] #(remove (comp zero? :ttl) %)))

(defn squared-distance [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)]
    (+ (* dx dx) (* dy dy))))

(defn collides? [item1 item2]
  (let [[x0 y0] (:position item1)
        s1 (:size item1)
        [x1 y1] (:position item2)
        s2 (:size item2)]
    (> (* (+ s1 s2) (+ s1 s2)) (squared-distance x0 y0 x1 y1))))

(defn some-collides [item coll]
  (some (partial collides? item) coll))

(defn new-meteors-and-bullets [state]
  (let [new-meteors (apply concat (for [meteor (:meteors state)]
                                  (if (some-collides meteor (:bullets state))
                                    (if (= :big (:type meteor))
                                      [(new-meteor (:position meteor) :small)
                                       (new-meteor (:position meteor) :small)
                                       (new-meteor (:position meteor) :small)]
                                      [])
                                    [meteor])))
        new-bullets (remove #(some-collides % (:meteors state)) (:bullets state))]
    (assoc state :bullets new-bullets
                 :meteors new-meteors)))

(defn check-collisions [state]
  (cond-> (new-meteors-and-bullets state)
    (some-collides (:player state) (:meteors state))
      (dissoc :player)))

;; Like the functions above, handle-event is a pure function that returns a new
;; state based on the old state and an event. In fact, it mostly just applies
;; the above functions to the old state.
(defn handle-event [state event]
  (case (:type event)
    :key-pressed (case (:key-name event)
                   ;; When F12 is pressed, we return initial-state. This gives
                   ;; us a simple way to reset the sketch to a known state.
                   :f12 initial-state
                   :up (start-stop-player state true)
                   :left (turn-player state :left)
                   :right (turn-player state :right)
                   ;; When any other key is pressed, we don't want to change the
                   ;; state, so we just return the old state.
                   (case (:key-code event)
                     32 (shoot state)
                     10 (update-in state [:newtonian] not)
                     state))
    :key-released (case (:key-name event)
                    :up (start-stop-player state false)
                    :left (turn-player state nil)
                    :right (turn-player state nil)
                    state)
    :tick (let [set-player-speed-fn (if (:newtonian state)
                                      #'set-player-speed-newtonian
                                      #'set-player-speed)]
            (-> state
              update-tick-count
              update-meteors
              set-player-speed-fn
              move-player
              move-bullets
              cull-bullets
              check-collisions))
    state))

;; See directories:
;; - resources/platformer
;; - resources/space-shooter
;; for more images by Kenney Vleugels (www.kenney.nl)
(def bg-image (delay (load-image "space-shooter/background.jpg")))
(def player-image {nil (delay (load-image "space-shooter/player.png"))
                   :left (delay (load-image "space-shooter/playerLeft.png"))
                   :right (delay (load-image "space-shooter/playerRight.png"))})
(def meteor-image-big (delay (load-image "space-shooter/meteorBig.png")))
(def meteor-image-small (delay (load-image "space-shooter/meteorSmall.png")))
(def jet-image (delay (load-image "space-shooter/jetFlame1.png")))
(def bullet-image (delay (load-image "space-shooter/laserRed.png")))

(defn draw-component [c img & [cs]]
  (push-matrix)
  (apply translate (:position c))
  (rotate (:rotation c))
  (image @img 0 0)
  (doseq [[comp img] cs]
    (draw-component comp img))
  (pop-matrix))

(def jets
  [[{:position [-18 45]
     :rotation 0}
    jet-image]
   [{:position [-28 42]
    :rotation 0}
    jet-image]
   [{:position [18 45]
    :rotation 0}
    jet-image]
   [{:position [28 42]
    :rotation 0}
    jet-image]])

;; draw is an impure function that takes a state, and calls Quil's drawing
;; functions to update the screen to match the state.
(defn draw [state]
  (background 0 0 0)
  (image-mode :center)
  (draw-component (:player state) (player-image (get-in state [:player :turning]))
                  (when (get-in state [:player :moving])
                    jets))
  (doseq [meteor (:meteors state)
          :let [image (case (:type meteor)
                        :big meteor-image-big
                        :small meteor-image-small)]]
    (draw-component meteor image))
  (doseq [bullet (:bullets state)]
    (draw-component bullet bullet-image)))

(defn make-sketch! []
  (inkwell.core/make-sketch! {:title "Inkwell Playground"
                              :renderer :p2d
                              :draw #'draw
                              :size window-size
                              :handle-event #'handle-event
                              :initial-state initial-state}))
