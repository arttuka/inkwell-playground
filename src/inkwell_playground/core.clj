(ns inkwell-playground.core
  (:require [quil.core :refer :all]
            inkwell.core))

(def window-size [1200 800])

(defn rand-float
  [min max]
  (+ min (* (- max min) (rand))))

;; This is the state that the sketch starts with. You can return the sketch to
;; this state by pressing F12. See handle-event for how it's done!
(def initial-state {:player {:position [300 400]
                             :rotation 0
                             :speed 0
                             :moving :decelerate
                             :direction 0
                             :turning nil
                             :side :left}
                    :meteors [{:position [300 100]
                              :rotation 0
                              :direction (rand-float 0 TWO-PI)
                              :speed (rand-float 1 3)
                              :type :big}
                              {:position [500 300]
                              :rotation 0
                              :direction (rand-float 0 TWO-PI)
                              :speed (rand-float 1 3)
                              :type :big}
                              {:position [1000 700]
                              :rotation 0
                              :direction (rand-float 0 TWO-PI)
                              :speed (rand-float 1 3)
                              :type :big}
                              {:position [800 200]
                              :rotation 0
                              :direction (rand-float 0 TWO-PI)
                              :speed (rand-float 1 3)
                              :type :big}]
                    :bullets []
                    :tick-count 0})

;; These functions return new states based on the old state (and some additional
;; arguments). Note that they are pure functions: they do not *modify* the
;; state! They return new values without touching the old value.
(defn update-tick-count [state]
  (update-in state [:tick-count] inc))

(defn item-rotated [[x0 y0] speed direction]
  (let [dx (* speed (sin direction))
        dy (* speed (cos direction))]
    [(+ x0 dx) (- y0 dy)]))

(defn move-item [item]
  (let [{:keys [position direction speed]} item
        [size-x size-y] window-size
        [new-x new-y] (item-rotated position speed direction)
        ttl (some-> item :ttl dec)]
    (cond-> (assoc item :position [(mod new-x size-x) (mod new-y size-y)])
            ttl (assoc :ttl ttl))))

(defn update-meteors [state]
  (let [rotation (mod (/ (:tick-count state) 100) TWO-PI)]
    (-> state
      (update-in [:meteors] #(map (fn [meteor] (assoc meteor :rotation rotation)) %))
      (update-in [:meteors] #(map move-item %)))))

(defn set-player-speed [state]
  (let [speed (get-in state [:player :speed])
        moving (get-in state [:player :moving])
        new-speed (case moving
                    :accelerate (min 10 (+ speed 0.2))
                    :decelerate (max 0 (- speed 0.2)))]
    (assoc-in state [:player :speed] new-speed)))

(defn move-player [state]
  (update-in state [:player] move-item))

(defn move-bullets [state]
  (update-in state [:bullets] #(map move-item %)))

(defn rotated [[x0 y0] [x1 y1] rotation]
  (let [s (sin rotation)
        c (cos rotation)
        dx (- (* x1 c) (* y1 s))
        dy (+ (* x1 s) (* y1 c))]
    [(+ x0 dx) (+ y0 dy)]))

(defn rotate-player [state]
  (let [{:keys [turning speed]} (:player state)]
    (-> (case turning
          :left (-> state
                  (update-in [:player :direction] - (* speed 0.01))
                  (update-in [:player :rotation] - (* speed 0.01)))
          :right (-> state
                  (update-in [:player :direction] + (* speed 0.01))
                  (update-in [:player :rotation] + (* speed 0.01)))
          state)
      (update-in [:player :rotation] mod TWO-PI)
      (update-in [:player :direction] mod TWO-PI))))

(defn turn-player [state direction]
  (assoc-in state [:player :turning] direction))

(defn start-stop-player [state moving]
  (assoc-in state [:player :moving] moving))

(defn shoot [state]
  (let [{:keys [side direction position]} (:player state)
        bullet {:position (case side
                            :left (rotated position [-45 -20] direction)
                            :right (rotated position [45 -20] direction))
                :rotation direction
                :direction direction
                :speed 20
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
        [x1 y1] (:position item2)]
    (> 5000 (squared-distance x0 y0 x1 y1))))

(defn new-meteors-and-bullets [state]
  (let [new-meteors (apply concat (for [meteor (:meteors state)]
                                  (if (some (partial collides? meteor) (:bullets state))
                                    (if (= :big (:type meteor))
                                      (do
                                        [{:position (:position meteor)
                                         :type :small
                                         :direction (rand-float 0 TWO-PI)
                                         :rotation 0
                                         :speed (rand-float 2 5)}
                                        {:position (:position meteor)
                                         :type :small
                                         :direction (rand-float 0 TWO-PI)
                                         :rotation 0
                                         :speed (rand-float 2 5)}
                                        {:position (:position meteor)
                                         :type :small
                                         :direction (rand-float 0 TWO-PI)
                                         :rotation 0
                                         :speed (rand-float 2 5)}])
                                      nil)
                                    [meteor])))
        new-bullets (remove #(some (partial collides? %) (:meteors state)) (:bullets state))]
    (assoc state :bullets new-bullets
                 :meteors new-meteors)))

(defn check-collisions [state]
  (cond-> (new-meteors-and-bullets state)
    (some (partial collides? (:player state)) (:meteors state))
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
                   :up (start-stop-player state :accelerate)
                   :left (turn-player state :left)
                   :right (turn-player state :right)
                   ;; When any other key is pressed, we don't want to change the
                   ;; state, so we just return the old state.
                   (case (:key-code event)
                     32 (shoot state)
                     state))
    :key-released (case (:key-name event)
                    :up (start-stop-player state :decelerate)
                    :left (turn-player state nil)
                    :right (turn-player state nil)
                    state)
    :tick (-> state
            update-tick-count
            update-meteors
            rotate-player
            set-player-speed
            move-player
            move-bullets
            cull-bullets
            check-collisions)
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
                  (when (= :accelerate (get-in state [:player :moving]))
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
