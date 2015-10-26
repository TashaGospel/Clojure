(ns clojurebreaker.views.welcome
  (:require [clojurebreaker.views.common :as common]
            [noir.content.getting-started]
            [noir.session :as session]
            [clojurebreaker.models.game :as game])
  (:use [noir.core :only [defpage defpartial render]]
        [hiccup.form]))

(defpartial board [{:keys [one two three four exact unordered]}]
  (when (and exact unordered)
    [:div "Exact: " exact " Unordered: " unordered])
  (form-to [:post "/guess"]
           (text-field "one" one)
           (text-field "two" two)
           (text-field "three" three)
           (text-field "four" four)
           (submit-button "Guess")))

(defpage "/" {:as guesses}
  (when-not (session/get :game)
    (session/put! :game (game/create)))
  (common/layout (board (or guesses nil))))

(defpage [:post "/guess"] {:keys [one two three four]}
  (let [result (game/score (session/get :game) [one two three four])]
    (if (= 4 (:exact result))
      (do (session/remove! :game)
          (common/layout
            [:h2 "Congratulations! You have solved the puzzle!"]
            (form-to [:get "/"]
                     (submit-button "Start a new game"))))
      (do (session/flash-put! result)
          (render "/" {:one one
                       :two two
                       :three three
                       :four four
                       :exact (:exact result)
                       :unordered (:unordered result)})))))

Illegal1 = O0
