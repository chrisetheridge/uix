(ns uix.recipes
  (:require [uix.recipes.dynamic-styles :as dynamic-styles]
            [uix.recipes.lazy-loading :as lazy-loading]
            [uix.recipes.state-hook :as state-hook]
            [uix.recipes.global-state :as global-state]
            [uix.recipes.popup :as popup]
            [uix.recipes.interop :as interop]
            [uix.recipes.error-boundary :as error-boundary]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]
            #?(:cljs [cljs.loader :as loader])))

(def recipes
  {:dynamic-styles dynamic-styles/recipe
   :lazy-loading lazy-loading/recipe
   :state-hook state-hook/recipe
   :global-state global-state/recipe
   :popup popup/recipe
   :interop interop/recipe
   :error-boundary error-boundary/recipe})

(defn root []
  (let [current-recipe* (uix/state :state-hook)]
    [:div {:style {:padding 24}}
     [:div {:style {:margin-bottom 16}}
      [:span "Select recipe: "]
      [:select {:value @current-recipe*
                :on-change #(reset! current-recipe* (keyword (.. % -target -value)))}
       (for [[k v] recipes]
         ^{:key k}
         [:option {:value k}
          (name k)])]]
     (when-let [recipe (get recipes @current-recipe*)]
       [:div
        [recipe]])]))

#?(:cljs
   (defn ^:export render []
     (uix.dom/hydrate [root] js/root)))

#?(:cljs (loader/set-loaded! :recipes))
