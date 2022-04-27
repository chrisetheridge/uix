(ns uix.hooks.linter
  (:require [clojure.walk]
            [cljs.analyzer :as ana]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [cljs.analyzer.api :as ana-api])
  (:import (cljs.tagged_literals JSValue)
           (java.io Writer)))

;; === Rules of Hooks ===

(def ^:dynamic *component-context* nil)
(def ^:dynamic *source-context* false)
(def ^:dynamic *in-branch?* false)
(def ^:dynamic *in-loop?* false)

(defn hook? [sym]
  (and (symbol? sym)
       (some? (re-find #"^use-|use[A-Z]" (name sym)))))

(defn effect-hook? [form]
  (contains? #{"use-effect" "use-layout-effect"} (name (first form))))

(declare lint-hooks!*)

(def forms
  '{:when #{when when-not when-let when-some when-first}
    :if #{if if-not if-let if-some}
    :logical #{and or}
    :cond #{cond}
    :cond-threaded #{cond-> cond->>}
    :some-threaded #{some-> some->>}
    :condp #{condp}
    :case #{case}
    :loop #{loop}
    :for  #{for doseq}
    :iter-fn #{map mapv map-indexed filter filterv reduce reduce-kv keep keep-indexed
               remove mapcat drop-while take-while group-by partition-by split-with
               sort-by some}})

(defmulti maybe-lint
          (fn [[sym :as form]]
            (reduce-kv
              (fn [ret kw forms]
                (if (forms sym)
                  (reduced kw)
                  ret))
              form
              forms)))

(defmethod maybe-lint :default [form]
  form)

(defmethod maybe-lint :when [[_ test & body]]
  (lint-hooks!* test :in-branch? false)
  (run! #(lint-hooks!* % :in-branch? true) body))

(defmethod maybe-lint :if [[_ test then else]]
  (lint-hooks!* test :in-branch? false)
  (lint-hooks!* then :in-branch? true)
  (lint-hooks!* else :in-branch? true))

(defmethod maybe-lint :logical [[_ test & tests]]
  (lint-hooks!* test :in-branch? false)
  (run! #(lint-hooks!* % :in-branch? true) tests))

(defmethod maybe-lint :cond [[_ clause & clauses]]
  (lint-hooks!* clause :in-branch? false)
  (run! #(lint-hooks!* % :in-branch? true) clauses))

(defmethod maybe-lint :condp [[_ pred e clause & clauses]]
  (lint-hooks!* pred :in-branch? false)
  (lint-hooks!* e :in-branch? false)
  (lint-hooks!* clause :in-branch? false)
  (run! #(lint-hooks!* % :in-branch? true) clauses))

(defmethod maybe-lint :cond-threaded [[_ e & clauses]]
  (lint-hooks!* e :in-branch? false)
  (->> (partition 2 clauses)
       (run! (fn [[test expr]]
               (lint-hooks!* test :in-branch? false)
               (lint-hooks!* expr :in-branch? true)))))

(defmethod maybe-lint :some-threaded [[_ e clause & clauses]]
  (lint-hooks!* e :in-branch? false)
  (lint-hooks!* clause :in-branch? false)
  (run! #(lint-hooks!* % :in-branch? true) clauses))

(defmethod maybe-lint :case [[_ e clause & clauses]]
  (lint-hooks!* e :in-branch? false)
  (lint-hooks!* clause :in-branch? false)
  (run! #(lint-hooks!* % :in-branch? true) clauses))

(defmethod maybe-lint :loop [[_ bindings & body]]
  (lint-hooks!* bindings :in-loop? false)
  (run! #(lint-hooks!* % :in-loop? true) body))

(defmethod maybe-lint :for [[_ bindings & body]]
  (let [[binding & bindings] (partition 2 bindings)]
    (lint-hooks!* (second binding) :in-loop? false)
    (run! (fn [[v expr]] (lint-hooks!* expr :in-loop? true))
          bindings)
    (run! #(lint-hooks!* % :in-loop? true) body)))

(defmethod maybe-lint :iter-fn [[_ f :as form]]
  (when (and (list? f)
             ('#{fn fn*} (first f))
             (vector? (second f)))
    (let [[_ _ & body] f]
      (run! #(lint-hooks!* % :in-loop? true) body))))

(defn add-error! [form type]
  (swap! *component-context* update :errors conj {:source form
                                                  :source-context *source-context*
                                                  :type type}))

(defn lint-hooks!*
  [expr & {:keys [in-branch? in-loop?]
           :or {in-branch? *in-branch?*
                in-loop? *in-loop?*}}]
  (binding [*in-branch?* in-branch?
            *in-loop?* in-loop?]
    (clojure.walk/prewalk
      (fn [form]
        (cond
          (and (list? form) (hook? (first form)))
          (do (when *in-branch?* (add-error! form ::hook-in-branch))
              (when *in-loop?* (add-error! form ::hook-in-loop))
              nil)

          (and (list? form) (or (not *in-branch?*) (not *in-loop?*)))
          (binding [*source-context* form]
            (maybe-lint form))

          :else form))
      expr)
    nil))

(defn lint-hooks! [exprs]
  (run! lint-hooks!* exprs))

(defmethod ana/error-message ::hook-in-branch [_ {:keys [name column line source]}]
  ;; https://github.com/facebook/react/blob/d63cd972454125d4572bb8ffbfeccbdf0c5eb27b/packages/eslint-plugin-react-hooks/src/RulesOfHooks.js#L457
  (str "React Hook " source " is called conditionally.\n"
       "React Hooks must be called in the exact same order in every component render.\n"
       "Found in " name ", at " line ":" column))

(defmethod ana/error-message ::hook-in-loop [_ {:keys [name column line source]}]
  ;; https://github.com/facebook/react/blob/d63cd972454125d4572bb8ffbfeccbdf0c5eb27b/packages/eslint-plugin-react-hooks/src/RulesOfHooks.js#L438
  (str "React Hook " source " may be executed more than once. "
       "Possibly because it is called in a loop. "
       "React Hooks must be called in the exact same order in "
       "every component render.\n"
       "Found in " name ", at " line ":" column))

(defn lint! [sym form env]
  (binding [*component-context* (atom {:errors []})]
    (lint-hooks! form)
    (let [{:keys [errors]} @*component-context*
          {:keys [column line]} env]
      (run! #(ana/warning (:type %) env (into {:name (str (-> env :ns :name) "/" sym)
                                               :column column
                                               :line line}
                                              %))
            errors))))

;; === Exhaustive Deps ===

(defn find-local-variables [env f]
  (let [syms (atom #{})]
    (clojure.walk/postwalk
      #(cond
         (symbol? %)
         (do (swap! syms conj %)
             %)

         (= (type %) JSValue)
         (.-val %)

         :else %)
      f)
    (filter #(get-in env [:locals % :name]) @syms)))

(defn find-free-variables [env f deps]
  (let [all-local-variables (find-local-variables env f)
        deps (set deps)]
    (filter #(nil? (deps %)) all-local-variables)))

(defmethod pp/code-dispatch JSValue [alis]
  (.write ^Writer *out* "#js ")
  (pp/code-dispatch (.-val alis)))

(defn ppr [s]
  (let [s (pp/with-pprint-dispatch pp/code-dispatch
            (with-out-str (pp/pprint s)))
        source (->> (str/split-lines s)
                    (take 8)
                    (str/join "\n"))]
    (str "```\n" source "\n```")))

(defmethod ana/error-message ::inline-function [_ {:keys [source]}]
  (str "React Hook received a function whose dependencies "
       "are unknown. Pass an inline function instead.\n"
       (ppr source)))

(defmethod ana/error-message ::missing-deps [_ {:keys [source missing-deps unnecessary-deps suggested-deps]}]
  (str "React Hook has "
       (when (seq missing-deps)
         (str "missing dependencies: [" (str/join " " missing-deps) "]\n"))
       (when (seq unnecessary-deps)
         (str (when (seq missing-deps) "and ")
              "unnecessary dependencies: [" (str/join " " unnecessary-deps) "]\n"
              (->> unnecessary-deps
                   (keep (fn [sym]
                           (case (:hook (meta sym))
                             "use-ref" (str "`" sym "` is an unnecessary dependency because it's a ref that doesn't change")
                             ("use-state" "use-reducer") (str "`" sym "` is an unnecessary dependency because it's a state updater function that is memoized")
                             nil)))
                   (str/join "\n"))
              "\n"))
       "Update the dependencies vector to be: [" (str/join " " suggested-deps) "]\n"
       (ppr source)))

(defmethod ana/error-message ::deps-array-literal [_ {:keys [source]}]
  (str "React Hook was passed a "
       "dependency list that is a JavaScript array, instead of Clojure’s vector. "
       "Change it to be a vector literal.\n"
       (ppr source)))

(defmethod ana/error-message ::deps-coll-literal [_ {:keys [source]}]
  (str "React Hook was passed a "
       "dependency list that is not a vector literal. This means we "
       "can’t statically verify whether you've passed the correct dependencies. "
       "Change it to be a vector literal with explicit set of dependencies.\n"
       (ppr source)))

(defmethod ana/error-message ::literal-value-in-deps [_ {:keys [source literals]}]
  (str "React Hook was passed literal values in dependency vector: [" (str/join ", " literals) "]\n"
       "Those are not valid dependencies because they never change. You can safely remove them.\n"
       (ppr source)))

(defmethod ana/error-message ::unsafe-set-state [_ {:keys [source unsafe-calls]}]
  (str "React Hook contains a call to `" (first unsafe-calls) "`.\n"
       "Without a vector of dependencies, this can lead to an infinite chain of updates.\n"
       "To fix this, pass the state value into a vector of dependencies of the hook.\n"
       (ppr source)))

(defn- fn-literal? [form]
  (and (list? form) ('#{fn fn*} (first form))))

(def literal?
  (some-fn keyword? number? string? nil? boolean?))

(defn- deps->literals [deps]
  (filter literal? deps))

(defn- lint-deps [form deps]
  (when deps
    (cond
      ;; when deps are passed as JS Array, should be a vector instead
      (and (= (type deps) JSValue) (vector? (.-val deps))) [::deps-array-literal {:source form}]

      ;; when deps are neither JS Array nor Clojure's vector, should be a vector instead
      #_#_
      (not (vector? deps)) [::deps-coll-literal {:source form}]

      ;; when deps vector has a primitive literal, it can be safely removed
      (and (vector? deps) (seq (deps->literals deps)))
      [::literal-value-in-deps {:source form :literals (deps->literals deps)}])))

(defn find-hook-for-symbol [env sym]
  (when-let [init (-> env :locals (get sym) :init)]
    (let [form (:form init)]
      (when (list? form)
        (cond
          (and (= 'clojure.core/nth (first form)) (= 1 (nth form 2)))
          (recur (:env init) (second form))

          (hook? (first form))
          (name (first form)))))))

(def stable-hooks
  #{"use-state" "use-reducer" "use-ref"})

(defn find-unnecessary-deps [env deps]
  (keep (fn [sym]
          (when-let [hook (find-hook-for-symbol env sym)]
            (when (contains? stable-hooks hook)
              (with-meta sym {:hook hook}))))
        deps))

(def state-hooks
  #{"use-state" "use-reducer" "useState" "useReducer"})

(defn find-unsafe-set-state-calls [env f]
  (let [set-state-calls (->> (find-local-variables env f)
                             (filter #(contains? state-hooks (find-hook-for-symbol env %)))
                             set)
        ast (ana-api/no-warn (ana-api/analyze env f))]
    (loop [[{:keys [children] :as node} & nodes] (:methods ast)
           unsafe-calls []]
      (if (= :fn (:op node))
        (recur nodes unsafe-calls)
        (let [child-nodes (mapcat #(let [child (get node %)]
                                     (if (map? child) [child] child))
                                  children)
              unsafe-calls (if (and (= :invoke (:op node))
                                    (-> node :fn :form set-state-calls))
                             (->> node :fn :form set-state-calls
                                  (conj unsafe-calls))
                             unsafe-calls)]
          (cond
            (seq child-nodes) (recur (concat child-nodes nodes) unsafe-calls)
            (seq nodes) (recur nodes unsafe-calls)
            :else (seq unsafe-calls)))))))

(defn find-missing-and-unnecessary-deps [env f deps]
  (let [free-vars (find-free-variables env f deps)
        all-unnecessary-deps (set (find-unnecessary-deps env (concat free-vars deps)))
        declared-unnecessary-deps (keep all-unnecessary-deps deps)
        #_#_missing-deps (filter (comp not all-unnecessary-deps) free-vars)
        suggested-deps (-> (filter (comp not (set declared-unnecessary-deps)) deps)
                           #_(into missing-deps))]
    [#_missing-deps declared-unnecessary-deps suggested-deps]))

(defn- lint-body [env form f deps]
  (cond
    ;; when a reference to a function passed into a hook, should be an inline function instead
    (not (fn-literal? f)) [::inline-function {:source form}]

    (vector? deps)
    (let [[#_missing-deps declared-unnecessary-deps suggested-deps] (find-missing-and-unnecessary-deps env f deps)]
      ;; when hook function is referencing vars from out scope that are missing in deps vector
      (when (or #_(seq missing-deps) (seq declared-unnecessary-deps))
        [::missing-deps {#_#_:missing-deps missing-deps
                         :unnecessary-deps declared-unnecessary-deps
                         :suggested-deps suggested-deps
                         :source form}]))

    (and (effect-hook? form) (nil? deps))
    (when-let [unsafe-calls (find-unsafe-set-state-calls env f)]
      ;; when set-state is called directly in a hook without deps, causing infinite loop
      [::unsafe-set-state {:unsafe-calls unsafe-calls
                           :source form}])))

(defn lint-exhaustive-deps [env form f deps]
  (let [errors [(lint-deps form deps)
                (lint-body env form f deps)]]
    (filter identity errors)))

(defn lint-exhaustive-deps! [env form f deps]
  (doseq [[error-type opts] (lint-exhaustive-deps env form f deps)]
    (ana/warning error-type env opts)))

