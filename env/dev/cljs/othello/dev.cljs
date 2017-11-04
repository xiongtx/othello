(ns ^:figwheel-no-load othello.dev
  (:require
    [othello.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
