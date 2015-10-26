(ns pinger.core
  (:import (java.net URL HttpURLConnection))
  (:require [pinger.scheduler :as scheduler]
            [clojure.tools.logging :as logger]
            [pinger.config :as config])
  (:gen-class))

(defn response-code [address]
  (let [conn ^HttpURLConnection (. (URL. address) openConnection)
        code (. conn getResponseCode)]
    (when (< code 400)
      (.. conn getInputStream close))
    code))

(defn available? [address]
  (= 200 (response-code address)))

(defn record-availability [address]
  (if (available? address)
    (logger/info (str address " is responding normally"))
    (logger/error (str address " is not available"))))

(defn check []
  (let [adresses (config/urls (config/config))]
      (doseq [address adresses]
        (record-availability address))))

(def immediately 0)
(def every-6-sec (* 1000 6))

(defn start [e]
  "REPL helper. Starts pinger on executor e."
  (scheduler/periodically e check
                          :initial-delay immediately
                          :delay every-6-sec))

(defn stop [e]
  "REPL helper. Stops executor e."
  (scheduler/shutdown-executor e))

(defn -main []
  (start (scheduler/scheduled-executor 1)))
