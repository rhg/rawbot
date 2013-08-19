(ns rawbot.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader)))

(def cfg (atom (-> "config.clj" io/resource slurp read-string)))
(def bots (atom {}))

(declare conn-handler)

(defn connect [server]
  (let [socket (Socket. (:name server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))
        conn (ref {:in in :out out})]
    (doto (Thread. #(conn-handler conn)) (.start))
    (swap! bots assoc (:name server) conn)
    conn))

(defn write [conn msg]
  (println "-->" msg)
  (doto (:out @conn)
    (.println (str msg "\r"))
    (.flush)))

(defn conn-handler [conn]
  (while (nil? (:exit @conn))
    (let [msg (.readLine (:in @conn))]
      (println msg)
      (cond 
        (and (re-find #":\$die" msg) (= (:master @cfg) ((re-find #":(.*)\!" msg) 1)))
          (dosync (alter conn assoc :exit true))
       (re-find #"^ERROR :Closing Link:" msg) 
       (dosync (alter conn merge {:exit true}))
       (re-find #"^PING" msg)
       (write conn (str "PONG "  (re-find #":.*" msg))))))
  (swap! bots disj conn))

(defn login [conn user]
  (write conn (str "NICK " (:nick user)))
  (write conn (str "USER " (:nick user) " 0 * :" (:name user))))

(defn join-channels [irc cfg]
  (doseq [c (:channels cfg)]
    (write irc (str "JOIN " c))))

(defn -main [& args]
  (doseq [[s c] (:servers @cfg)]
    (doto (connect {:name s :port (:port c)})
      (login {:nick (:nick c) :name (:nick c)})
      (join-channels c))))
