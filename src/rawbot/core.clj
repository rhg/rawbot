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
    (doto (Thread. #(conn-handler (:name server) conn)) (.start))
    (swap! bots assoc (:name server) conn)
    conn))

(defn write [conn msg]
  (println "-->" msg)
  (doto (:out @conn)
    (.println (str msg "\r"))
    (.flush)))

(defn master? [{:keys [cfg user]}]
  (when-let [masters (:masters cfg)]
    (let [[nick _ hmask] user]
      (reduce #(and % (if (string? %2)
                        (= hmask %2)
                        (and (= nick (first %2)) (= hmask (second %2)))))
              true masters))))

(defmacro when-master [com-m t & body]
  `(when (and (master? ~com-m) ~t)
    ~@body))


(defn conn-handler [name conn]
  (if (nil? (:exit @conn))
    (let [msg (.readLine (:in @conn))
          com-m {:server name :bot conn :user (rest (re-find #":(.*)!(.*)@(.*) " msg))
                 :cfg (get @cfg name)}]
      (println msg)
      (cond 
        (when-master com-m (re-find #":\$die" msg)
        (dosync (alter conn assoc :exit true)))
        (comment (if-master (re-find #"\$join.*" msg))
                 (write conn (str "JOIN" (get (re-find #"\$join(.+)" msg) 1))))
        (re-find #"^ERROR :Closing Link:" msg) 
        (dosync (alter conn merge {:exit true}))
        (re-find #"^PING" msg)
        (write conn (str "PONG "  (re-find #":.*" msg)))))
  (swap! bots disj conn)))

(defn login [conn user]
  (write conn (str "NICK " (:nick user)))
  (write conn (str "USER " (:nick user) " 0 * :" (:name user))))

(defn join-channels [irc cfg]
  (doseq [c (:channels cfg)]
    (write irc (str "JOIN " c))))

(defn -main [& args]
  (doseq [[s c] (:servers @cfg)]
    (future (doto (connect {:name s :port (:port c)})
      (Thread/sleep 15000)
      (login {:nick (:nick c) :name (:nick c)})
      (join-channels c)))))
