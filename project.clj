(defproject gll "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]]
  :profiles {:dev {:dependencies [[dorothy "0.0.6"]
                                  [fipp "0.6.7"]]
                   :resource-paths ["test/resources"]}}
  :global-vars {*warn-on-reflection* true})
