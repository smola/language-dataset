(import flask)
(import [flask.ext.jsonify [jsonify]])
(import yesbina.api)
(import yesbina.line)

(def app (flask.Flask "__main__"))

(with-decorator (app.route "/<name>/<line>") jsonify
  (defn departures [name line]
    (yesbina.api.departures-from-stop name line)))

(with-decorator (app.route "/<line>/stops") jsonify
  (defn line-stops [line]
    (yesbina.line.all-stops-for-line line)))

(with-decorator (app.route "/<line>") jsonify
  (defn interesting-departures [line]
    (yesbina.api.interesting-departures line)))
