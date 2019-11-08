# Copyright 2016 Alexandre Terrasa <alexandre@moz-code.org>.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module app

import model

import popcorn
import popcorn::pop_json

class APIHandler
	super Handler

	var config: AppConfig

	redef type BODY: FactForm
	redef var validator = new FactValidator
end

class FactsHandler
	super APIHandler

	redef fun get(req, res) do
		var facts = config.facts.find_all
		res.header["Access-Control-Allow-Origin"] = "*"
		res.json facts.rand
	end

	redef fun post(req, res) do
		var body = validate_body(req, res)
		if body == null then return

		var fact = deserialize_body(req, res)
		if fact == null then return

		var catfact = new CatFact(fact.user, fact.fact)
		config.facts.save(catfact)
		res.header["Access-Control-Allow-Origin"] = "*"
		res.json fact
	end
end

class FactHandler
	super APIHandler

	fun get_fact(req: HttpRequest, res: HttpResponse): nullable CatFact do
		var id = req.param("id")
		if id == null then
			res.json_error("Missing /:id", 403)
			return null
		end
		var fact = config.facts.find_by_id(id)
		if fact == null then
			res.json_error("Fact not found", 404)
			return null
		end
		return fact
	end

	redef fun get(req, res) do
		var fact = get_fact(req, res)
		if fact == null then return
		res.header["Access-Control-Allow-Origin"] = "*"
		res.json fact
	end

	redef fun put(req, res) do
		var fact = get_fact(req, res)
		if fact == null then return

		var body = validate_body(req, res)
		if body == null then return

		var edit = deserialize_body(req, res)
		if edit == null then return

		fact.user = edit.user
		fact.fact = edit.fact

		config.facts.save(fact)
		res.header["Access-Control-Allow-Origin"] = "*"
		res.json fact
	end

	redef fun delete(req, res) do
		var fact = get_fact(req, res)
		if fact == null then return

		config.facts.remove_by_id(fact.id)
		res.header["Access-Control-Allow-Origin"] = "*"
		res.json fact
	end
end

class FactValidator
	super ObjectValidator

	init do
		super
		add(new StringField("user", required=true, min_size=3, max_size=16))
		add(new StringField("fact", required=true, min_size=5, max_size=255))
	end
end

class FactForm
	serialize

	var user: String

	var fact: String
end

var config = new AppConfig
config.parse_options(args)

config.facts.clear
for file in "json".files do
	var obj = ("json" / file).to_path.read_all.parse_json
	if not obj isa JsonObject then continue
	var fact = new CatFact("Morriar", obj["facts"].as(JsonArray).first.as(String))
	config.facts.save(fact)
end
config.facts.save(new CatFact("a", "b"))

var app = new App
app.use("/", new FactsHandler(config))
app.use("/fact/:id", new FactHandler(config))
app.listen(config.app_host, config.app_port)
