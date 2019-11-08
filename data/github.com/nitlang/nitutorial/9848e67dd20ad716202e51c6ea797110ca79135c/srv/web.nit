# The web server of the tutorial
module web

import popcorn
import template
import html
import nitcorn::log

import tuto
import diff
import nitc::parser
import nitc::toolcontext

# Common services to handle tutorial web request
abstract class TutorialHandler
	super Handler

	# Return a empty template for a page
	fun new_page(req: HttpRequest): Page
	do
		var p = new Page
		var session = req.session
		if session != null then
			if debug then
				var first = tutorial.poset.first
				var status = session.get_status(first)
				status.solved = true
			end

			var score = 0
			for t in tutorial.poset do
				if session.solved(t) then score += 1
			end
			p.session_nav.add """
			<li><a><span class="glyphicon glyphicon-star"></span> {{{score}}}/{{{tutorial.poset.length}}}</a></li>
"""
		end
		return p
	end

	# The associated tutorial
	var tutorial: Tutorial
end

redef class Session
	# The associated tutorial
	#
	# Is needed because most query on a session require knowledge on the tutorial
	var tutorial: Tutorial is noinit

	# 
	var status = new HashMap[Exercise, ExerciseStatus]

	# Is the `exercise` solved? 
	fun solved(exercise: Exercise): Bool do
		var status = status.get_or_null(exercise)
		if status == null then return false
		return status.solved
	end

	# Is the `exercise` requirements all solved?
	fun open(exercise: Exercise): Bool
	do
		for r in tutorial.poset[exercise].direct_greaters do
			if not solved(r) then return false
		end
		return true
	end

	# Display the `exercises` in a `<li>` with their status.
	fun list_exercises(exercises: Collection[Exercise]): HTMLTag
	do
		var ul = new HTMLTag("ul")
		for t in tutorial.poset.linearize(exercises) do
			var a = ul.open("li").open("a").attr("href", t.title_id)
			var icon = a.open("span")
			icon.classes.add "glyphicon"
			a.append " "
			if solved(t) then
				a.add_class "solved"
				icon.classes.add "glyphicon-check"
			else
				if open(t) then
					a.add_class "open"
					icon.classes.add "glyphicon-expand"
				else
					a.add_class "locked"
					icon.classes.add "glyphicon-unchecked"
				end
			end
			a.append(t.title)
		end
		return ul
	end

	# Return the status of an `exercise`.
	# Create it if needed.
	fun get_status(exercise: Exercise): ExerciseStatus
	do
		var res = status.get_or_null(exercise)
		if res == null then
			res = new ExerciseStatus(exercise)
			status[exercise] = res
		end
		return res
	end

	# Save the session in the `db`
	fun save
	do
		var done = new Array[String]
		for t in tutorial.poset do
			if solved(t) then done.add t.name
		end

		if done.is_empty then return

		print "save {id_hash}"
		tutorial.db[id_hash + ".done"] = done.join(",")
		tutorial.db.save
	end

	# Try to load the session from the `db` and change the `id_hash` if successful.
	fun load(id_hash: String)
	do
		var done = tutorial.db[id_hash + ".done"]
		if done == null then return

		print "load {id_hash} from {id_hash}"
		var dones = done.split(",")
		for d in dones do
			var t = tutorial.exercise_by_name.get_or_null(d)
			if t == null then continue
			var s = get_status(t)
			s.solved = true
		end

		sys.sessions[id_hash] = self
		self.id_hash = id_hash
	end
end

redef class HTMLTag
	# Open a `<div>` tag with given `classes`.
	fun div(classes: String): HTMLTag
	do
		return open("div").attr("class", classes)
	end

	# Open a `<p>` tag with a given `text`.
	fun p(text: String): HTMLTag
	do
		return open("p").append(text)
	end
end

# Homepage
class HomeHandler
	super TutorialHandler

	redef fun get(req, res) do
		var p = new_page(req)
		var div = new HTMLTag("div")
		var session = req.session
		if session == null then return

		div.open("h1").append("Missions")
		div.add session.list_exercises(tutorial.poset)
		p.content.add div
		res.html p
	end
end

# Page of an exercise
class MissionHandler
	super TutorialHandler

	redef fun get(req, res) do
		var title_id = req.param("mission")
		if title_id == null then return
		var session = req.session
		if session == null then return

		var p = new_page(req)
		var div = new HTMLTag("div")
		p.content.add div

		var t = tutorial.exercise_by_name.get_or_null(title_id)
		if t == null then
			div.p("No mission {title_id}.")
			res.html(p, 404)
			return
		end

		p.title = t.title

		var status = session.status.get_or_null(t)

		# Top panels
		if session.solved(t) then
			var div2 = div.div("panel panel-success")
			div2.div("panel-heading").open("h1").append "This mission is solved!"
			div2.div("panel-body").p("To do next:").add session.list_exercises(tutorial.poset[t].direct_smallers)

		else if not session.open(t) then
			var div2 = div.div("panel panel-danger")
			div2.div("panel-heading").open("h1").append "This mission has prerequisites"
			div2.div("panel-body").p("It is suggested to do before:").add session.list_exercises(tutorial.poset[t].direct_greaters)

		end

		# Description
		div.div("panel panel-default").div("panel-body").add_raw_html t.subject.write_to_string

		# Form
		var form = div.open("form").attr("id", "form").attr("method", "post").div("panel panel-default").div("panel-body")
		var code = null
		if status != null then code = status.code
		if code == null then code = t.template
		form.open("div").open("textarea").attr("id", "code").attr("name", "code").attr("rows", "10").append(code)
		form.open("button").attr("class","btn").attr("type", "submit").attr("name","submit").append("Try It!")
		form.append " "
		form.open("button").attr("class","btn").attr("type", "submit").attr("name","reset").append("Reset")

		# Result/status
		if status != null then
			var answer = status.answer
			var result = status.result
			if answer != null then
				var div2 = div.div("panel panel-success")
				div2.div("panel-heading").open("h3").append "Success"
				div2 = div2.div("panel-body")
				div2.append("Solution:")
				div2.add_raw_html(answer)
				div2.p("To do next:").add session.list_exercises(tutorial.poset[t].direct_smallers)
			else if result != null then
				var div2 = div.open("div").attr("class", "panel panel-danger")
				div2.div("panel-heading").open("h3").append "Error"
				div2.div("panel-body").open("pre").append(result)
			end

			# Callback to display line-widgets
			div.add_raw_html "<script>function nitmessage()\{"
			for m in status.messages do
				div.add_raw_html """
	l = document.createElement("div");
	l.className = "lint-error"
	l.innerHTML = "<span class='glyphicon glyphicon-warning-sign lint-error-icon'></span> {{{m.text.html_escape}}}";
	w = editor.addLineWidget({{{m.location.line_start-1}}}, l);
"""
			end
			div.add_raw_html "\};</script>"
		end

		# Debug
		if debug then
			div.open("hr")
			div.p("Some debug information...")
			if status == null then
				div.p "No status"
			else
				div.open("pre").append status.code or else "# no code"
				div.p "Solved? {status.solved}"
				div.open("pre").append status.result or else "no result"
				div.open("div").add_raw_html status.answer or else "no answer"
			end
		end
		res.html p
	end

	redef fun post(req, res) do
		var title_id = req.param("mission")
		var t = tutorial.exercise_by_name.get_or_null(title_id)
		if t == null then return
		res.redirect "#form"

		var session = req.session
		if session == null then return

		print req.post_args.keys
		var status = session.get_status(t)
		var code = req.post_args.get_or_null("code")
		if req.post_args.has_key("reset") then
			code = null
		end

		status.tryit(code)
		session.save
	end
end

# The current status of an exercise by a player.
class ExerciseStatus
	# The associated exercise.
	var exercise: Exercise

	# Is the exercise solved?
	var solved = false

	# The last attempted code
	var code: nullable String = null

	# The last result (message)
	var result: nullable String = null

	# The last answer (solution)
	var answer: nullable String = null

	# List of messages to display
	var messages = new Array[Message]

	# Try to validate the `code`.
	#
	# Update the attributes.
	fun tryit(code: nullable String) do
		self.code = code
		self.result = null
		self.answer = null
		self.messages.clear
		if code == null then return

		# TODO
		# * [_] log things
		# * [_] async testing
		# * [_] do more processing here (not in the shell)
		# * [_] better results information
		# * [_] remove hard paths

		var date = (new TimeT).to_i
		var out = "../nit_jail/out/{date}"
		out.mkdir
		var prog = out / "prog.nit"
		code.write_to_file(prog)
		print "out: {prog}"

		var d = tmpl_diff(exercise.template.split('\n'), code.split('\n'))
		if d != null then
			self.result = "Please use the template provided as is without any modification."
			return
		end

		var source = new SourceFile.from_string("", code)
		var lexer = new nitc::Lexer(source)
		var parser = new nitc::Parser(lexer)
		var tree = parser.parse
		var eof = tree.n_eof
		if eof isa AError then
			var message = new Message(eof.location, "syntax", eof.message, 2)
			messages.add message
			self.result = message.to_s
			return
		end

		var res = system("cd ../nit_jail && ./runtest.sh out/{date}/prog.nit > out/{date}/result 2>&1")
		var result = (out / "result").to_path.read_all
		var answer = (out / "answer.html").to_path.read_all

		if not result.is_empty then
			self.result = result
		end

		if not answer.is_empty then
			self.answer = answer
			solved = true
		end
	end
end

# A template for a page
class Page
	super Template

	# The default title.
	var title: String = "Nit Tutorial"

	# The default content (to fill)
	var content = new Template

	# The session information in the nav bar
	var session_nav = new Template

	redef fun rendering do
		add """
<!DOCTYPE html>
<html>
<head>
	<meta charset='utf-8'/>
	<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous"/>
	<link rel='stylesheet' href='static/main.css'/>
	<link rel="stylesheet" href="codemirror/lib/codemirror.css">
	<link rel="stylesheet" href="codemirror/theme/blackboard.css">
	<script src="codemirror/lib/codemirror.js"></script>
	<Xscript src="codemirror/mode/css/css.js"></script>
	<script src="codemirror/mode/nit/nit.js"></script>
	<title>{{{title.html_escape}}}</title>
</head>
<body><div class="container">
<nav class="navbar navbar-inverse">
	<div class="container-fluid">
	<div class="navbar-header">
	<a class="navbar-brand" href="/">Missions</a>
	</div>
	<ul class="nav navbar-nav navbar-right">
"""
		add session_nav
		add """
	</ul>
	</div>
</nav>

"""
		add content
		add "</div>"
		add """
<script>
	var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
		lineNumbers: true,
		viewportMargin: Infinity,
		theme: "blackboard"
	});
	nitmessage();
</script>
"""
		add "</body></html>"
	end
end

# Create or reattach to a saved session
class SessionHandler
	super TutorialHandler

	redef fun all(req, res) do
		if req.session == null then
			# Can we get a saved session?
			var id_hash = req.cookie.get_or_null("nitcorn_session")

			var session = new Session
			session.tutorial = tutorial
			req.session = session

			if id_hash != null then
				session.load(id_hash)
			end
		end
	end
end

redef class Tutorial
	var home_handler = new HomeHandler(self)
	var mission_handler = new MissionHandler(self)
	var session_handler = new SessionHandler(self)
end

# Activate debug information in the generated HTML
fun debug: Bool do return false

var pages = "../tracks"
var iface = "localhost"
var port = 3000

if args.length > 0 then
	iface = args[0]
end
if args.length > 1 then
	port = args[1].to_i
end

var tutorial = new Tutorial
tutorial.load(pages)

var app = new App
app.use_before("/*", new RequestClock)
app.use("/*", new StaticHandler("../public"))
app.use("/*", tutorial.session_handler)
app.use("/", tutorial.home_handler)
app.use("/:mission", tutorial.mission_handler)
app.use_after("/*", new ConsoleLog)
app.listen(iface, port)
