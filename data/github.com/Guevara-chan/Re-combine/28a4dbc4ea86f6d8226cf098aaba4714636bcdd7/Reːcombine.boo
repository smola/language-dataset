# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Re:combine raw -> SQLite converter v0.01  #
# Developed in 2019 by Victoria A. Guevara  #
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

import System
import System.IO
import System.Collections
import System.Collections.Generic
import System.Runtime.CompilerServices
import System.Data.SQLite from 'lib/System.Data.SQLite'

#.{ [Classes]
class Pipe():
	event log as callable(string, string)
	private progress = HashSet[of string]()
	private db		as SQLiteConnection
	private index	as StreamWriter

	# --Methods goes here.
	def constructor():
		pass

	def connect(path as string):
		# Primary SQLite setup.
		(db = SQLiteConnection("Data Source=$path;Version=3;")).Open()
		SQLiteCommand("CREATE TABLE IF NOT EXISTS data (mail VARCHAR(255), pass VARCHAR(255))", db).ExecuteNonQuery()
		# SQLiteCommand("DELETE FROM data WHERE rowid NOT IN (SELECT MIN(rowid) FROM data GROUP BY mail,pass)", db
		# 	).ExecuteNonQuery()
		# Indexer setup.
		log("Indexing '$(path.decolon())'...", "foreword")
		if File.Exists(idx = path + ".idx"):
			for src in File.ReadLines(idx): progress.Add(src.ToLower()); log(src.decolon(), "word")
		index = StreamWriter(idx, true)
		log("$(progress.Count.account('feed')) was marked to skip.\n", "afterword")
		# Finalization.
		return self

	def absorb(feed as Feed):
		# Init setup.
		total = 0
		accum = List[of string]()
		flush = def():
			using c = SQLiteCommand("BEGIN TRANSACTION;\n$(string.Join('\n', accum))COMMIT", db): c.ExecuteNonQuery() 
			log("[$(total += accum.Count)]: $(accum[accum.Count-1])", 'pulse'); accum.Clear()
		# Sequental writing.
		return if progress.Contains(feed.src.ToLower())
		log("Pairs to SQL: $(feed.src.decolon())", "begin")
		for entry in feed:
			accum.Add("insert into data (mail, pass) values ('$(entry[0].esc())', '$(entry[1].esc())');")
			flush() if accum.Count >= 1000
		flush() if accum.Count
		log("$(FileInfo(feed.src).Length.account('byte')) converted.\n", "sum")
		# Reporting finish.
		progress.Add(feed.src.ToLower())
		index.WriteLine(feed.src)
		index.Flush()

	def absorb(feeds as Feed*):
		for feed in feeds: absorb(feed)

	[Extension] static def decolon(text as string):
		return text.Replace(char('ː'), char(':'))

	[Extension] static def esc(text as string):
		# Init setup.
		chars	= text.ToCharArray()
		result	= Text.StringBuilder()
		pos, size = 0, chars.Length-1
		# Escapation loop.
		while pos++ < size:
			if (chr = chars[pos]) == 39: result.Append("''")
			elif chr < 0x20: pass
			else: result.Append(chr)
		# Finalization.
		return result.ToString()

	[Extension] static def account[of T](num as T, countable as string):
		return ("No " if 0 == num else "$num ") + countable + ("" if 1 == num else "s")
# -------------------- #
class Feed((string)*):
	public final src as string
	static final delim = char(':')

	# --Methods goes here.
	def constructor(path as string):
		src = path

	def IEnumerable.GetEnumerator() as IEnumerator:
		return null

	def GetEnumerator():
		for line in File.ReadLines(src):
			yield (line[0:pos=line.IndexOf(delim)], line[pos:]) if line.count(delim) == 1

	static def from_dir(path as string) as Feed*:
		tree = DirectoryInfo(path)
		for branch in tree.GetDirectories():
			for leaflet in Feed.from_dir(branch.FullName): yield leaflet
		for leaflet in tree.GetFiles(): yield Feed(leaflet.FullName)

	[Extension] static def count(text as string, mark as char):
		found = 0
		bytes = text.ToCharArray()
		pos	= bytes.Length
		while pos--:
			found++ if bytes[pos] == mark
		return found
# -------------------- #
class CUI():
	# --Constants goes here--
	static final name	= 'Re:combine'
	static final channels = {'meta': ('Green', ''), 'fault': ('DarkRed', ''),
		'begin': ('Cyan', '╒'), 'pulse': ('DarkGray', '│'), 'sum': ('DarkGreen', '╘'),
		'foreword': ('Yellow', '┌'), 'afterword': ('Magenta', '└'), 'word': ('DarkCyan', '├> ')}

	# --Methods goes here.
	def constructor(input as string):
		# Init setup.
		Console.Title = "◢.$name.◣"
		"""	# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
			# Re:combine raw -> SQLite converter v0.01  #
			# Developed in 2019 by Victoria A. Guevara  #
			# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
			""".Replace('\t', '').log('meta')
		# Main args processing.
		try: Pipe(log: CUI.log[of string]).connect("$input.sqlite").absorb(Feed.from_dir(input))
		except ex: "$ex".log('fault')

	def destructor():
		Console.ForegroundColor = ConsoleColor.Gray
		Threading.Thread.Sleep(3000)

	[Extension] static def log[of T](info as T, channel as string):
		chan as (string) = channels[channel]
		Console.ForegroundColor = ConsoleColor.White
		Console.Write(chan[1])
		Console.ForegroundColor = Enum.Parse(ConsoleColor, chan[0])
		print info
#.}

# ==Main code==
def Main(argv as (string)):
	AppDomain.CurrentDomain.AssemblyResolve += def(sender, e):
		return Reflection.Assembly.LoadFrom("lib/$(Reflection.AssemblyName(e.Name).Name).dll")
	CUI((argv[0] if argv.Length else 'data'))