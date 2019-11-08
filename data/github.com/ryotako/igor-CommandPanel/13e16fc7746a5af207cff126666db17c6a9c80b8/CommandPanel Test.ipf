#pragma ModuleName = CommandPanelTest 
#include "CommandPanel"
#include "unit-testing"

Menu "Test"
	"CommandPanel Test", RunTest("CommandPanel Test.ipf", name = "CommandPanel Test")
End

static Function TestGUI()
	
	if(WhichListItem("CommandPanel", WinList("*", ";", "WIN:64")) >= 0)
		KillWindow CommandPanel
	endif
	
	// Panel is created?
	CreateCommandPanel()
	CHECK( WhichListItem("CommandPanel", WinList("*", ";", "WIN:64")) >= 0 )
	
	// Outputted into buffer?
	Make/T/FREE buf = {"This", "is", "a", "test"}
	CommandPanelOutput(buf)
	CHECK_EQUAL_TEXTWAVES(CommandPanel#GetTxtWave("buffer"), buf)
	
	// Executed?
	K0 = 0
	CommandPanelExecute("K0 = 1")
	CHECK_EQUAL_VAR(K0, 1)

	// teardown
	Make/FREE/T/N=0 null
	CommandPanelOutput(null)
	KillWindow CommandPanel
End

static Function TestExpansion()
	// Strong line split
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit(""), {""})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a"), {"a"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a;b;c"), {"a;b;c"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("aaa;bbb;ccc"), {"aaa;bbb;ccc"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a;;b;;c"), {"a","b","c"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a;;b;;c;;"), {"a","b","c",""})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("aaa;;bbb;;ccc"), {"aaa","bbb","ccc"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit(" a ;; b ;; c "), {" a "," b "," c "})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a;;b//;;c"), {"a","b//;;c"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a;;\"b;;c\""), {"a","\"b;;c\""})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#StrongLineSplit("a;;\\\"b;;c\\\""), {"a","\\\"b","c\\\""})

	// Weak Line Split 
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit(""), {""})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit("a;b;c"), {"a","b","c"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit("aaa;bbb;ccc"), {"aaa","bbb","ccc"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit(" a ; b ; c "), {" a "," b "," c "})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit("a;b;c;"), {"a","b","c",""})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit("a;b//;c"), {"a","b//;c"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#WeakLineSplit("a;\"b;c\""), {"a","\"b;c\""})

	// stash aliases
	Duplicate/FREE CommandPanel#GetTxtWave("alias") tmp
	CommandPanel#SetTxtWave("alias", {"alias=CommandPanel#alias"})

	// Alias expansion
	CommandPanel#Alias("ts=test")
	String got, expected
	CHECK(StringMatch(CommandPanel#ExpandAlias(""), ""))
	CHECK(StringMatch(CommandPanel#ExpandAlias("ts"), "test"))
	CHECK(StringMatch(CommandPanel#ExpandAlias("ts;ts"), "test;test"))
	CHECK(StringMatch(CommandPanel#ExpandAlias("tss"), "tss"))

	// restore aliases
	CommandPanel#SetTxtWave("alias", tmp)
	
	// Brace expansion
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace(""), {""})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("test"), {"test"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{test1,test2,test3}"), {"test1","test2","test3"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{}"), {"{}"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{test}"), {"{test}"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{a,b,c}"), {"a","b","c"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{1..10}"), {"1","2","3","4","5","6","7","8","9","10"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{6..10}"), {"6","7","8","9","10"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{5..1}" ), {"5","4","3","2","1"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{A..Z}" ), {"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{a..z}" ), {"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{Z..A}" ), {"Z","Y","X","W","V","U","T","S","R","Q","P","O","N","M","L","K","J","I","H","G","F","E","D","C","B","A"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{z..a}" ), {"z","y","x","w","v","u","t","s","r","q","p","o","n","m","l","k","j","i","h","g","f","e","d","c","b","a"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{1..10..2}"), {"1","3","5","7","9"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{m,n}"), {"am","an"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{m,n}a"), {"ma","na"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{m,n}b"), {"amb","anb"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{a,b}{m,n}"), {"am","an","bm","bn"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{,m,n}"), {"a","am","an"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{m,n,}"), {"am","an","a"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{m,,n}"), {"am","a","an"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{,,,}"), {"a","a","a","a"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("a{b,{c,}}"), {"ab","ac","a"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("\"{a,b,c}\""), {"\"{a,b,c}\""})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("//{a,b,c}"), {"//{a,b,c}"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{a,b//,c}"), {"{a,b//,c}"})	
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("\\{a,b,c}"), {"\\{a,b,c}"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{a,b,c\\}"), {"{a,b,c\\}"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{a\\,b,c}"), {"a\\,b","c"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandBrace("{4,{10..40..10},{50..300..50}} K"), {"4 K","10 K","20 K","30 K","40 K","50 K","100 K","150 K","200 K","250 K","300 K"})

	// Path expansion
	NewDataFolder/O/S root:Packages:CommandPanelTest
	NewDataFolder/O root:Packages:CommandPanelTest:folder1
	NewDataFolder/O root:Packages:CommandPanelTest:folder1:sub1
	NewDataFolder/O root:Packages:CommandPanelTest:folder1:sub2
	NewDataFolder/O root:Packages:CommandPanelTest:folder1:sub3
	NewDataFolder/O root:Packages:CommandPanelTest:folder2
	NewDataFolder/O root:Packages:CommandPanelTest:folder2:sub1
	NewDataFolder/O root:Packages:CommandPanelTest:folder2:sub2
	NewDataFolder/O root:Packages:CommandPanelTest:folder2:sub3
	NewDataFolder/O root:Packages:CommandPanelTest:folder3
	NewDataFolder/O root:Packages:CommandPanelTest:folder3:sub1
	NewDataFolder/O root:Packages:CommandPanelTest:folder3:sub2
	NewDataFolder/O root:Packages:CommandPanelTest:folder3:sub3

	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandPath("cd :*"), {"cd :folder1", "cd :folder2", "cd :folder3"})
	CHECK_EQUAL_TEXTWAVES(CommandPanel#ExpandPath("cd :**:"), {"cd :folder1:","cd :folder1:sub1:","cd :folder1:sub2:","cd :folder1:sub3:","cd :folder2:","cd :folder2:sub1:","cd :folder2:sub2:","cd :folder2:sub3:","cd :folder3:","cd :folder3:sub1:","cd :folder3:sub2:","cd :folder3:sub3:"})

	// Complete Parenthesis
	CHECK(StringMatch(CommandPanel#CompleteParen(""),""))
	CHECK(StringMatch(CommandPanel#CompleteParen(" CommandPanelTestFunc_IGNORE ")," CommandPanelTestFunc_IGNORE() "))
	CHECK(StringMatch(CommandPanel#CompleteParen("CommandPanelTestFunc_IGNORE a, b, c // comment"),"CommandPanelTestFunc_IGNORE(a, b, c) // comment"))
	CHECK(StringMatch(CommandPanel#CompleteParen("CommandPanelTestFunc_IGNORE //"),"CommandPanelTestFunc_IGNORE() //"))
	CHECK(StringMatch(CommandPanel#CompleteParen("CommandPanelTestFunc_IGNORE \" // \" "),"CommandPanelTestFunc_IGNORE(\" // \") "))
	CHECK(StringMatch(CommandPanel#CompleteParen(" CommandPanelTestFuncStr ")," CommandPanelTestFuncStr(\"\") "))
	CHECK(StringMatch(CommandPanel#CompleteParen("CommandPanelTestFuncStr test "),"CommandPanelTestFuncStr(\"test\") "))
End

Function CommandPanelTestFunc_IGNORE()

End

Function CommandPanelTestFuncStr(s)
	String s
End

static Function TestCompletion()
	NewDataFolder/O/S root:Packages:CommandPanelTest
	NewDataFolder/O root:Packages:CommandPanelTest:folder1
	NewDataFolder/O root:Packages:CommandPanelTest:folder1:sub1
	NewDataFolder/O root:Packages:CommandPanelTest:folder1:sub2
	NewDataFolder/O root:Packages:CommandPanelTest:folder1:sub3
	NewDataFolder/O root:Packages:CommandPanelTest:folder2
	NewDataFolder/O root:Packages:CommandPanelTest:folder2:sub1
	NewDataFolder/O root:Packages:CommandPanelTest:folder2:sub2
	NewDataFolder/O root:Packages:CommandPanelTest:folder2:sub3
	NewDataFolder/O root:Packages:CommandPanelTest:folder3
	NewDataFolder/O root:Packages:CommandPanelTest:folder3:sub1
	NewDataFolder/O root:Packages:CommandPanelTest:folder3:sub2
	NewDataFolder/O root:Packages:CommandPanelTest:folder3:sub3

	CommandPanel#SetStr("commandLine", "cd :")
	CommandPanel#Complete()
	CHECK_EQUAL_TEXTWAVES(CommandPanel#GetTxtWave("buffer"), {"cd :folder1","cd :folder2","cd :folder3"})

	cd :folder1
	CommandPanel#SetStr("commandLine", "cd :")
	CommandPanel#Complete()
	CHECK_EQUAL_TEXTWAVES(CommandPanel#GetTxtWave("buffer"), {"cd :sub1","cd :sub2","cd :sub3"})

	CommandPanel#SetStr("commandLine", "cd ::")
	CommandPanel#Complete()
	CHECK_EQUAL_TEXTWAVES(CommandPanel#GetTxtWave("buffer"), {"cd ::folder1","cd ::folder2","cd ::folder3"})	

	// teardown	
	Make/FREE/T/N=0 null
	CommandPanelOutput(null)
End 