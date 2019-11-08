/*  This test uses Chapel's data parallel features to create a
 *  parallel hello world program that utilizes multiple cores on a
 *  single locale (node)
 */


//
// This configuration constant indicates the number of messages to
// print out.  The default can be overridden on the command-line
// (e.g., --numMessages=1000000)
//
config const numMessages = 100;
config const n = 12;
config const maxkey = 2**63 : int(64);

//
// Here, we use a data parallel forall loop to iterate over a range
// representing the number of messages to print.  In a forall loop,
// the number of tasks used to implement the parallelism is determined
// by the implementation of the thing driving the iteration -- in this
// case, the range.  See $CHPL_HOME/doc/README.executing (controlling
// degree of data parallelism) for more information about controlling
// this number of tasks.
//
// Because the messages are printed within a parallel loop, they may
// be displayed in any order.  The writeln() procedure protects
// against finer-grained interleaving of the messages themselves.
//
use List;

class Tuple {
  var a, b: string;
  proc printFields() {
    writeln("a = ", a, " b = ", b);
  }
}

class Entry {
    type K;
    type V;
    var key: K;
    var value: V;
}

// note that associative domains not occur on int
var keys: domain(string);
var hashtable0: [keys] list(Entry(string,Tuple));
keys += "1";
keys += "2";
keys += "3";

var A: [1..n] Tuple;
var B: [1..n] Tuple;

forall t in A {
    t = new Tuple("1", "2");
}
forall t in B {
    t = new Tuple("2", "3");
}

//FIXME: not concurrent safe

forall t in A {
    var l = hashtable0[t.b];
    var e = new Entry(string, Tuple, t.b, t);
    if l.last {
        l.append(e);
    } else {
        hashtable0[t.b] = makeList(e);
    }
}

forall t in B {
   var l = hashtable0[t.a];
   if l.last {
       for ss in l.these() {
            if ss.key == t.a { 
                writeln("{");
                ss.value.printFields(); 
                t.printFields();
                writeln("}");
            }
        }
    }
}
         

