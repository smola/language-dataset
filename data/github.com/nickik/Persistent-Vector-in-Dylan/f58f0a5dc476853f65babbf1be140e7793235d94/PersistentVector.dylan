module: PersistentVector
synopsis: 
author: 
copyright: 


define constant EMPTY-NODE :: <node> = make(<node>);
define constant EMPTY-PVector :: <PVector> = make(<PVector>);

define class <node> (<object>)
  constant slot array :: <vector> = make(<vector>, size: 32), init-keyword:  array:;
end class <node>;

define method print-object ( node ::<node> , stream :: <stream> ) => ()
  format-out("%=\n", node.array);
end method print-object;

define class <PVector>(<sequence>)
  constant slot element-count :: <integer> = 0, init-keyword: size:;
  constant slot shift :: <integer> = 5, init-keyword: shift:;
  constant slot root-tail :: <vector> = make(<vector>), init-keyword: tail:;
  constant slot root-node :: <node> = EMPTY-NODE, init-keyword: root-node:;
  //constant slot tailoff :: <integer> = element-count - size(tail);
end class <PVector>;

define method print-object ( vec :: <PVector>, stream :: <stream>) => ()
  format-out("PVector %=\n Tail: %=\n\n", vec.root-node, vec.root-tail); 
end method print-object;

define function main(name, arguments)
  let pvec = EMPTY-PVector;
  for (element  from 0  to 10)
    pvec := add(pvec, element);
    //format-out("%=\n", pvec);
  end for; 
  format-out("%=\n", pvec[5]);
  let new-vec :: <PVector> = assoc(pvec, 5, "maaaan!!!!");
  let myelement = element(new-vec, 5, default: "why default?");
  format-out("after assoc in pos 5 print pos 5: %=\n", myelement);
  //format-out("  %=\n", map(\-, pvec));
  format-out("reduce + : %=\n", reduce(\+,0,pvec));
  format-out("type for copy object-class pvec %=\n", type-for-copy(new-vec));
  format-out(" empty? %=\n", empty?(pvec));
  format-out("empty? %=\n", empty?(EMPTY-PVector));
  format-out("%=\n", map(\+,pvec, pvec));
  format-out("%=\n", object-class ( map(\+, pvec, pvec)));
  format-out("%=\n", last(pvec));
  format-out("list = vec %=\n", list(1,2,3,4) = #[1,2,3,4]);
  format-out("list == vec %=\n", list(1,2,3,4) == #[1, 2, 3, 4]);
  exit-application(0);
end function main;

define method empty? ( vec :: <PVector> ) => ( bool :: <boolean>)
  element-count(vec) == 0;
end method empty?;

define method type-for-copy ( vec :: <PVector> ) => ( type :: <type> )
  <simple-object-vector>;
end method type-for-copy;

define method add ( vec :: <PVector>, val ) => (result-vec :: <PVector>)
  let  tail-size = size(vec.root-tail);  
  if ( tail-size < 32 )
    let new-tail = add( vec.root-tail, val );
    make(<PVector>, size: element-count(vec) + 1, shift: shift(vec), tail: new-tail, root-node: root-node(vec));
  else
    let tailnode = make(<node>, array: vec.root-tail);
    if ( ash( element-count(vec), - 5) > lsh(1, shift(vec)))
      let new-root = make(<node>);
      new-root.array[0] := root-node(vec);
      new-root.array[1] := new-path(vec, shift(vec), tailnode);      
      make(<PVector>, size: element-count(vec) + 1, 
	              shift: shift(vec) + 5, 
                      root-node: new-root, 
                      tail: add(make(<vector>), val));
    else
      let new-root :: <node> = push-tail(vec, shift(vec), root-node(vec), tailnode);
      make(<PVector>, size: element-count(vec) + 1,
                      shift: shift(vec),
                      root-node: new-root,
	              tail: add( make(<vector>), val));
    end if;
  end if;
end method add;

define method size (vec :: <PVector>) => (i :: <integer>)
  element-count(vec);
end method size;

define method push-tail ( vec :: <PVector>, level, parent :: <node>, tailnode :: <node>) => ( node :: <node> )
  let subindex :: <integer> = logand( ash( vec.element-count - 1, - level), 31);
  let ret = make(<node>, array: copy-sequence(parent.array)  );
  let node-to-insert = if (level == 5)
			 tailnode
		       else
			 let child = parent.array[subindex];
			 if (child)
			   push-tail( vec, level - 5, child, tailnode);
			 else
			   new-path (vec, level - 5, tailnode);
			 end if;
		       end if;
  ret.array[subindex] := node-to-insert;
  ret;
end method push-tail;

define method new-path (vec :: <PVector>, level :: <integer>, node :: <node> ) => ( return-node :: <node> )
  if ( level = 0 )
    node;
  else
    let ret :: <node> = make(<node>);
    ret.array[0] := new-path(vec, level - 5, node);
    ret;
  end if;
end method new-path;

define method element(vec :: <PVector>, key, #key default) => (obj :: <object>)
  if (key >= 0 & key < element-count(vec))
    if ( key >= element-count(vec) - size(root-tail(vec)) )
      root-tail(vec)[logand(key, 31)]
    else
      let node-array :: <vector> = array(root-node(vec));
      for (level from shift(vec) above  0 by - 5)
	node-array := array(node-array[logand( ash(key, level), 31)]);
      end for;
      node-array[logand( key, 31)];
    end if;
  else
    default;
  end if;
end method element;

define method assoc (vec :: <PVector>, key :: <integer>, val) => (vec :: <PVector> )
  if (key >= 0 & key < element-count(vec))
      if (key >=  element-count(vec) - size(root-tail(vec)))
        let new-tail = copy-sequence(root-tail(vec));
	format-out("1:element-count: %=\n", element-count(vec));
	make(<PVector>, size: element-count(vec),
	                shift: shift(vec),
	                root-node: root-node(vec),
	                tail: new-tail);
      else
	make(<PVector>, size: element-count(vec),
	                shift: shift(vec),
	                root-node: doAssoc(shift(vec), root-node(vec), key, val),
	                tail: root-tail(vec));
      end if;
  elseif (element-count(vec) == key)
      add(vec, val);
  else
      format-out("Index out of bound! \n");
  end if;
end method assoc;    

define method doAssoc( level :: <integer>, node :: <node>, key :: <integer>, val)
  let ret = make(<node>, array: copy-sequence( array(node)));
  if (level == 0)
    array(ret)[logand( key, 31)] := val;
  else
    let subindex :: <integer> = logand( ash( key, - level)  ,31);
    array(ret)[subindex] := doAssoc(level - 5, array(node)[subindex], key, val);
  end if;
  ret;
end method doAssoc;
/*
define inline method forward-iteration-protocol (sequence :: <sequence>)
    => (initial-state :: <integer>, 
        limit :: <integer>,
        next-state :: <function>, 
        finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, 
        current-element-setter :: <function>,
        copy-state :: <function>);
  values(0,
	 sequence.size,
	 sequence-next-state,
	 sequence-finished-state?,
	 sequence-current-key,
	 element,
	 sequence-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;
*/
/*
define method forward-iteration-protocol 
    (sorted-sequence :: <sorted-sequence>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>, current-element :: <function>,
     current-element-setter :: <function>, copy-state :: <function>)
  values(// Initial state
         0, 
         // Limit
         sorted-sequence.size, 
          // Next state
         method (collection :: <sorted-sequence>, state :: <integer>)
           state + 1
	 end,
         // Finished state?
         method (collection :: <sorted-sequence>, state :: <integer>,
                 limit :: <integer>)
           state = limit;
         end, 
         // Current key
         method (collection :: <sorted-sequence>, state :: <integer>)
           state
         end, 
         // Current element
         element, 
         // Current element setter
         method (value :: <object>, collection :: <sorted-sequence>, 
                 state :: <integer>)
           error("Setting an element of a sorted sequence is not allowed.");
         end, 
         // Copy state
         identity);
end method forward-iteration-protocol; */



define method forward-iteration-protocol  ( vec :: <PVector> )
 => (initial-state :: <integer>, limit :: <integer>,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>);
values(0, // init
       vec.element-count, // limit
       method (collection :: <PVector> , state :: <integer>) 
	 state + 1
       end, // next state
       method (collection :: <PVector>, state :: <integer>, limit :: <integer>)
           state = limit;
       end, // finished state
       method (collection :: <PVector>, state :: <integer>)
	 state
       end, // current state
       element, // current val
       method (vals, seq, states) // current-element-setter
	 error("Immutable collection %=", seq)
       end,
       identity // copy state
       );
end method forward-iteration-protocol;

// Invoke our main() function.
main(application-name(), application-arguments());