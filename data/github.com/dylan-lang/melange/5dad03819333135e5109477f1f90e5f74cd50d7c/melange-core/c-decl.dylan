module: c-declarations
copyright: See LICENSE file in this distribution.
           This code was produced by the Gwydion Project at Carnegie Mellon
           University.  If you are interested in using this code, contact
           "Scott.Fahlman@cs.cmu.edu" (Internet).

//======================================================================
// c-decl.dylan encapsulates definitions and "standard" functions for the
// <declaration> class.  Other files in the c-declarations model handle the
// interface to the parser (c-decl-state.dylan) and write out dylan code
// corresponding to the declarations (c-decl-write.dylan).
//
// The operations defined in this file are designed to be called from
// "define-interface" in a set order.  This ordering is shown in the exports
// list below.
//======================================================================

//------------------------------------------------------------------------
// <declaration>
//
// This section contains definitions and functions common to all (or most)
// declarations.
//------------------------------------------------------------------------
// The class hierarchy for declarations includes the following:
//   <declaration>
//        operations include mapped-name, remap, dylan-name,
//        compute-dylan-name, rename, equate, canonical-name, type-name,
//        compute-closure, find-dylan-name, apply-options, exclude-decl
//     <type-declaration>
//         operations include true-type, pointer-to, c-type-size
//       <structured-type-declaration>
//            operations include find-slot, exclude-slots, make-struct-type,
//            members, apply-container-options
//         <struct-declaration>
//         <union-declaration>
//         <enum-declaration>
//       <pointer-declaration>
//            operations include referent, pointer-to
//       <vector-declaration>
//            operations include length, pointer-equiv
//       <function-type-declaration>
//           operations include find-parameter, find-result
//         <block-type-declaration>
//       <typedef-declaration> (uses <typed> mixin)
//       <incomplete-type-declaration>
//       <predefined-type-declaration>
//         <integer-type-declaration>
//             operations include accessor-name
//           <signed-integer-type-declaration>
//           <unsigned-integer-type-declaration>
//         <float-type-declaration>
//       <bitfield-declaration>
//           operations include bits-in-field, base-type
//     <value-declaration> (includes <typed> mixin)
//         operations include sealed-string, inlined-string,
//       <function-declaration>
//           operations include find-parameter, find-result
//       <object-declaration>
//           operations include equated and read-only
//         <variable-declaration>
//             operations include getter and setter, read-only.
//         <slot-declaration>
//             operations include excluded?
//         <result-declaration>
//             operations include ignored?-setter
//         <arg-declaration>
//             operations include direction, original-type,
//             argument-direction-setter
//           <varargs-declaration>
//     <constant-declaration>
//         operations include constant-value
//       <enum-slot-declaration>
//       <macro-declaration>
//           operations include add-cpp-declaration
//     <coalesced-bitfields>
//        operations include type, bit-size, fields
//   <typed> (Mix-in class)
//     operations include type
//   <new-static-pointer> (Mix-in class)
//     Corresponds to new types which will be subtypes of
//     <statically-typed-pointer>.  Operations include superclasses.
//------------------------------------------------------------------------

// A <declaration> can correspond to any sort of declaration that might appear
// in a C header file.
//
define abstract class <declaration> (<object>)
  slot simple-name :: <string>, required-init-keyword: #"name";
  slot c-name :: false-or(<string>), init-value: #f;
  slot d-name :: false-or(<string>),
    init-value: #f, init-keyword: #"dylan-name";
  slot map-type :: false-or(<string>), init-value: #f;
  slot declared? :: <boolean>, init-value: #f;
  constant slot abstract-type? :: <boolean>,
    init-value: #f, init-keyword: abstract-type?:;
end class <declaration>;

ignore(abstract-type?);

define abstract class <typed> (<object>)
  slot type :: <type-declaration>, required-init-keyword: #"type";
end class <typed>;

// The following operations are defined upon some or all declarations.
// Method definitions occur with the appropriate subclasses.

// Returns the dylan type to which the declaration is mapped.  (For object
// declarations, this will be the mapped version of the object's type, for
// type declarations it will be the mapped versions of that type.)
//
// The "explicit-only:" keyword is for internal use only.  It instructs
// "mapped-name" to simply return #f if no explicit mapping has been specified
// for the given declaration.
//
define generic mapped-name
    (decl :: <declaration>, #key) => (result :: false-or(<string>));

// Sets the mapped name for a given type (or for an object's type).
//
define generic remap (decl :: <declaration>, name :: <string>) => ();

// Returns the dylan name for this object or type.  This name is independent
// of mapping, but may be changed by renaming or equating.
//
define generic dylan-name (decl :: <declaration>) => (result :: <string>);
define generic dylan-name-setter
    (value :: <string>, decl :: <declaration>) => (result :: <string>);

// Computes an appropriate name for object or type declarations whose names
// haven't already been explicitly set.  Should also recursively call
// "find-dylan-name" (defined in "c-decl-write.dylan") on any declarations
// referenced by this declaration.  Typically just calls the given
// "name-mapper" function with an appropriate "category" argument.
//
define generic compute-dylan-name
    (decl :: <declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);

// Find-dylan-name provides low level support for "apply-options".  It checks
// whether various attributes need to be computed and calls the computation
// functions as required and can be called recursively to deal with nested
// components.
//
define generic find-dylan-name
    (decl :: <declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, read-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);

// Sets the dylan name for this object or type.  (External interface.)
//
define generic rename (decl :: <declaration>, name :: <string>) => ();

// Sets the dylan type to which this type (or this object's type) is
// equivalent.  This is like renaming, but also implies that the dylan type
// already exists.
//
define generic equate (decl :: <declaration>, name :: <string>) => ();

// Returns/computes the string/name by which a type or object can be named in
// a "define interface" clause.  This may be replaced by a more general
// mechanism later.
//
define generic canonical-name (decl :: <declaration>) => (result :: <string>);

// Returns the dylan name of a type or of an object's type.  Since it takes
// into account "equate" clauses on both types and objects, it is different
// from "object.type.dylan-name".
//
define generic type-name (decl :: <declaration>) => (result :: <string>);

// Look up the given pointer type in the state's pointer table, and create it
// if it doesn't yet exist.  (The definition of this function is bundled with
// the definitions for <pointer-declaration>.)
//
define generic pointer-to
    (target-type :: <type-declaration>, state :: <parse-state>)
 => (ptr-type :: <pointer-declaration>);

// This is the exported function which computes various properties of a C
// declarations based upon user specified options.  This includes name
// computation, read-only declarations, and method sealing.
//
// The parameters should correspond to the global values for these options.
// If the declaration had any more specific names or options set earlier than
// they will remain in force in spite of calls to "apply-options".
//
define generic apply-options
    (decl :: <declaration>, map-function :: <function>, prefix :: <string>,
     read-only :: <boolean>, sealing :: <string>, inlining :: <string>,
     pointer-type :: false-or(<string>))
 => ();

//------------------------------------------------------------------------

define method equated? (decl :: <declaration>) => (result :: <boolean>)
  #f;
end method equated?;

define method mapped-name
    (decl :: <declaration>, #key explicit-only?)
 => (result :: false-or(<string>));
  decl.map-type | (~explicit-only? & decl.type-name);
end method mapped-name;

define method remap (decl :: <declaration>, name :: <string>) => ();
  decl.map-type := name;
end method remap;

// Find-dylan-name provides low level support for "apply-options".  It checks
// whether various attributes need to be computed and calls the computation
// functions as required and can be called recursively to deal with nested
// components.
//
define method find-dylan-name
    (decl :: <declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, read-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  decl.d-name
    | (decl.d-name := compute-dylan-name(decl, mapper, prefix, containers,
                                         read-only, sealing, inlining));
end method find-dylan-name;

define method dylan-name (decl :: <declaration>) => (result :: <string>);
  // The name should always be computed by compute-dylan-name before we ask
  // for it.  This routine explicitly verifies this.
  decl.d-name | error("No dylan name defined for %= (%s)",
                      decl, decl.simple-name);
end method dylan-name;

define method dylan-name-setter
    (value :: <string>, decl :: <declaration>) => (result :: <string>);
  decl.d-name := value;
end method dylan-name-setter;

define method rename (decl :: <declaration>, name :: <string>) => ();
  decl.dylan-name := name;
end method rename;

define method canonical-name (decl :: <declaration>) => (result :: <string>);
  // The "canonical name" for most declarations is the same as the "simple
  // name".
  decl.c-name | (decl.c-name := decl.simple-name);
end method canonical-name;

define method compute-closure
    (results :: <deque>, decl :: <declaration>) => (results :: <deque>);
  // For simple declarations, we simply check whether the type has already be
  // "declared" and add it to the result otherwise.  Other methods may call
  // this one after doing recursive declarations.
  if (~decl.declared?)
    decl.declared? := #t;
    push-last(results, decl);
  end if;
  results;
end method compute-closure;

define method apply-options
    (decl :: <declaration>, map-function :: <function>, prefix :: <string>,
     read-only :: <boolean>, sealing :: <string>, inlining :: <string>,
     pointer-type :: false-or(<string>))
 => ();
  find-dylan-name(decl, map-function, prefix, #(), read-only, sealing, inlining);
end method apply-options;

// Exclude-decl -- exported.
//
// By pretending that this declaration has already been processed, we
// effectively exclude it from the generation process.
//
define method exclude-decl (decl :: <declaration>)
  decl.declared? := #t;
end method exclude-decl;


//------------------------------------------------------------------------
// Type declarations
//------------------------------------------------------------------------

define abstract class <type-declaration> (<declaration>)
  slot equated? :: <boolean>, init-value:  #f, init-keyword: #"equated";
end class;

define abstract class <new-static-pointer> (<object>)
  slot superclasses :: false-or(<sequence>), init-value: #f;
end class <new-static-pointer>;

// Pushes past any typedefs to find an actual "structured" type declaration.
// Should only be used in calls of the form: instance?(foo.true-type, <bar>)
//
define generic true-type (type :: <type-declaration>);

//------------------------------------------------------------------------

define method equate (decl :: <type-declaration>, name :: <string>) => ();
  if (instance?(decl.true-type, <predefined-type-declaration>))
    error("Cannot 'equate:' predefined type: %s.", decl.simple-name);
  end if;
  decl.dylan-name := name;
  decl.equated? := #t;
end method equate;

define method compute-dylan-name
    (decl :: <type-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  mapper(#"type", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method type-name (decl :: <type-declaration>) => (result :: <string>);
  decl.dylan-name;
end method type-name;

define method true-type (type :: <type-declaration>)
  type;
end method true-type;

//------------------------------------------------------------------------

define abstract class <structured-type-declaration> (<type-declaration>)
  slot members :: false-or(<sequence>), init-value: #f;
  // This slot (initialized lazily by "do-coalesce-members" stores an
  // alternate version of the "members" sequence in which adjacent
  // bitfields are combined into a <coalesced-bitfields> pseudo-slot.
  slot %coalesced-members :: false-or(<sequence>) = #f;
  slot anonymous? :: <boolean>, required-init-keyword: #"anonymous?";
  // This is used for structs and unions, but not enums:
  slot pointer-type-name :: false-or(<symbol>) = #f;
end class <structured-type-declaration>;

define method coalesced-members (decl :: <structured-type-declaration>)
 => (members :: <sequence>)
  decl.%coalesced-members | do-coalesce-members(decl)
end;

define class <struct-declaration>
    (<new-static-pointer>, <structured-type-declaration>)
end class;

define class <union-declaration>
    (<new-static-pointer>, <structured-type-declaration>)
end class;

define class <enum-declaration> (<structured-type-declaration>)
end class;

// Given a function (or function type) declaration, return the declaration
// corresponding to its result type.
//
define generic find-slot
    (decl :: <structured-type-declaration>, name :: <string>)
 => (result :: <declaration>);

// Removes any slots which were explicitly excluded or, if import-all? is
// false, which are not explicitly imported.
//
define generic exclude-slots
    (decl :: <structured-type-declaration>,
     imports :: <explicit-key-collection>, import-all? :: <boolean>);

// Operation called by the parser to define a new structured (i.e. struct,
// union, or enum) type.  The appropriate declaration class is computed from
// the given token.
//
define generic make-struct-type
    (name :: false-or(<string>),
     member-list :: false-or(<list>),
     token :: <token>, state :: <parse-state>)
 => (result :: <structured-type-declaration>);

// This function is analogous to "apply-options" except that it is called upon
// a specific structured type and applies the options to all "members" of
// that type.  Like "apply-options" it will avoid modifying any names or
// options that might have been set by earlier calls.
//
define generic apply-container-options
    (decl :: <structured-type-declaration>, state :: <parse-state>,
     map-function :: <function>, prefix :: <string>, read-only :: <boolean>,
     sealing :: <string>, inlining :: <string>,
     pointer-type-name :: false-or(<symbol>))
 => ();

define method compute-closure
    (results :: <deque>,
     decl :: type-union(<struct-declaration>, <union-declaration>))
 => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    if (decl.members)
      for (elem in decl.members)
        // When writing the struct out, we use the true-type, so
        // use it here so that typedefs can disappear as needed
        // if they aren't otherwise referenced.
        let elem-type
          = select (elem.type by instance?)
              <vector-declaration> => elem.type.pointer-equiv.referent;
              otherwise => elem.type.true-type;
            end select;
        if (~elem.excluded?) compute-closure(results, elem-type) end if;
      end for;
    end if;
    push-last(results, decl);
  end if;
  results;
end method compute-closure;

define method canonical-name (decl :: <struct-declaration>)
 => (result :: <string>);
  decl.c-name | (decl.c-name := concatenate("struct ", decl.simple-name));
end method canonical-name;

define method canonical-name (decl :: <union-declaration>)
 => (result :: <string>);
  decl.c-name | (decl.c-name := concatenate("union ", decl.simple-name));
end method canonical-name;

define method canonical-name (decl :: <enum-declaration>)
 => (result :: <string>);
  decl.c-name | (decl.c-name := concatenate("enum ", decl.simple-name));
end method canonical-name;

define method make-enum-slot
    (name :: <string>, value :: false-or(<integer>),
     prev :: false-or(<enum-slot-declaration>), state :: <parse-state>)
 => (result :: <enum-slot-declaration>);
  if (element(state.objects, name, default: #f))
    parse-error(state, "Enumeration literal does not have a unique name: %s",
                name);
  else
    let value
      = case
          value => value;
          prev => prev.constant-value + 1;
          otherwise => 0;
        end case;
    state.objects[name] := add-declaration(state,
                                           make(<enum-slot-declaration>,
                                                name: name, value: value))
  end if;
end method make-enum-slot;

define method make-struct-type
    (name :: false-or(<string>),
     member-list :: false-or(<list>),
     decl-token :: <token>, state :: <parse-state>)
 => (result :: <structured-type-declaration>);
  let declaration-class = select (decl-token by instance?)
                            <enum-token> => <enum-declaration>;
                            <struct-token> => <struct-declaration>;
                            <union-token> => <union-declaration>;
                          end select;

  let (true-name, anonymous?)
    = if (name)
        values(name, #f);
      else
        values(anonymous-name(), #t);
      end if;
  let old-type = element(state.structs, true-name, default: #f);
  let type
    = if (old-type)
        if (object-class(old-type) ~= declaration-class)
          parse-error(state,
                      "Struct or union type doesn't match original "
                        "declaration: %s",
                      true-name);
        end if;
        old-type;
      elseif (~instance?(state, <parse-file-state>))
        parse-error(state, "Type not found: %s.", true-name);
      else
        state.structs[true-name]
          := add-declaration(state, make(declaration-class,
                                         name: true-name,
                                         anonymous?: anonymous?));
      end if;

  // "process-member" will make slot or "enum slot" declarations for the raw
  // data returned by the parser.
  let last :: <integer> = -1;
  let process-member
    = if (declaration-class == <enum-declaration>)
        method (elem)
          elem.containing-enum-declaration := type;
          elem;
        end method;
      else
        method (elem :: <pair>)
          make(<slot-declaration>,
               name: elem.head | anonymous-name(),
               type: elem.tail,
               excluded?: ~elem.head);
        end method;
      end if;

  if (member-list)
    if (type.members)
      parse-error(state, "Can't declare structure twice: %s.", true-name);
    else
      type.members := map(process-member, member-list);
    end if;
  end if;
  type;
end method make-struct-type;

define method find-slot
    (decl :: <structured-type-declaration>, name :: <string>)
 => (result :: <declaration>);
  decl.members
    & any?(method (member) member.simple-name = name & member end method,
           decl.members)
    | error("No such slot: %s", name);
end method find-slot;

define method exclude-slots
    (decl :: <structured-type-declaration>,
     imports :: <explicit-key-collection>, import-all? :: <boolean>)
 => ();
  if (decl.members)
    for (member in decl.members)
      member.excluded? := ~element(imports, member, default: import-all?);
    end for;
  end if;
end method exclude-slots;

define method find-dylan-name
    (decl :: <structured-type-declaration>, mapper :: <function>,
     prefix :: <string>, containers :: <sequence>, read-only :: <boolean>,
     sealing :: <string>, inlining :: <string>)
 => (result :: <string>);
  unless (decl.d-name)
    decl.d-name := compute-dylan-name(decl, mapper, prefix, containers,
                                      read-only, sealing, inlining);
    // Take care of the contained objects as well.  Some of these may
    // already have been handled by "container" declarations.
    let sub-containers = list(decl.simple-name);
    if (decl.members)
      for (sub-decl in decl.members)
        find-dylan-name(sub-decl, mapper, prefix, sub-containers,
                        read-only, sealing, inlining);
      end for;
    end if;
  end;
  decl.d-name;
end method find-dylan-name;

define method apply-container-options
    (decl :: <structured-type-declaration>, state :: <parse-state>,
     map-function :: <function>, prefix :: <string>, read-only :: <boolean>,
     sealing :: <string>, inlining :: <string>, ptr-type-name :: false-or(<symbol>))
 => ();
  let sub-containers = list(decl.simple-name);
  if (decl.members)
    for (elem in decl.members)
      find-dylan-name(elem, map-function, prefix, sub-containers, read-only,
                      sealing, inlining);
    end for;
  end if;
  if (ptr-type-name & ~decl.pointer-type-name)
    decl.pointer-type-name := ptr-type-name;
    let pointer-type = element(state.pointers, decl.true-type, default: #f);
    if (pointer-type)
      pointer-type.simple-name := as(<string>, ptr-type-name);
      pointer-type.d-name := as(<string>, ptr-type-name);
    end if;
  end if;
end method apply-container-options;

//------------------------------------------------------------------------

define class <pointer-declaration> (<new-static-pointer>, <type-declaration>)
  constant slot referent :: <type-declaration>, required-init-keyword: #"referent";
  slot accessors-written?, init-value: #f;
end class;

ignore(accessors-written?);
ignore(accessors-written?-setter);

define class <vector-declaration> (<new-static-pointer>, <type-declaration>)
  constant slot pointer-equiv :: <type-declaration>, required-init-keyword: #"equiv";
  constant slot length :: false-or(<integer>),
    required-init-keyword: #"length";
end class <vector-declaration>;

define method rename (decl :: <pointer-declaration>, name :: <string>) => ();
  if (decl.simple-name = decl.referent.simple-name)
    decl.referent.dylan-name := name;
  else
    decl.dylan-name := name;
  end if;
end method rename;

define method mapped-name
    (decl :: <pointer-declaration>, #key explicit-only?)
 => (result :: false-or(<string>));
  if (decl.simple-name = decl.referent.simple-name)
    decl.map-type | decl.referent.map-type
      | (~explicit-only? & decl.type-name);
  else
    decl.map-type | (~explicit-only? & decl.type-name);
  end if;
end method mapped-name;

define method mapped-name
    (decl :: <vector-declaration>, #key explicit-only?)
 => (result :: false-or(<string>));
  decl.map-type | decl.pointer-equiv.map-type
    | (~explicit-only? & decl.type-name);
end method mapped-name;

define method canonical-name (decl :: <pointer-declaration>)
 => (result :: <string>);
  if (decl.c-name)
    decl.c-name;
  else
    for (referent-type = decl.referent then referent-type.referent,
         suffix = "*" then concatenate(suffix, "*"),
         while: instance?(referent-type, <pointer-declaration>))
    finally
      select (referent-type by instance?)
        <vector-declaration> =>
          let referent-name = referent-type.canonical-name;
          let sub-name = referent-type.pointer-equiv.referent.canonical-name;
          decl.c-name
            := concatenate(sub-name, suffix,
                           copy-sequence(referent-name, start: sub-name.size));
        <block-type-declaration> =>
          let referent-name = referent-type.canonical-name;
          let sub-name = referent-type.result.canonical-name;
          decl.c-name := format-to-string("%s (^%s)", sub-name,
                                          copy-sequence(referent-name,
                                                        start: sub-name.size));
        <function-type-declaration> =>
          let referent-name = referent-type.canonical-name;
          let sub-name = referent-type.result.canonical-name;
          decl.c-name := format-to-string("%s (%s)", sub-name,
                                          copy-sequence(referent-name,
                                                        start: sub-name.size));
        otherwise =>
          decl.c-name := concatenate(referent-type.canonical-name, suffix);
      end select;
    end for;
  end if;
end method canonical-name;

define method canonical-name (decl :: <vector-declaration>)
 => (result :: <string>);
  decl.c-name
    | (decl.c-name := concatenate(decl.pointer-equiv.referent.canonical-name,
                                  "[]"));
end method canonical-name;

define method compute-dylan-name
    (decl :: <pointer-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  if (decl.simple-name = decl.referent.simple-name)
    find-dylan-name(decl.referent, mapper, prefix, #(), rd-only, sealing, inlining);
  else
    mapper(#"type", prefix, decl.simple-name, containers);
  end if;
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <pointer-declaration>)
 => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    let have-pointer-type-name?
      = instance?(decl.referent, <structured-type-declaration>) &
        decl.referent.pointer-type-name;
    if (~have-pointer-type-name?)
      compute-closure(results, decl.referent);
      push-last(results, decl);
    end if;
  end if;
  results;
end method compute-closure;

define method compute-dylan-name
    (decl :: <vector-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  mapper(#"type", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <vector-declaration>) => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    compute-closure(results, decl.pointer-equiv);
    push-last(results, decl);
  end if;
  results;
end method compute-closure;

// Look up the given pointer type in the state's pointer table, and create it
// if it doesn't yet exist.
//
define method pointer-to
    (target-type :: <type-declaration>, state :: <parse-state>)
 => (ptr-type :: <pointer-declaration>);
  let target-type = target-type.true-type;
  let found-type = element(state.pointers, target-type, default: #f);
  if (found-type)
    found-type;
  else
    let new-type
      = make(<pointer-declaration>,
             name: concatenate(target-type.simple-name, "*"),
             referent: target-type);
    state.pointers[target-type] := new-type;
    new-type;
  end if;
end method pointer-to;

define method vector-of
    (target-type :: <type-declaration>, state :: <parse-state>, #key length = #f)
 => (vector-type :: <vector-declaration>);
  let found-type = element(state.vectors,
                           vector(target-type, length),
                           default: #f);
  if (found-type)
    found-type;
  else
    let new-type
      = make(<vector-declaration>, length: length,
             name: if (length)
                     format-to-string("%s<@%=>",
                                      target-type.simple-name, length)
                   else
                     format-to-string("%s<@>", target-type.simple-name)
                   end if,
             equiv: pointer-to(target-type, state));
    state.vectors[vector(target-type, length)] := new-type;
    new-type;
  end if;
end method vector-of;

//------------------------------------------------------------------------

define class <function-type-declaration> (<type-declaration>)
  constant slot result :: <result-declaration>, required-init-keyword: #"result";
  constant slot parameters :: <sequence>, required-init-keyword: #"params";
  slot local-name-mapper :: false-or(<function>) = #f;
end class <function-type-declaration>;

define method canonical-name (decl :: <function-type-declaration>)
 => (result :: <string>);
  if (decl.c-name)
    decl.c-name
  else
    // We need to include the actual parameters eventually.  This is a stopgap
    // to guarantee that all function declarations will end up in the name
    // table.
    format-to-string("%s (%s)", decl.result.canonical-name,
                     decl.simple-name);
  end if;
end method canonical-name;

define method find-dylan-name
    (decl :: <function-type-declaration>, mapper :: <function>,
     prefix :: <string>, containers :: <sequence>, read-only :: <boolean>,
     sealing :: <string>, inlining :: <string>)
 => (result :: <string>);
  find-dylan-name(decl.result, mapper, prefix, #(), read-only,
                  sealing, inlining);
  for (elem in decl.parameters)
    if (~instance?(elem, <varargs-declaration>))
      find-dylan-name(elem, mapper, prefix, #(), read-only, sealing, inlining);
    end if;
  end for;

  decl.d-name
    | (decl.d-name := compute-dylan-name(decl, mapper, prefix, containers,
                                         read-only, sealing, inlining));
end method find-dylan-name;

define method compute-dylan-name
    (decl :: <function-type-declaration>, mapper :: <function>,
     prefix :: <string>, containers :: <sequence>, rd-only :: <boolean>,
     sealing :: <string>, inlining :: <string>)
 => (result :: <string>);
  mapper(#"type", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <function-type-declaration>)
 => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;

    compute-closure(results, decl.result);
    for (elem in decl.parameters)
      if (~instance?(elem, <varargs-declaration>))
        compute-closure(results, elem)
      end if;
    end for;

    push-last(results, decl);
  end if;
  results;
end method compute-closure;

//------------------------------------------------------------------------

define class <block-type-declaration> (<function-type-declaration>)
end class <block-type-declaration>;

define method canonical-name (decl :: <block-type-declaration>)
 => (result :: <string>);
  if (decl.c-name)
    decl.c-name
  else
    // We need to include the actual parameters eventually.  This is a stopgap
    // to guarantee that all block declarations will end up in the name
    // table.
    format-to-string("%s (^%s)", decl.result.canonical-name,
                     decl.simple-name);
  end if;
end method canonical-name;

//------------------------------------------------------------------------

define class <typedef-declaration> (<type-declaration>, <typed>) end class;

define method mapped-name
    (decl :: <typedef-declaration>, #key explicit-only?)
 => (result :: false-or(<string>));
  decl.map-type | decl.type.map-type | (~explicit-only? & decl.type-name);
end method mapped-name;

define method compute-dylan-name
    (decl :: <typedef-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  mapper(#"type", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <typedef-declaration>)
 => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    compute-closure(results, decl.type);
    push-last(results, decl);

    // Propagate the typedef name to anonymous (struct, union, enum) types
    if (instance?(decl.type, <structured-type-declaration>)
          & decl.type.anonymous?)
      decl.type.simple-name := decl.simple-name;
      decl.type.c-name := decl.simple-name;
      decl.type.anonymous? := #f;
    end if;
  end if;
  results;
end method compute-closure;

define method true-type (alias :: <typedef-declaration>)
  true-type(alias.type);
end method true-type;

//------------------------------------------------------------------------

define class <incomplete-type-declaration> (<type-declaration>) end class;

define class <predefined-type-declaration> (<type-declaration>)
  constant slot type-size-slot :: <integer>, required-init-keyword: #"size";
end class;

define class <integer-type-declaration> (<predefined-type-declaration>)
  // Accessor-name specifies the "dereference" function to call in order to
  // retrieve the correct number of bytes.
  constant slot accessor-name :: <string>, required-init-keyword: #"accessor";
end class;

ignore(accessor-name);

define class <signed-integer-type-declaration>   (<integer-type-declaration>) end class;
define class <unsigned-integer-type-declaration> (<integer-type-declaration>) end class;

define class <float-type-declaration> (<predefined-type-declaration>)
  constant slot accessor-name :: <string>, required-init-keyword: #"accessor";
end class;

define constant unknown-type = make(<incomplete-type-declaration>,
                                    name: "machine-pointer");
define constant unsigned-type = make(<incomplete-type-declaration>,
                                     name: "unknown-type");
define constant signed-type = make(<incomplete-type-declaration>,
                                   name: "unknown-type");
define constant void-type = make(<predefined-type-declaration>,
                                 dylan-name: "<c-void>",
                                 abstract-type?: #t,
                                 name: "void-type", size: 0);

define constant int-type = make(<signed-integer-type-declaration>,
                                accessor: "signed-long-at",
                                name: "int",
                                dylan-name: "<C-signed-int>",
                                size: $integer-size);
define constant unsigned-int-type = make(<unsigned-integer-type-declaration>,
                                         accessor: "unsigned-long-at",
                                         name: "unsigned-int",
                                         dylan-name: "<C-unsigned-int>",
                                         size: $integer-size);
define constant short-type = make(<signed-integer-type-declaration>,
                                  accessor: "signed-short-at",
                                  name: "short",
                                  dylan-name: "<C-signed-short>",
                                  size: $short-int-size);
define constant unsigned-short-type = make(<unsigned-integer-type-declaration>,
                                           accessor: "unsigned-short-at",
                                           name: "unsigned-short",
                                           dylan-name: "<C-unsigned-short>",
                                           size: $short-int-size);
define constant long-type = make(<signed-integer-type-declaration>,
                                 accessor: "signed-long-at",
                                 name: "long",
                                 dylan-name: "<C-signed-long>",
                                 size: $long-int-size);
define constant unsigned-long-type = make(<unsigned-integer-type-declaration>,
                                          accessor: "unsigned-long-at",
                                          name: "unsigned-long",
                                          dylan-name: "<C-unsigned-long>",
                                          size: $long-int-size);
// "long long" is an idiom supported by gcc, so we'll recognize it, without
// actually supporting access.
define constant longlong-type = make(<signed-integer-type-declaration>,
                                     accessor: "longlong-at",
                                     name: "long-long",
                                     dylan-name: "<C-signed-long>", //XXX: broken!
                                     size: $long-int-size * 2);
define constant unsigned-longlong-type = make(<unsigned-integer-type-declaration>,
                                              accessor: "unsigned-longlong-at",
                                              name: "unsigned-long-long",
                                              dylan-name: "<C-unsigned-long>", // XXX: broken too!
                                              size: $longlong-int-size);
define constant char-type = make(<signed-integer-type-declaration>,
                                 accessor: "signed-byte-at",
                                 name: "char",
                                 dylan-name: "<C-signed-char>",
                                 size: $char-size);
define constant unsigned-char-type = make(<unsigned-integer-type-declaration>,
                                          accessor: "unsigned-byte-at",
                                          name: "unsigned-char",
                                          dylan-name: "<C-unsigned-char>",
                                          size: $char-size);
define constant float-type = make(<float-type-declaration>,
                                  accessor: "float-at",
                                  name: "float",
                                  dylan-name: "<C-float>",
                                  size: $float-size);
define constant double-type = make(<float-type-declaration>,
                                   accessor: "double-at",
                                   name: "double",
                                   dylan-name: "<C-double>",
                                   size: $double-float-size);
define constant long-double-type = make(<float-type-declaration>,
                                        accessor: "long-double-at",
                                        name: "long-double",
                                        dylan-name: "<C-extended>", // XXX: more brokenness
                                        size: $long-double-size);
define constant bool-type = make(<signed-integer-type-declaration>,
                                 accessor: "unsigned-byte-at",
                                 name: "_Bool",
                                 dylan-name: "<C-boolean>",
                                 size: $char-size);
define constant size-t-type = make(<unsigned-integer-type-declaration>,
                                   accessor: "size-t-at",
                                   name: "size-t",
                                   dylan-name: "<C-size-t>",
                                   size: $long-int-size);
define constant ssize-t-type = make(<signed-integer-type-declaration>,
                                    accessor: "ssize-t-at",
                                    name: "ssize-t",
                                    dylan-name: "<C-ssize-t>",
                                    size: $long-int-size);

define method compute-closure
    (results :: <deque>, decl :: <predefined-type-declaration>)
 => (results :: <deque>);
  // We don't need to declare it -- it's predefined.
  results;
end method compute-closure;

//------------------------------------------------------------------------

// Bitfields are special integer objects which are packed together at the bit
// level.  As far as I know, they may only appear in structs.  We handle
// bitfields by packing them together into a <coalesced-bitfields> object,
// which is a pseudo-slot holding some sort of integer.  We end up with two
// parallel sequences of members.  The normal one can have slots with
// <bitfield-declaration>s, while the "coalesced-members" sequence contains
// <coalesced-bitfields> pseudo-slots.  <bitfield-declarations> and
// <coalesced-bitfields> are cross-linked to each other (by the
// "do-coalesce-members" function).

define class <bitfield-declaration> (<type-declaration>)
  constant slot bits-in-field :: <integer>, required-init-keyword: #"bits";
  constant slot base-type :: <type-declaration>, required-init-keyword: #"base";
  slot composite-field :: false-or(<coalesced-bitfields>) = #f;
  slot start-bit :: <integer> = 0;        // only meaningful if composite ~= #f
end class <bitfield-declaration>;

ignore(base-type);
ignore(composite-field);
ignore(start-bit);

define class <coalesced-bitfields> (<declaration>)
  slot type :: <predefined-type-declaration> = unsigned-long-type;
  slot bit-size :: <integer> = 0;
  slot fields :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot excluded? :: <boolean> = #f; // defer to each field
end class <coalesced-bitfields>;

define method compute-closure
    (results :: <deque>, decl :: <bitfield-declaration>)
 => (results :: <deque>);
  // We don't need to declare it -- we just use <integer>
  results;
end method compute-closure;

//------------------------------------------------------------------------

define abstract class <value-declaration> (<declaration>, <typed>)
  slot sealed-string :: <string>, init-value: "";
  slot inlined-string :: <string>, init-value: "";
end class;
define class <function-declaration> (<value-declaration>) end class;
define class <object-declaration> (<value-declaration>)
  slot equated :: false-or(<string>), init-value: #f;
  slot read-only :: type-union(<boolean>, <empty-list>), init-value: #();
end class;
define class <variable-declaration> (<object-declaration>)
  slot getter :: false-or(<string>), init-value: #f;
  slot setter :: false-or(<string>), init-value: #f;
end class;
define class <slot-declaration> (<object-declaration>)
  slot excluded? :: <boolean>, init-value: #f, init-keyword: #"excluded?";
end class;
define class <result-declaration> (<object-declaration>)
  slot error-result? :: <boolean>, init-value: #f, init-keyword: #"error-result?";
end class;
define class <arg-declaration> (<object-declaration>)
  slot direction :: <symbol>, init-value: #"default";
  constant slot original-type :: false-or(<type-declaration>),
    init-value: #f;
end class;
define class <varargs-declaration> (<arg-declaration>) end class;

// Given a function (or function type) declaration, locate the declaration
// corresponding to a given parameter, either by name or position.
//
define generic find-parameter (decl :: <declaration>, param :: <object>)
 => (result :: <arg-declaration>);

// Given a function (or function type) declaration, return the declaration
// corresponding to its result type.
//
define generic find-result (decl :: <declaration>)
 => (result :: <result-declaration>);

// Flag a function result so that it will be ignored.
//
define generic ignored?-setter
    (value :: <boolean>, decl :: <result-declaration>)
 => (result :: <boolean>);

// Sets the "direction" for the given argument.
//
define generic argument-direction-setter
    (dir :: <symbol>, decl :: <arg-declaration>) => (dir :: <symbol>);

define method equate (decl :: <object-declaration>, name :: <string>) => ();
  if (instance?(decl.type.true-type, <predefined-type-declaration>))
    error("Cannot 'equate:' predefined type: %s.", decl.type.simple-name);
  end if;
  decl.equated := name;
end method equate;

define method find-dylan-name
    (decl :: <function-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, read-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  if (decl.inlined-string = "") decl.inlined-string := inlining end if;
  find-dylan-name(decl.type, mapper, prefix, #(), read-only, sealing, inlining);
  decl.d-name
    | (decl.d-name := compute-dylan-name(decl, mapper, prefix, containers,
                                         read-only, sealing, inlining));
end method find-dylan-name;

define method compute-dylan-name
    (decl :: <function-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  find-dylan-name(decl.type, mapper, prefix, containers, rd-only, sealing, inlining);
  mapper(#"function", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <function-declaration>)
 => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;

    // Don't declare the function type -- just do its parameters.
    compute-closure(results, decl.type.result);
    for (elem in decl.type.parameters)
      if (~instance?(elem, <varargs-declaration>))
        compute-closure(results, elem)
      end if;
    end for;

    push-last(results, decl);
  end if;
  results;
end method compute-closure;

define method mapped-name
    (decl :: <object-declaration>, #key explicit-only?)
 => (result :: false-or(<string>));
  decl.map-type | mapped-name(decl.type, explicit-only?: #t)
    | (~explicit-only? & decl.type-name);
end method mapped-name;

define method type-name (decl :: <object-declaration>) => (result :: <string>);
  decl.equated | decl.type.type-name;
end method type-name;

define method find-dylan-name
    (decl :: <value-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, read-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  if (decl.sealed-string = "") decl.sealed-string := sealing end if;
  decl.d-name
    | (decl.d-name := compute-dylan-name(decl, mapper, prefix, containers,
                                         read-only, sealing, inlining));
end method find-dylan-name;

define method find-dylan-name
    (decl :: <object-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  if (decl.sealed-string = "") decl.sealed-string := sealing end if;
  if (decl.read-only == #()) decl.read-only := rd-only end if;
  find-dylan-name(decl.type, mapper, prefix, #(), rd-only, sealing, inlining);
  decl.d-name
    | (decl.d-name := compute-dylan-name(decl, mapper, prefix, containers,
                                         rd-only, sealing, inlining));
end method find-dylan-name;

define method compute-dylan-name
    (decl :: <object-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  mapper(#"variable", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method find-dylan-name
    (decl :: <variable-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>, #next next-method)
 => (result :: <string>);
  next-method();
  decl.getter := decl.getter | decl.d-name;
  decl.setter := decl.setter | concatenate(decl.d-name, "-setter");
  decl.d-name;
end method find-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <object-declaration>) => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    compute-closure(results, decl.type);
    push-last(results, decl);
  end if;
  results;
end method compute-closure;

define method find-dylan-name
    (decl :: <arg-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>, #next next-method)
 => (result :: <string>);
  if (decl.original-type)
    find-dylan-name(decl.original-type, mapper, prefix, #(), rd-only,
                    sealing, inlining);
  end if;
  next-method();
end method find-dylan-name;

define method compute-dylan-name
    (decl :: <arg-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  mapper(#"variable", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <result-declaration>)
 => (results :: <deque>);
  // We don't want to declare the args themselves, but we should make sure we
  // have the arg types.
  compute-closure(results, decl.type);
end method compute-closure;

define method compute-closure
    (results :: <deque>, decl :: <arg-declaration>)
 => (results :: <deque>);
  // We don't want to declare the args themselves, but we should make sure we
  // have the arg types.
  if (decl.original-type)
    compute-closure(results, decl.original-type);
  end if;
  compute-closure(results, decl.type);
end method compute-closure;

define method find-parameter
    (decl :: <function-declaration>, param :: <object>)
 => (result :: <arg-declaration>);
  find-parameter(decl.type, param);
end method find-parameter;

define method find-parameter
    (decl :: <function-type-declaration>, param :: <integer>)
 => (result :: <arg-declaration>);
  element(decl.parameters, param - 1, default: #f)
    | error("%s: No such parameter: %d. (There are only %d parameters.)",
            decl.simple-name, param, size(decl.parameters));
end method find-parameter;

define method find-parameter
    (decl :: <function-type-declaration>, param :: <string>)
 => (result :: <arg-declaration>);
  any?(method (arg) arg.simple-name = param & arg end, decl.parameters)
    | error("%s: No such parameter: %s.", decl.simple-name, param);
end method find-parameter;

define method find-parameter
    (decl :: <function-type-declaration>, param :: <symbol>)
 => (result :: <arg-declaration>);
  error("Cannot currently identify parameters by symbols.");
end method find-parameter;

define method find-result
    (decl :: <function-declaration>) => (result :: <result-declaration>);
  find-result(decl.type);
end method find-result;

define method find-result
    (decl :: <function-type-declaration>) => (result :: <result-declaration>);
  decl.result;
end method find-result;

define method ignored?-setter (value == #t, decl :: <result-declaration>)
 => (result :: <boolean>);
  decl.type := void-type;
  #t;
end method ignored?-setter;

define method argument-direction-setter
    (dir :: <symbol>, decl :: <arg-declaration>) => (dir :: <symbol>);
  if (decl.direction ~= #"default")
    error("Parameter direction cannot be respecified.");
  end if;
  if (dir ~= #"in")
    let ptype = decl.type.true-type;
    if (~instance?(ptype, <pointer-declaration>))
      error("Output parameter '%s' is %s (%s), but a pointer type was expected.",
            decl.simple-name, ptype.simple-name, ptype.dylan-name);
    end if;
  end if;
  decl.direction := dir;
end method argument-direction-setter;

//------------------------------------------------------------------------

define abstract class <constant-declaration> (<declaration>)
  slot constant-value :: <object>, required-init-keyword: #"value";
end class;

define class <enum-slot-declaration> (<constant-declaration>)
  slot containing-enum-declaration :: <enum-declaration>;
  slot excluded? :: <boolean> = #f;
end class;

define method exclude-decl (decl :: <enum-slot-declaration>)
  decl.declared? := #t;
  decl.excluded? := #t;
end method exclude-decl;

define method compute-dylan-name
    (decl :: <enum-slot-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);

  // (Do not emit container prefixes for enum constants. C semantics
  // put all enums in the global namespace.)

  ignore(containers);
  mapper(#"constant", prefix, decl.simple-name, #());
end method compute-dylan-name;

define method compute-closure
    (results :: <deque>, decl :: <enum-slot-declaration>) => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    compute-closure(results, decl.containing-enum-declaration);
    push-last(results, decl);
  end if;
  results;
end method compute-closure;

define class <macro-declaration> (<constant-declaration>) end class;

// Attempts to add a new declarations corresponding to a CPP macro.  The value
// may be another declaration; or a constant value; or it may be
// indeterminate, in which case no declaration will be added.
//
define generic add-cpp-declaration
    (state :: <parse-state>, macro-name :: <string>) => ();

define method compute-dylan-name
    (decl :: <constant-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  mapper(#"constant", prefix, decl.simple-name, containers);
end method compute-dylan-name;

define method add-cpp-declaration
    (state :: <parse-state>, macro-name :: <string>) => ();
  block ()
    let value = parse-macro(macro-name, state);
    state.objects[macro-name] :=
      add-declaration(state, make(<macro-declaration>, name: macro-name,
                                  value: value));
  exception (err :: <error>)
    parse-progress-report(state, "Error: %s", err);
    #f;
  end block;
end method add-cpp-declaration;

define method compute-closure
    (results :: <deque>, decl :: <macro-declaration>) => (results :: <deque>);
  if (~decl.declared?)
    decl.declared? := #t;
    if (instance?(decl.constant-value, <declaration>))
      compute-closure(results, decl.constant-value);
    end if;
    push-last(results, decl);
  end if;
  results;
end method compute-closure;

define method compute-dylan-name
    (decl :: <macro-declaration>, mapper :: <function>, prefix :: <string>,
     containers :: <sequence>, rd-only :: <boolean>, sealing :: <string>,
     inlining :: <string>)
 => (result :: <string>);
  // If we are aliasing another declaration, we should use the same category.
  // We should only use #"constant" if we are renaming a constant or have an
  // actual constant value
  let category = select (decl.constant-value by instance?)
                   <constant-declaration> => #"constant";
                   <type-declaration> => #"type";
                   <value-declaration> => #"variable";
                   otherwise => #"constant";
                 end select;
  mapper(category, prefix, decl.simple-name, containers);
end method compute-dylan-name;

// Seals for file c-decl.dylan

// <struct-declaration> -- subclass of <new-static-pointer>, <structured-type-declaration>
define sealed domain make(singleton(<struct-declaration>));
// <union-declaration> -- subclass of <new-static-pointer>, <structured-type-declaration>
define sealed domain make(singleton(<union-declaration>));
// <enum-declaration> -- subclass of <structured-type-declaration>
define sealed domain make(singleton(<enum-declaration>));
// <pointer-declaration> -- subclass of <new-static-pointer>, <type-declaration>
define sealed domain make(singleton(<pointer-declaration>));
// <vector-declaration> -- subclass of <new-static-pointer>, <type-declaration>
define sealed domain make(singleton(<vector-declaration>));
// <function-type-declaration> -- subclass of <type-declaration>
define sealed domain make(singleton(<function-type-declaration>));
// <block-type-declaration> -- subclass of <function-type-declaration>
define sealed domain make(singleton(<block-type-declaration>));
// <typedef-declaration> -- subclass of <type-declaration>, <typed>
define sealed domain make(singleton(<typedef-declaration>));
// <incomplete-type-declaration> -- subclass of <type-declaration>
define sealed domain make(singleton(<incomplete-type-declaration>));
// <predefined-type-declaration> -- subclass of <type-declaration>
define sealed domain make(singleton(<predefined-type-declaration>));
// <integer-type-declaration> -- subclass of <predefined-type-declaration>
define sealed domain make(singleton(<integer-type-declaration>));
// <signed-integer-type-declaration> -- subclass of <integer-type-declaration>
define sealed domain make(singleton(<signed-integer-type-declaration>));
// <unsigned-integer-type-declaration> -- subclass of <integer-type-declaration>
define sealed domain make(singleton(<unsigned-integer-type-declaration>));
// <float-type-declaration> -- subclass of <predefined-type-declaration>
define sealed domain make(singleton(<float-type-declaration>));
// <function-declaration> -- subclass of <value-declaration>
define sealed domain make(singleton(<function-declaration>));
// <object-declaration> -- subclass of <value-declaration>
define sealed domain make(singleton(<object-declaration>));
// <variable-declaration> -- subclass of <object-declaration>
define sealed domain make(singleton(<variable-declaration>));
// <slot-declaration> -- subclass of <object-declaration>
define sealed domain make(singleton(<slot-declaration>));
// <result-declaration> -- subclass of <object-declaration>
define sealed domain make(singleton(<result-declaration>));
// <arg-declaration> -- subclass of <object-declaration>
define sealed domain make(singleton(<arg-declaration>));
// <varargs-declaration> -- subclass of <arg-declaration>
define sealed domain make(singleton(<varargs-declaration>));
// <enum-slot-declaration> -- subclass of <constant-declaration>
define sealed domain make(singleton(<enum-slot-declaration>));
// <macro-declaration> -- subclass of <constant-declaration>
define sealed domain make(singleton(<macro-declaration>));
