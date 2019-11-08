@license{
   Copyright (c) 2009-2017 CWI NWO-I
   All rights reserved. This program and the accompanying materials
   are made available under the terms of the Eclipse Public License v1.0
   which accompanies this distribution, and is available at
   http://www.eclipse.org/legal/epl-v10.html
 }
@contributor{Tijs van der Storm - storm@cwi.nl - CWI}

@doc{Discover IDE services (contributions) through tags in Rascal code.

Contributions (see util::IDE) are found and registered by finding functions
annotated by tags indicating the contribution and language name.

Supported:

@Parser{MyLanguageName:myExtension}
start[MyStartNonterminal] myParser(str src, loc l) { ... }

// for builder, annotator, liveUpdater, outliner
@<ContributionName>{MyLanguageName}
Type1 functionName(Type2 t) { ... }

..or..

// for popup, menu, categories
@<ContributionName>{MyLanguageName}    
public Type variableName = someValue; 

TODO:
- check types of functions
- support syntaxProperties
- support proposer

}
module util::DiscoverIDE

import util::Reflective;
import util::FileSystem;
import util::IDE;
import util::Eval;
import lang::rascal::\syntax::Rascal;
import IO;
import String;
import Type;
import ParseTree;


list[loc] searchPathDirs() 
  = [ |file://<replaceLast(dir, "/bin", "/src")>|  
       | str dir <- split(":", getRascalClasspath()), endsWith(dir, "/bin") ];

void discoverIDE(list[loc] dirs = searchPathDirs()) {
  names = contributionNames();
  mName = "";
  
  rel[str, str, str, str] langs = {};
  map[str, map[str, tuple[str, str]]] contribs = ();
  
  void addContrib(str lang, Name x, Name decl) {
    str c = uncapitalize("<x>");
    if (c in names) { 
      println("Discovered contribution <mName>::<decl> as <c> for language <lang>");
      if (lang notin contribs) {
        contribs[lang] = ();
      }
      contribs[lang][c] = <mName, "<decl>">;
    }
  }

  for (loc srcDir <- dirs, loc m <- files(srcDir), m.extension == "rsc") {
    t = parseModule(m);
    mName = "<t.header.name>";
   
    visit (t) {
      case FunctionDeclaration fd: {
        for (Tag tg <- fd.tags.tags) {
          switch (tg) {
            case (Tag)`@Parser <TagString ts>`: {
              if ([str l, str e] := split(":", "<ts>"[1..-1])) {
                println("Discovered language <l> with parser <mName>::<fd.signature.name>");
                langs += {<l, e, mName, "<fd.signature.name>">}; 
              } 
            }
            case (Tag)`@<Name tagName> <TagString ts>`: 
              addContrib("<ts>"[1..-1], tagName, fd.signature.name);
          }
        }
      }
      
      case (Declaration)`<Tags tags> <Visibility _> <Type _> <{Variable ","}+ vs>;`: {
        for ((Tag)`@<Name tagName> <TagString ts>` <- tags.tags, Variable v <- vs) {
           addContrib("<ts>"[1..-1], tagName, v.name);
        }
      }
      
    }
    
  }

  for (<str lang, str ext, str mName, str func> <- langs) {
    if (Tree (str input, loc origin) p := 
        eval(["import ParseTree;", "import <mName>;", "<mName>::<func>;"]).val) {
      println("Registering lang: <lang>");
      registerLanguage(lang, ext, p);
      set[Contribution] realContribs = {};
      for (str contribName <- contribs[lang]) {
        <contribModule, funcName> = contribs[lang][contribName];
        println("Registering contribution <contribName> for <lang>...");
        unimport(contribModule);
        r = eval(#Contribution, ["import util::IDE;" 
          , "import <contribModule>;"
          , "<contribName>(<contribModule>::<funcName>);"]);
        realContribs += {r.val};
      }
      registerContributions(lang, realContribs); 
    }
  }  
}

private set[str] contributionNames() = 
  { x | cons(label(str x, _), _, _, _) <-  #Contribution.definitions[adt("Contribution", [])].alternatives };



