#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
namespace eval pwoo {
  variable classes [dict create]
  variable classStack {}
  variable cmdFlags [dict create]
  variable verbose 0


  namespace export class
  proc class {className args} {
    variable classes
    variable classStack

    if { [llength $classStack] > 0 } {
      set className "[join $classStack "::"]::$className"
      error "Nested classes are unsupported ($className)"
    }
    lappend classStack $className

    set superClasses [dict create]
    dict set superClasses public [list]
    dict set superClasses private [list]
    # working dict only merged with classes on success
    set classDict [dict create]

    vputs "Defining class $className"

    if { 0 == [llength $args] } {
      # assume forward declare "class clsName"
      # dict count will be set below as placeholder
      set sfx ""
      if { [classExists $className] } {
        set sfx " (Redundant)"
      }
      vputs "  Forward declare$sfx"
    } elseif { [classBodyExists $className] } {
      error "Class body already exists for '$className'"
    } elseif { 1 == [llength $args] } {
      # assume args == "body"
      dict set classDict $className body [lindex $args 0]
    } elseif { ":" != [lindex $args 0] || [llength $args] < 3 } {
      # too few args - minimal args == ": superClassName body"
      error "Invalid class declaration at '[string range $args 0 30]'"
    } else {
      # assume args == ": public|private superClassName... body"
      # body must be last arg
      dict set classDict $className body [lindex $args end]
      # strip off first and last args
      set args [lrange $args 1 end-1]
      # process remaining args as visibility flags or superclasses
      set superClassVis private
      foreach arg $args {
        switch $arg {
        public -
        private {
          set superClassVis $arg
        }
        default {
          # arg must be a class name
          addSuperClass $arg $superClassVis superClasses
        }
        }
      }
    }

    if { [dict exists $classDict $className body] } {
      set classDeclare::className $className
      set classDeclare::classDict [dict create]
      namespace eval classDeclare [dict get $classDict $className body]
      set classDict [dict merge $classDict $classDeclare::classDict]


      ## make procs in classDeclare visible to $className namespace
      #namespace eval $className {namespace path ::pwoo::classDeclare}
      ##namespace upvar ns classDict classDict
      #
      ## process class body declaration
      #namespace eval $className [dict get $classDict $className body]
      #
      #vputs "  $className vars: [info vars ::pwoo::${className}::*]"
    }

    if { ![classBodyExists $className] } {
      dict set classDict $className count 0
      dict set classDict $className superclasses public [dict get $superClasses public]
      dict set classDict $className superclasses private [dict get $superClasses private]
      set classes [dict merge $classes $classDict]
    }
    unset classDict

    vputs "End class $className"
    vputs ""

    # strip last item from list - needed when nested classes are implemented
    set classStack [lreplace $classStack end end]
  }


  namespace export new
  proc new {className args} {
    variable classes

    if { ![classExists $className] } {
      error "Could not instantiate undefined class '$className'."
    } elseif { ![classBodyExists $className] } {
      error "Could not instantiate incomplete class '$className'."
    }

    set key [dict get $classes $className count]
    set body [dict get $classes $className body]
    set cmd ${className}_${key}

    vputs ""
    vputs "new '$className'"
    vputs "  cmd: '$cmd'"
    vputs "  path: '[namespace eval $cmd {namespace path}]'"

    # make procs in classInstance visible to object's namespace
    namespace eval $cmd {namespace path ::pwoo::classInstance}

    # load class body into object namespace
    namespace eval $cmd $body

    # Declare the this variable in object namespace
    namespace eval $cmd [list variable this $cmd]

    # Create object namespace ensemble
    namespace eval $cmd [list namespace ensemble create -command ::$cmd]

    # exec object ctor if one defined
    if { [ctorFlag $cmd] } {
      set ctor [makeCtorName $cmd]
      vputs "  ctor: $ctor"
      namespace eval $cmd [list $ctor {*}$args]
    } else {
      vputs "  ctor: not defined"
    }

    if { [dtorFlag $cmd] } {
      vputs "  dtor: class defined"
    } else {
      vputs "  dtor: created default"
      namespace eval $cmd {destructor {}}
    }

    dict set classes $className count [incr key]

    # hide classInstance procs
    namespace eval $cmd {namespace path ""}

    vputs "  $cmd methods: [namespace eval $cmd {namespace export}]"
    vputs ""

    return $cmd
  }


  proc vputs { {msg ""} } {
    variable verbose
    if { $verbose } {
      puts $msg
    }
  }


  proc veval { script } {
    variable verbose
    if { $verbose } {
      uplevel 1 $script
    }
  }


  proc setCmdFlag { cmd flag val } {
    variable cmdFlags
    dict set cmdFlags $cmd $flag $val
  }


  proc getCmdFlag { cmd flag defVal } {
    variable cmdFlags
    if { ![dict exists $cmdFlags $cmd $flag] } {
      set val $defVal
    } else {
      set val [dict get $cmdFlags $cmd $flag]
    }
    return $val
  }


  proc setVisiblityFlag { cmd val } {
    setCmdFlag $cmd visibility $val
  }


  proc visiblityFlag { cmd {defVis private} } {
    return [getCmdFlag $cmd visibility $defVis]
  }


  proc setCtorFlag { cmd val } {
    setCmdFlag $cmd hasCtor $val
  }


  proc ctorFlag { cmd } {
    return [getCmdFlag $cmd hasCtor 0]
  }


  proc setDtorFlag { cmd val } {
    setCmdFlag $cmd hasDtor $val
  }


  proc dtorFlag { cmd } {
    return [getCmdFlag $cmd hasDtor 0]
  }


  proc dumpDict {} {
    variable classes
    puts "*** BEGIN pwoo::classes ***"
    dict for {className classAttrs} $classes {
      set status ""
      if { ![classBodyExists $className] } {
        set status " INCOMPLETE"
      }
      puts "*** ${className}${status} ***"
      dict for {attrName attrVal} $classAttrs {
        if {"body" == $attrName } {
          set attrVal "'[string range $attrVal 0 30] ... [string range $attrVal end-30 end]'"
        }
        puts "  $attrName: $attrVal"
      }
    puts "*** END pwoo::classes ***"
      puts ""
    }
  }


  proc addSuperClass { superClassName visibility superClassesVar errVar } {
    upvar $superClassesVar superClasses
    upvar $errVar err
    set ret 0
    if { 1 != [llength $superClassName] } {
      # spaces not allowed in name
      error "Invalid superclass name '$superClassName'"
    } elseif { ![classExists $superClassName] } {
      error "Superclass does not exist '$superClassName'"
    } elseif { -1 != [lsearch [dict get $superClasses public] $superClassName] } {
      error "Duplicate public superclass '$superClassName'"
    } elseif { -1 != [lsearch [dict get $superClasses private] $superClassName] } {
      error "Duplicate private superclass '$superClassName'"
    } else {
      dict lappend superClasses $visibility $superClassName
      set ret 1
    }
    return $ret
  }


  proc createMethod {ns vis methName methArgs body} {
    # create proc for method in namespace $ns. Inject the 'this'
    # variable into the body of the proc
    namespace eval $ns [list proc $methName $methArgs [concat {variable this ;} $body]]
    if { $vis == "public" } {
      namespace eval $ns [list namespace export $methName]
    }

    vputs "  METHOD of $ns\n"
    vputs "    visibility: '$vis'"
    vputs "    method    : '$methName'"
    vputs "    args      : '[concat $methArgs]'"
    vputs ""
  }


  proc setVerbose { val } {
    variable verbose
    set verbose $val
  }


  proc isVerbose {} {
    variable verbose
    return $verbose
  }


  proc classExists { className } {
    variable classes
    return [dict exists $classes $className count]
  }


  proc classBodyExists { className } {
    variable classes
    return [dict exists $classes $className body]
  }


  proc makeCtorName { cmd } {
    return "$cmd"
  }


  proc makeDtorName { cmd } {
    return "delete"
  }
}


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
namespace eval pwoo::classDeclare {
  namespace path ::pwoo
  variable className
  variable classDict


  proc constructor {ctorArgs body} {
    puts "  constructor \{ [concat $ctorArgs] \} \{[string range [concat $body] 0 30]...\}"
    #set ns [uplevel 1 {namespace current}]
    #set cmd [namespace tail $ns]
    #createMethod $ns private [makeCtorName $cmd] $ctorArgs $body
    #setCtorFlag $cmd 1
  }


  proc destructor {body} {
    puts "  destructor \{[string range [concat $body] 0 30]...\}"
    #set ns [uplevel 1 {namespace current}]
    #set cmd [namespace tail $ns]
    #createMethod $ns public [makeDtorName $cmd] {} [concat $body \; namespace delete $ns]
    #setDtorFlag $cmd 1
  }


  proc method {methName methArgs body} {
    puts "  method $methName \{ [concat $methArgs] \} \{[string range [concat $body] 0 30]...\}"
    #set ns [uplevel 1 {namespace current}]
    #createMethod $ns [visiblityFlag [namespace tail $ns]] $methName $methArgs $body
  }


  proc private: {} {
    puts "  PRIVATE"
    #set ns [uplevel 1 {namespace current}]
    #setVisiblityFlag [namespace tail $ns] private
  }


  proc public: {} {
    puts "  PUBLIC"
    #set ns [uplevel 1 {namespace current}]
    #setVisiblityFlag [namespace tail $ns] public
  }


  proc variable { name args } {
    puts "  variable $name \{$args\}"
    #vputs "  class variable [uplevel 1 {namespace current}]::$name"
    #uplevel 1 [list ::variable $name]
    set ns [uplevel 1 {namespace current}]
    addClassVariable [namespace tail $ns] $name $args
  }


  proc addClassVariable { cmd varName varValue } {
    ::variable classDict
  }
}


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
namespace eval pwoo::classInstance {
  namespace path ::pwoo


  proc constructor {ctorArgs body} {
    set ns [uplevel 1 {namespace current}]
    set cmd [namespace tail $ns]
    createMethod $ns private [makeCtorName $cmd] $ctorArgs $body
    setCtorFlag $cmd 1
  }


  proc destructor {body} {
    set ns [uplevel 1 {namespace current}]
    set cmd [namespace tail $ns]
    createMethod $ns public [makeDtorName $cmd] {} [concat $body \; namespace delete $ns]
    setDtorFlag $cmd 1
  }


  proc method {methName methArgs body} {
    set ns [uplevel 1 {namespace current}]
    createMethod $ns [visiblityFlag [namespace tail $ns]] $methName $methArgs $body
  }


  proc private: {} {
    set ns [uplevel 1 {namespace current}]
    setVisiblityFlag [namespace tail $ns] private
  }


  proc public: {} {
    set ns [uplevel 1 {namespace current}]
    setVisiblityFlag [namespace tail $ns] public
  }


  proc variablex { name args } {
    vputs "  variable [uplevel 1 {namespace current}]::$name $args"
    uplevel 1 [list ::variable $name {*}$args]
  }
}
