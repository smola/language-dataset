# Turn on C++11, this is not necessary.
# It's just handy to have it ready
CONFIG += c++11

# [Library Dependencies]
# Often when linking against a library, qmake relies on the underlying platform to know what other libraries this library links against,
# and lets the platform pull them in. In many cases, however, this is not sufficent. For example, when statically linking a library,
# no other libraries are linked to, and therefore no dependencies to those libraries are created.
# However, an application that later links against this library will need to know where to find the symbols that the static library will require.
# To help with this situation, qmake attempts to follow a library's dependencies where appropriate, but this behavior must be explicitly enabled by following two steps.
# The first step is to enable dependency tracking in the library itself. To do this you must tell qmake to save information about the library:
CONFIG += create_prl
# This is only relevant to the lib template, and will be ignored for all others. When this option is enabled, qmake will create a file ending in .prl
# which will save some meta-information about the library. This metafile is just like an ordinary project file, but only contains internal variable declarations.
# You are free to view this file and, if it is deleted, qmake will know to recreate it when necessary, either when the project file is later read,
# or if a dependent library (described below) has changed. When installing this library, by specifying it as a target in an INSTALLS declaration,
# qmake will automatically copy the .prl file to the installation path.
# The second step in this process is to enable reading of this meta information in the applications that use the static library:
CONFIG += link_prl

# [Usage for this macro]
# These two functions sets up the dependencies for libraries that are build with
# this project. Specify the lib you need to depend on in the variable "DEPENDENCY_PROJECT".
# Make sure that the library project name and target are the same!
# eg: I want to use target DriverX and DriverY in my qt project.
#   DEPENDENCY_PROJECT += projectX
#   DEPENDENCY_PROJECT += projectY
#   Or
#   DEPENDENCY_PROJECT += projectX projectY
TARGET_DIRECTORY_NAME = 0           #The target directory name (just the project folder name)
TARGET_PATH = 0                     #The path to the project directory from the root folder
LIB_PATH = 0                        #The path to the LIB

# [The MACRO]
# The "dep" variable that is used in these two for loops will hold the paramaters from the DEPENDENCY_PROJECT.
# From the example above this would hold projectX and projectY (a litteral string)
# Now this function will loop through all the parameters

# [Setup the Dependencies]
for(dep, DEPENDENCY_PROJECT) {
    TARGET_NAME = $${dep} # The name of the depending target
    message($${TARGET}.depends = $${TARGET_NAME})
    $${TARGET}.depends += $${TARGET_NAME}
}
# [setup the actual library dependencies]
for(dep, DEPENDENCY_PROJECT) {
    TARGET_NAME = $${dep}                       # The name of the depending target
    TARGET_PATH = $${PWD}/$${TARGET_NAME}       # The path to the depending target source
    LIB_PATH = $${OUT_PWD}/../$${TARGET_NAME}   # The path to the depending compiled target
    #message(Depending target \"$${TARGET_NAME}\" source path: $${TARGET_PATH})
    #message(Depending target \"$${TARGET_NAME}\" compiled path: $${LIB_PATH})

    # Adds the wanted lib to the linker
    # Windows
    win32:CONFIG(release, debug|release): LIBS += -L$${LIB_PATH}/release/ -l$${TARGET_NAME}
    else:win32:CONFIG(debug, debug|release): LIBS += -L$${LIB_PATH}/debug/ -l$${TARGET_NAME}
    # UNIX
    unix: LIBS += -L$${LIB_PATH}/ -l$${TARGET_NAME}

    # Adds the wanted lib to the project.
    INCLUDEPATH += $${TARGET_PATH}
    #message(INCLUDEPATH: $${INCLUDEPATH})

    # Adds a dependpath to the project
    # This forces a rebuild if the headers change
    DEPENDPATH += $${TARGET_PATH}
    #message(DEPENDPATH: $${DEPENDPATH})

    #Pre target
    PRE_TARGETDEPS += $${TARGET_PATH}
    #message(PRE_TARGETDEPS: $${PRE_TARGETDEPS})
}
