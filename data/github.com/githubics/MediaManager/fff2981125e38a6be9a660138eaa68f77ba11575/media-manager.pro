TEMPLATE = subdirs

SUBDIRS += core \
# ics-plugins \

core.file = core/media-manager.pro
# core.depends += ics-plugins

message ("Building Media Manager")
message ("To build everything add: \"CONFIG+=build_all QJSONRPC_LIBRARY_TYPE=staticlib\" as qmake additional arguments")
message ("Note that this only needs to be done once for the project and you can remove the above for subsequent builds until the libraries need to be rebuilt")
message ("To build with C++11 support: \"CONFIG+=c++11\"")

CONFIG (build_all) {
    SUBDIRS += \
    MediaInfoLib \
    ZenLib \
#    qjsonrpc
#

    ZenLib.file = ZenLib/Project/Qt/ZenLib.pro
    MediaInfoLib.file = MediaInfoLib/Project/Qt/MediaInfoLib.pro
    MediaInfoLib.depends=ZenLib
    core.depends+=MediaInfoLib
#    ics-plugins.depends+=qjsonrpc
}


