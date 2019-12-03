QT += qml
CONFIG += c++11

# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += \
        main.cpp

RESOURCES += qml.qrc


ANDROID_DEPLOYMENT_DEPENDENCIES += \
    jar/QtAndroid.jar \
    jar/QtAndroidExtras.jar \
    lib/libQt5Core.so \
    lib/libQt5Gui.so \
    lib/libQt5Network.so \
    lib/libQt5Qml.so \
    lib/libQt5Quick.so \
    lib/libQt5AndroidExtras.so \
    plugins/platforms/android/libqtforandroid.so \
    qml/QtQuick.2/qmldir \
    qml/QtQuick.2/libqtquick2plugin.so \
    qml/QtQuick/Window.2/libwindowplugin.so \
    qml/QtQuick/Window.2/plugins.qmltypes \
    qml/QtQuick/Window.2/qmldir \
