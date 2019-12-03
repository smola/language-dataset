# Add more folders to ship with the application, here
folder_01.source = qml/qtandroidrunner
folder_01.target = qml
DEPLOYMENTFOLDERS = folder_01

# Additional import path used to resolve QML modules in Creator's code model
QML_IMPORT_PATH =

# The .cpp file which was generated for your project. Feel free to hack it.
SOURCES += main.cpp \
    qtandroidrunner.cpp

QT += androidextras
ANDROID_PACKAGE_SOURCE_DIR = $$PWD/android

# Installation path
# target.path =

# Please do not modify the following two lines. Required for deployment.
include(qtquick2applicationviewer/qtquick2applicationviewer.pri)
qtcAddDeployment()

OTHER_FILES += \
    README.md \
    android/src/com/github/qt/QtRunner.java

HEADERS += \
    qtandroidrunner.h

