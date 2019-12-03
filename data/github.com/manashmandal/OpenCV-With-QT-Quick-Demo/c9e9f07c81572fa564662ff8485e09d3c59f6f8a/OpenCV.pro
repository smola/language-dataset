#-------------------------------------------------
#
# Project created by QtCreator 2015-02-08T14:07:22
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = OpenCV
TEMPLATE = app


SOURCES += main.cpp\
        dialog.cpp

HEADERS  += dialog.h

FORMS    += dialog.ui

INCLUDEPATH += C://opencv//sources//release//install//include

LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_contrib2410.dll.a

LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_core2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_features2d2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_flann2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_gpu2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_highgui2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_imgproc2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_legacy2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_ml2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_nonfree2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_objdetect2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_ocl2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_photo2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_stitching2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_superres2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_ts2410.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_video2410.dll.a
LIBS += C://opencv//sources//release//install//x64//mingw//lib//libopencv_videostab2410.dll.a
