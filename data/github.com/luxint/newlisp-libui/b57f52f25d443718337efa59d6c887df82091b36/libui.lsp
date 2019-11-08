;; @module libui.lsp
;; @description Functions for programming native GUI's on OSX, LinuX or Windows.
;; @version 0.1 initial release
;; @author FdB 2018
;;
;;
;; A newLisp binding to the libui library. You can download the latest from 
;; @link https://github.com/andlabs/libui/releases
;; Put the libui.so (Linux), libui.dll (Windows) or libui.dylib (OSX) into 
;; "/usr/local/share/newlisp/" (Linux & OSX
;; "c:/Program Files/newlisp/" (Windows).

(context 'ui)

(set 'lib (lookup ostype '(("Linux" ".so") ("OSX" ".dylib") ( "Windows" ".dll"))))
(set 'libui (string (env "NEWLISPDIR") "/libui" lib)) 

(import libui "uiInit")
(import libui "uiUninit")
;(import libui "uiFreeInitError")
(import libui "uiMain")
;(import libui "uiMainSteps")
;(import libui "uiMainStep")
(import libui "uiQuit")
;(import libui "uiQueueMain")
(import libui "uiTimer")
(import libui "uiOnShouldQuit" "void" "void*" "void*")
;(import libui "uiFreeText")
(import libui "uiControlDestroy")
(import libui "uiControlHandle")
(import libui "uiControlParent")
(import libui "uiControlSetParent")
(import libui "uiControlToplevel")
(import libui "uiControlVisible")
(import libui "uiControlShow")
(import libui "uiControlHide")
(import libui "uiControlEnabled")
(import libui "uiControlEnable")
(import libui "uiControlDisable")
;(import libui "uiAllocControl")
;(import libui "uiFreeControl")
;(import libui "uiControlVerifySetParent")
;(import libui "uiControlEnabledToUser")
;(import libui "uiUserBugCannotSetParentOnToplevel")
(import libui "uiWindowTitle")
(import libui "uiWindowSetTitle")
(import libui "uiWindowContentSize")
(import libui "uiWindowSetContentSize")
(import libui "uiWindowFullscreen")
(import libui "uiWindowSetFullscreen")
(import libui "uiWindowOnContentSizeChanged")
(import libui "uiWindowOnClosing" "void" "void*" "void*" "void*")
(import libui "uiWindowBorderless")
(import libui "uiWindowSetBorderless")
(import libui "uiWindowSetChild")
(import libui "uiWindowMargined")
(import libui "uiWindowSetMargined")
(import libui "uiNewWindow")
(import libui "uiButtonText")
(import libui "uiButtonSetText")
(import libui "uiButtonOnClicked" "void" "void*" "void*" "void*")
(import libui "uiNewButton")
(import libui "uiBoxAppend")
(import libui "uiBoxDelete")
(import libui "uiBoxPadded")
(import libui "uiBoxSetPadded")
(import libui "uiNewHorizontalBox")
(import libui "uiNewVerticalBox")
(import libui "uiCheckboxText")
(import libui "uiCheckboxSetText")
(import libui "uiCheckboxOnToggled")
(import libui "uiCheckboxChecked")
(import libui "uiCheckboxSetChecked")
(import libui "uiNewCheckbox")
(import libui "uiEntryText")
(import libui "uiEntrySetText")
(import libui "uiEntryOnChanged")
(import libui "uiEntryReadOnly")
(import libui "uiEntrySetReadOnly")
(import libui "uiNewEntry")
(import libui "uiNewPasswordEntry")
(import libui "uiNewSearchEntry")
(import libui "uiLabelText")
(import libui "uiLabelSetText")
(import libui "uiNewLabel")
(import libui "uiTabAppend")
(import libui "uiTabInsertAt")
(import libui "uiTabDelete")
(import libui "uiTabNumPages")
(import libui "uiTabMargined")
(import libui "uiTabSetMargined")
(import libui "uiNewTab")
(import libui "uiGroupTitle")
(import libui "uiGroupSetTitle")
(import libui "uiGroupSetChild")
(import libui "uiGroupMargined")
(import libui "uiGroupSetMargined")
(import libui "uiNewGroup")
(import libui "uiSpinboxValue")
(import libui "uiSpinboxSetValue")
(import libui "uiSpinboxOnChanged")
(import libui "uiNewSpinbox")
(import libui "uiSliderValue")
(import libui "uiSliderSetValue")
(import libui "uiSliderOnChanged")
(import libui "uiNewSlider")
(import libui "uiProgressBarValue")
(import libui "uiProgressBarSetValue")
(import libui "uiNewProgressBar")
(import libui "uiNewHorizontalSeparator")
(import libui "uiNewVerticalSeparator")
(import libui "uiComboboxAppend")
(import libui "uiComboboxSelected")
(import libui "uiComboboxSetSelected")
(import libui "uiComboboxOnSelected")
(import libui "uiNewCombobox")
(import libui "uiEditableComboboxAppend")
(import libui "uiEditableComboboxText")
(import libui "uiEditableComboboxSetText")
(import libui "uiEditableComboboxOnChanged")
(import libui "uiNewEditableCombobox")
(import libui "uiRadioButtonsAppend")
(import libui "uiRadioButtonsSelected")
(import libui "uiRadioButtonsSetSelected")
(import libui "uiRadioButtonsOnSelected")
(import libui "uiNewRadioButtons")
(import libui "uiDateTimePickerTime")
(import libui "uiDateTimePickerSetTime")
(import libui "uiDateTimePickerOnChanged")
(import libui "uiNewDateTimePicker")
(import libui "uiNewDatePicker")
(import libui "uiNewTimePicker")
(import libui "uiMultilineEntryText")
(import libui "uiMultilineEntrySetText")
(import libui "uiMultilineEntryAppend")
(import libui "uiMultilineEntryOnChanged")
(import libui "uiMultilineEntryReadOnly")
(import libui "uiMultilineEntrySetReadOnly")
(import libui "uiNewMultilineEntry")
(import libui "uiNewNonWrappingMultilineEntry")
(import libui "uiMenuItemEnable")
(import libui "uiMenuItemDisable")
(import libui "uiMenuItemOnClicked")
(import libui "uiMenuItemChecked")
(import libui "uiMenuItemSetChecked")
(import libui "uiMenuAppendItem")
(import libui "uiMenuAppendCheckItem")
(import libui "uiMenuAppendQuitItem")
(import libui "uiMenuAppendPreferencesItem")
(import libui "uiMenuAppendAboutItem")
(import libui "uiMenuAppendSeparator")
(import libui "uiNewMenu")
(import libui "uiOpenFile")
(import libui "uiSaveFile")
(import libui "uiMsgBox")
(import libui "uiMsgBoxError")
(import libui "uiAreaSetSize")
(import libui "uiAreaQueueRedrawAll")
(import libui "uiAreaScrollTo")
(import libui "uiAreaBeginUserWindowMove")
(import libui "uiAreaBeginUserWindowResize")
(import libui "uiNewArea")
(import libui "uiNewScrollingArea")
(import libui "uiDrawNewPath")
(import libui "uiDrawFreePath")
(import libui "uiDrawPathNewFigure")
(import libui "uiDrawPathNewFigureWithArc")
(import libui "uiDrawPathLineTo")
(import libui "uiDrawPathArcTo")
(import libui "uiDrawPathBezierTo")
(import libui "uiDrawPathCloseFigure")
(import libui "uiDrawPathAddRectangle")
(import libui "uiDrawPathEnd")
(import libui "uiDrawStroke")
(import libui "uiDrawFill")
(import libui "uiDrawMatrixSetIdentity")
(import libui "uiDrawMatrixTranslate")
(import libui "uiDrawMatrixScale")
(import libui "uiDrawMatrixRotate")
(import libui "uiDrawMatrixSkew")
(import libui "uiDrawMatrixMultiply")
(import libui "uiDrawMatrixInvertible")
(import libui "uiDrawMatrixInvert")
(import libui "uiDrawMatrixTransformPoint")
(import libui "uiDrawMatrixTransformSize")
(import libui "uiDrawTransform")
(import libui "uiDrawClip")
(import libui "uiDrawSave")
(import libui "uiDrawRestore")
(import libui "uiFreeAttribute")
(import libui "uiAttributeGetType")
(import libui "uiNewFamilyAttribute")
(import libui "uiAttributeFamily")
(import libui "uiNewSizeAttribute")
(import libui "uiAttributeSize")
(import libui "uiNewWeightAttribute")
(import libui "uiAttributeWeight")
(import libui "uiNewItalicAttribute")
(import libui "uiAttributeItalic")
(import libui "uiNewStretchAttribute")
(import libui "uiAttributeStretch")
(import libui "uiNewColorAttribute")
(import libui "uiAttributeColor")
(import libui "uiNewBackgroundAttribute")
(import libui "uiNewUnderlineAttribute")
(import libui "uiAttributeUnderline")
(import libui "uiNewUnderlineColorAttribute")
(import libui "uiAttributeUnderlineColor")
(import libui "uiNewOpenTypeFeatures")
(import libui "uiFreeOpenTypeFeatures")
(import libui "uiOpenTypeFeaturesClone")
(import libui "uiOpenTypeFeaturesAdd")
(import libui "uiOpenTypeFeaturesRemove")
(import libui "uiOpenTypeFeaturesGet")
(import libui "uiOpenTypeFeaturesForEach")
(import libui "uiNewFeaturesAttribute")
(import libui "uiNewAttributedString")
(import libui "uiFreeAttributedString")
(import libui "uiAttributedStringString")
(import libui "uiAttributedStringLen")
(import libui "uiAttributedStringAppendUnattributed")
(import libui "uiAttributedStringInsertAtUnattributed")
(import libui "uiAttributedStringDelete")
(import libui "uiAttributedStringSetAttribute")
(import libui "uiAttributedStringForEachAttribute")
(import libui "uiAttributedStringNumGraphemes")
(import libui "uiAttributedStringByteIndexToGrapheme")
(import libui "uiAttributedStringGraphemeToByteIndex")
(import libui "uiDrawNewTextLayout")
(import libui "uiDrawFreeTextLayout")
(import libui "uiDrawText")
(import libui "uiDrawTextLayoutExtents")
(import libui "uiFontButtonFont")
(import libui "uiFontButtonOnChanged")
(import libui "uiNewFontButton")
(import libui "uiFreeFontButtonFont")
(import libui "uiColorButtonColor")
(import libui "uiColorButtonSetColor")
(import libui "uiColorButtonOnChanged")
(import libui "uiNewColorButton")
(import libui "uiFormAppend")
(import libui "uiFormDelete")
(import libui "uiFormPadded")
(import libui "uiFormSetPadded")
(import libui "uiNewForm")
(import libui "uiGridAppend")
(import libui "uiGridInsertAt")
(import libui "uiGridPadded")
(import libui "uiGridSetPadded")
(import libui "uiNewGrid")
(import libui "uiNewImage")
(import libui "uiFreeImage")
(import libui "uiImageAppend")
;(import libui "uiFreeTableValue")
(import libui "uiTableValueGetType")
(import libui "uiNewTableValueString")
(import libui "uiTableValueString")
(import libui "uiNewTableValueImage")
(import libui "uiTableValueImage")
(import libui "uiNewTableValueInt")
(import libui "uiTableValueInt")
(import libui "uiNewTableValueColor")
(import libui "uiTableValueColor")
(import libui "uiNewTableModel")
(import libui "uiFreeTableModel")
(import libui "uiTableModelRowInserted")
(import libui "uiTableModelRowChanged")
(import libui "uiTableModelRowDeleted")
(import libui "uiTableAppendTextColumn")
(import libui "uiTableAppendImageColumn")
(import libui "uiTableAppendImageTextColumn")
(import libui "uiTableAppendCheckboxColumn")
(import libui "uiTableAppendCheckboxTextColumn")
(import libui "uiTableAppendProgressBarColumn")
(import libui "uiTableAppendButtonColumn")
(import libui "uiNewTable")

(import (lookup ostype '(("Linux" "libc.so") ("OSX" "libc.dylib") ("Windows" "msvcrt.dll")))  "memset")
 
(struct 'uiInitOptions "unsigned int")
;; should be the type of size_t of your system

(set 'options (pack uiInitOptions 0))

;; 
;; <h1> Usage </h1>
;;
;; <h2> Setup and teardown </h2>
;; @syntax (ui:init)
;; Initializes the library. This must be used before creating widgets.
;; It will give an error message if the libranry cannot be initialized

(define (init)
	(memset (address options) 0 (length options))  
	(set 'err (uiInit (address options)))
	(when (!= err 0)
		(println "error initializing libui:" err)
		(exit)
	)
)

;; @syntax (ui:main)
;; Enters the main event loop. Note that all widgets must have been set up and displayed
;; before this point, otherwise they will not appear.
;;

(define (main)
 (uiMain)
)

;; @syntax (ui:uninit)
;; Frees all GUI resources
;;

(define (uninit)
	(uiUninit)
)

;; @syntax (ui:quit)
;; Ends the main event loop
;;

(define (quit)
	(uiQuit)
)

;; @syntax (ui:window <sym-id> <str-title> <int-width> <int-height> <int-has-menubar> <sym-function-handler> [<list-children>])
;; @param <sym-id> the name of the window
;; @param <str-title> the title of the window 
;; @param <int-width> the width of the window
;; @param <int-height> the hieght of the window
;; @param <int-has-menubar> either 1 (yes) or 0 (no) 
;; Creates a new window

(define (window id title width height has-menubar)
	(set id (uiNewWindow title width height has-menubar))
)

;; @syntax (ui:window-on-close <sym-id> <sym-function>)
;; @param <sym-id> the name of the window
;; @param <sym-function-on-close> the name of the function which get called after the close button is clicked
;; @return window-id
;; The function has to return int > 0  for the window to close. 0 prohibits closing

(define (window-on-close id on-close)	
	(uiWindowOnClosing id (callback on-close "int" "void*" "void*") 0)
)

;; @syntax (ui:window-on-change <sym-id> <sym-function>)
;; @param <sym-id> the id of the window
;; @param <sym-function-on-change> the name of the function which gets called after window is resized
;; @return window-id

(define (window-on-change id on-change)
	(uiWindowOnContentsSized Change (callback on-change "int" "void*" "void*") 0)
)

;; @syntax (ui:window-title <sym-id> <str-title>)
;; @syntax (ui:window-title <sym-id> )
;; @param <sym-id> the id of the window
;; @param <str-title> the title of the window
;; @return <str-title>
;; The first syntax changes the title of a window and returns it
;; the second syntax returns the current title

(define (window-title id title)
	(when title (uiWIndowSetTitle id title))
	(uiWindowTitle id)
)
;; @syntax (ui:window-size <sym-id> <int-width> <int-height>)
;; @syntax (ui:window-size <sym-id> )
;; @param <sym-id> the id of the window
;; @param <str-title> the title of the window
;; @return <list <int-width> <int-height>>
;; The first syntax changes the size of a window and returns it
;; the second syntax returns the current size

(define (window-size id width height)
	(when (and width height) (uiWindowSetContentSize id width hieght))
	(set 'w 0 'h 0)
	(uiWindowContentSize id (address w) (address h))
	(list w h)
)

;; @syntax (ui:window-fullscreen <sym-id> <int-fullscreen>)
;; @syntax (ui:window-fullscreen <sym-id> )
;; @param <sym-id> the id of the window
;; @param <int-fullscreen> 1 for fullscreen, 0 for not
;; @return <int-fullscreen>
;; The first syntax change teh fullscreen state to either fullscreen (1) or not (0)
;; the second syntax returns the fullscreen state

(define (window-fullscreen id fullscreen)
	(when fullscreen (uiWIndowSetFullscreen id fullscreen))
	(uiWindowFullscreen id)
)

;; @syntax (ui:window-borderless <sym-id> <int-borderless>)
;; @syntax (ui:window-size <sym-id> )
;; @param <sym-id> the id of the window
;; @param <str-title> the borderless state of the window
;; @return <list <int-borderless>
;; The first syntax changes the borderless state of a window and returns it
;; the second syntax returns the borderless state

(define (window-borderless id borderless)
	(when borderless (uiWindowSetBorderless id borderless))
	(uiWindowBorderless borderless)
)

;; @syntax (ui:window-margined <sym-id> <int-margined>)
;; @syntax (ui:window-size <sym-id> )
;; @param <sym-id> the id of the window
;; @param <str-title> the margined of the window
;; @return <list <int-margined>
;; The first syntax changes the margined state of a window and returns it
;; the second syntax returns the margined state

(define (window-margined id margin)
	(when margin (uiWindowSetMargined id margin))
	(uiWindowMargined id)
)

;; @syntax <ui-window-child sym-id-window> <sym-id control1> <sym-id-control2> ..
;; @param <sym-id-window> the id of the window
;; @param <sym-id-control> id (s) of the controls who have the window as parent 

(define (window-child window)
	(doargs (control)
		(uiWindowSetChild window control)
	)
)

;; @syntax (ui:show <sym-id-control>)
;; @param <sym-id-control> the id of the control
;; Sets the control visible, also childs will be visible, so typically done for windows

(define (show control)
	(uiControlShow control))

;; @syntax (ui:box <sim-id> <int-orientation> <int-padded>)
;; @param <sym-id> the name of the horizontal box
;; @param <int-orietantion> 0 creates a horizontal box, 1 a vertical box
;; @param <int-padded> 1 is padded, 0 is not padded
;; Creates a horizontal or vertical box. For horizontal boxesd widgets will be added from
;; the top, for a vertical box from the left.

(define (box id orientation)
	(if (zero? orientation)
		(set id (uiNewHorizontalBox))
	  (set id (uiNewVericalBox))
	)
)

(define (box-padded id padded)
	(when padded (uiBoxSetPadded idpadded))
	(uiBoxPadded id)
)

(define (box-append box stretchy )
	(doargs (x)
		(uiBoxAppend box x stretchy)
	)
)

(define (box-delete box)
	(doargs (x)
		(uiBoxDelete box x)
	)
)

(define (group id title)
	(set id (uiNewGroup title))
)

(define (tab id)
	(set id (uiNewTab	))
)

(define (button id text )
	(set id (uiNewButton text))
)
	
(define (button-text id text)
	(when text (uiButtonSetText id text))
	(uiButtonText id))

(define (button-clicked on-clicked)
	(doargs (btn)
		(uiButtonOnClicked btn (callback on-clicked  "void" "void*" "void*") 0)
	)
)

(define (checkbox id text handler)
	(if handler
		(begin
			(set (uiNewCheckbox text))
			(uiCheckBoxOnToggled (eval id) (callback  handler "void" "void*" "void*") 0)
		)
		(if text
			(uiCheckboxSetText id text)
			(uiCheckboxText id)
		)
	)
)

(define (checkbox-toggle toggle)
	(if toggle
		(uiCheckboxSetToggle toggle)
		(uiCheckboxToggle)
	)
)

(define (entry id text)
 (uiNewEntry text))

(define (password-entry id text)
 (uiNewPasswordEntry text))

(define (search-entry id text)
 (uiNewSearchEntry text))

(define (label id text)
 (uiNewLabel text))

(define (spinbox text)
 (uiNewSpinbox text))	

(define (show control)
	(uiControlShow control))

(define (set-parent control parent)
	(uiControlSetParent control parent))
	 
(context 'MAIN)


	
