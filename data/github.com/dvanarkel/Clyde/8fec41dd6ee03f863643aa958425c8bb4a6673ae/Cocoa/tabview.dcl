definition module Cocoa.tabview

:: NSTabView				=: NSTabView Int
:: NSTabViewItem			=: NSTabViewItem Int
:: NSTabViewController		=: NSTabViewController Int


class addTabViewItem a where addTabViewItem :: !a !NSTabViewItem !*env -> *env
class tabViewItems a where tabViewItems :: !a !*env -> (!Int,!*env)
class removeTabViewItem a where removeTabViewItem :: !a !NSTabViewItem !*env -> *env
class insertTabViewItemAtIndex a where insertTabViewItemAtIndex :: !a !NSTabViewItem !Int !*env -> *env
class tabView a where tabView :: !a !*env -> (!NSTabView,!*env)

// `raw` NSTabView
instance addTabViewItem NSTabView

// `raw` NSTabViewItem
tabViewItemWithViewController :: !Int !*env -> (!NSTabViewItem,!*env)

// `raw` NSTabViewController (+/+ inherited NSViewController)
instance addTabViewItem NSTabViewController
instance tabView NSTabViewController

tabStyle :: !NSTabViewController !*env -> (!NSTabViewControllerTabStyle,!*env)
setTabStyle :: !NSTabViewController ! NSTabViewControllerTabStyle !*env -> *env

:: NSTabViewControllerTabStyle =: NSTabViewControllerTabStyle Int
NSTabViewControllerTabStyleToolbar :: NSTabViewControllerTabStyle
