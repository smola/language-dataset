use gtk
import gtk/[Gtk, Widget, Bin]

MenuShell: cover from GtkMenuShell* extends Widget {
    
    append: extern(gtk_menu_shell_append) func (menuItem: MenuItem)
    
}

/**
 * A menu widget
 */
Menu: cover from GtkMenu* extends MenuShell {

	/**
	 * Creates a new GtkMenu.
	 */
	new: static extern(gtk_menu_new) func -> This

    popup: extern(gtk_menu_popup) func (parentMenuShell, parentMenuItem: Widget,
        menuPosFunc: Pointer, data: Pointer, button: UInt, activate_time: UInt32)

}

MenuBar: cover from GtkMenuBar* extends MenuShell {
    
    /**
     * Create a new empty menu bar
     */
    new: static extern(gtk_menu_bar_new) func -> This
    
}

MenuItem: cover from GtkMenuItem* extends Bin {

    new: static extern(gtk_menu_item_new) func ~empty -> This

    /**
     * Creates a new menu item with a given label
     */
    new: static extern(gtk_menu_item_new_with_label) func ~withLabel (label: CString) -> This
    
    setSubMenu: extern(gtk_menu_item_set_submenu) func (Menu)
    
}
