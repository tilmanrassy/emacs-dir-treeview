emacs-dir-treeview
==================

Emacs file system navigator and simple file manager.

* [Overview](#overview)
* [Screenshots](#screenshots)
* [Installation](#installation)
* [Usage](#usage)
* [Customization](#customization)


Overview
--------

* Displays the file system as a tree; subtrees can be folded and unfolded
* A click on a filename loads the file in Emacs
* A right-click opens a context menu with further actions
* Can open files by external programs or Lisp functions
* Can open terminal in directory
* Can copy, delete, and move files
* Works in text mode, too
* Highly customizable
* Themable
* Ships with one theme called "pleasant" using Font Awesome icons


Screenshots
-----------

#### Theme 'pleasant'
![Theme 'pleasant'](screenshots/010_with_pleasant_theme.png "Theme 'pleasant'")

#### Context dialog
![Context dialog](screenshots/020_with_pleasant_theme_and_context_dialog.png "Context dialog")

#### Without theme
![Without theme](screenshots/030_without_theme.png "Without theme")

#### Text modus
![Text modus](screenshots/040_text_modus.png "Text modus")


Installation
------------

As a prerequisite, install [emacs-treeview](https://github.com/tilmanrassy/emacs-treeview).

### Installation of dir-treeview

Copy the file dir-treeview.el to somewhere in the load path. Optionally, byte-compile the
file. Add the following to your `init.el`:

```elisp
    (require 'dir-treeview)
```

Bind a key, for example F9, to the command `dir-treeview` by adding the following
to your `init.el` (somewhere after the `require` statement above):

```elisp
    (global-set-key (kbd "<f9>") 'dir-treeview)
```

### Installation of the "pleasant" theme

Make sure the "Font Awesome" icon font is installed on your computer. Copy the file `dir-treeview-pleasant-theme.el`
to somewhere in the theme loadpath (cf. Lisp variable `custom-theme-load-path`, see
[Custom Themes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html#Custom-Themes "Custom Themes - GNU Emacs Manual")
in the Emacs Manual).
Add the following to your `init.el` (somewhere after the `require` statement above):

```elisp
    (load-theme 'dir-treeview-pleasant t)
```


Usage
-----

### Start

The normal way to start dir-treeview is by the command `dir-treeview`. This is why we recommended to bind
a key to it in the "Installation" section above. It shows the tree originating at your home
directory. If you want to start dir-treeview with a different origin, use the command `dir-treeview-open`. It
will ask for the origin directory in the minibuffer:

```
    M-x dir-treeview-open RET <directory> RET
```

### Current vs. side window

By default, dir-treeview uses the current window to display the tree. However, it is also possible to display the
tree in a side window (cf. [Side Windows](https://www.gnu.org/software/emacs/manual/html_node/elisp/Side-Windows.html "Side Windows - GNU Emacs Lisp Manual") in the Emacs Lisp Manual). Thic can be controlled by the customizable variable `dir-treeview-show-in-side-window`.

### Navigating in the tree, with and without mouse

Navigating in the tree with the mouse is straight forward. A left click on the `[+]` or `[-]` symbols expand or collapse the respective directory. A left click on a filename opens the respective file in Emacs.

It is possible to navigate in the tree completely without the mouse, only with the keyboard:

* The up and down arrow keys jump to the previuos or next node, respectively. The point (cursor) is placed on the `[+]` resp. `[-]` symbol if the node is a directory, and on the filename otherwise.
* The `SPACE` key expands/collapses the respective directory if the point is on a `[+]`/`[-]` symbol.
* The `RETURN` key expands/collapses the respective directory if the point is on a `[+]`/`[-]` symbol.
* The `RETURN` key opens the respective file in Emacs if the point is on a filename.

### The context menu

A right-click on an node opens the nodes's context menu. Alternativly, the context menu can be opened with the `m` key when the point is on a node. The menu looks different depending on whether the node is a directory or not:

#### Non-directory-file:
![Context menu for non-directory files](screenshots/050_context_menu_non_directory.png "Context menu for non-directory files")

#### Directory:
![Context menu directories](screenshots/060_context_menu_directory.png "Context menu for directories")

Most of the menu items should be self-explanatory.

The `Open with...` item opens a submenu with external programs or Lisp functions to open the file.

The `Open Terminal` item opens a shell in terminal program at the corresponding directory.

Both the context menu and the `Open with...` submenu are customizable.



Customization
-------------

Dir-treeview uses [Emacs' Easy Customization](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization "Easy Customization Interface - GNU Emacs Manual") system. The customization group is "dir-treeview". Type

```
    M-x customize-group RET dir-treeview RET
```
in the minibuffer. The customization buffer for dir-treeview opens and a list off settings
is displayed. You can change each setting interactively in the buffer. For example, to change the key bindings of the `[+]`/`[-]` symbols,
open the "Dir Treeview Control Keymap" entry by clicking on the small triangle left to it. You will see the following:

![Customization buffer](screenshots/070_customization.png "Customization buffer")

You can now add or remove keys or change the Lisp commands bound the them.

