emacs-dir-treeview
==================

Emacs tree navigator for the file system and simple file manager.


Overview
--------

* Displays the file system as a tree, with branches that can be folded and unfolded
* A click on a filename loads the file in Emacs
* A right-click opnes a context menu with further actions
* Can open files by external programs or Lisp functions
* Can open terminal in directory
* Can copy, delete, and move files
* Works in text mode, too
* Highly customizable
* Themable
* Ships with one theme called "pleasant" using Font Awesome icons


Installation
------------

As a prerequisite, install the "treeview" library.

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
o somewhere in the theme loadpath (cf. Lisp variable `custom-theme-load-path`, see
[Custom Themes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html#Custom-Themes "Custom Themes - GNU Emacs Manual")
in the Emacs Manual).
Add the following to your `init.el` (somewhere after the `require` statement above):

```elisp
    (load-theme 'dir-treeview-pleasant t)
```


Usage
-----

### Start

The easiest way to start dir-treeview is by the key you bound to the command `dir-treeview`
during installation (see section "Installation" above). It shows the tree originating at your home
directory. If you want to start dir-treeview with a different origin, use the command `dir-treeview-open`,
which will ask for the origin directory in the minibuffer:

```
    M-x dir-treeview-open RET <directory> RET
```

### Current vs. side window

By default, dir-treeview uses the current window to display the tree. However, it is also possible to display the
tree in a side window (cf. [Side Windows](https://www.gnu.org/software/emacs/manual/html_node/elisp/Side-Windows.html "Side Windows - GNU Emacs Lisp Manual") in the Emacs Lisp Manual). Thic can be controlled by the customizable variable `dir-treeview-show-in-side-window`.

### Navigating in the tree, with and without mouse

Navigating in the tree is straight forward. A left click on the `[+]` or `[-]` symbols expand or collapse the respective directory, respectively. A left click on the filename opens the respective file in Emacs.

T.B.C.
