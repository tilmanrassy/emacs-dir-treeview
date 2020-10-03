;;; dir-treeview-pleasant-theme.el --- A plain dir-treeview theme with fontawesome icons -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Tilman Rassy <tilman.rassy@googlemail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A theme for the "dir-treeview" package with a clean a simple look.  Uses icons
;; from the "Font Awesome" icon font (see https://fontawesome.com).  This font
;; must be installed on your computer, and must be accessible under the font
;; familiy name "Font Awesome".
;;
;; The theme is suitable for graphical displays only.  Thus, on text displays all
;; settings of the theme are ignored, and the theme has no effect.

;;; Code:

(deftheme dir-treeview-pleasant
  "A plain dir-treeview theme with fontawesome icons.")
  
(custom-theme-set-variables
 'dir-treeview-pleasant
 '(dir-treeview-icon-margin-right  " ")
 '(dir-treeview-default-icon       "F15B")     ;; file
 '(dir-treeview-folded-dir-icon    "F07B")     ;; folder
 '(dir-treeview-expanded-dir-icon  "F07C")     ;; folder-open
 '(dir-treeview-special-icons
   '((dir-treeview-is-image-p .    "F1C5")     ;; file-image
     (dir-treeview-is-audio-p .    "F1C7")     ;; file-audio
     (dir-treeview-is-video-p .    "F1C8")     ;; file-video
     (dir-treeview-is-archive-p .  "F1C6")     ;; file-archive
     ("\\.pdf$"            .       "F1C1")     ;; file-pdf
     (file-executable-p    .       "F013"))))  ;; cog

(custom-theme-set-faces
 'dir-treeview-pleasant
 '(dir-treeview-control-face         ((((type graphic)) . (:foreground "gray40"))))
 '(dir-treeview-indent-face          ((((type graphic)) . (:foreground "gray40"))))
 '(dir-treeview-default-icon-face    ((((type graphic)) . (:family "Font Awesome" :weight bold :foreground "light steel blue"))))
 '(dir-treeview-directory-icon-face  ((((type graphic)) . (:inherit dir-treeview-default-icon-face :foreground "cornflower blue"))))
 '(dir-treeview-directory-face       ((((type graphic)) . (:inherit dir-treeview-default-filename-face))))
 '(dir-treeview-symlink-face         ((((type graphic)) . (:inherit dir-treeview-default-filename-face :slant italic))))
 '(dir-treeview-executable-icon-face ((((type graphic)) . (:inherit dir-treeview-default-icon-face :foreground "rosy brown"))))
 '(dir-treeview-executable-face      ((((type graphic)) . (:inherit dir-treeview-default-filename-face))))
 '(dir-treeview-archive-icon-face    ((((type graphic)) . (:inherit dir-treeview-default-icon-face :foreground "cadet blue"))))
 '(dir-treeview-archive-face         ((((type graphic)) . (:inherit dir-treeview-default-filename-face))))
 '(dir-treeview-image-icon-face      ((((type graphic)) . (:inherit dir-treeview-default-icon-face :foreground "plum"))))
 '(dir-treeview-image-face           ((((type graphic)) . (:inherit dir-treeview-default-filename-face))))
 '(dir-treeview-audio-icon-face      ((((type graphic)) . (:inherit dir-treeview-default-icon-face :foreground "dark sea green"))))
 '(dir-treeview-audio-face           ((((type graphic)) . (:inherit dir-treeview-default-filename-face))))
 '(dir-treeview-video-face           ((((type graphic)) . (:inherit dir-treeview-default-filename-face)))))
  
;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dir-treeview-pleasant)
(provide 'dir-treeview-pleasant-theme)

;;; dir-treeview-pleasant-theme.el ends here
