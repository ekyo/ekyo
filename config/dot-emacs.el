;;; dot-emacs.el --- Ekyo's configuration;;;

;; Copyright (c) 2012, 2013 Simon Kérouack <ekyo777@gmail.com>
;;
;; Author: Simon Kérouack <ekyo777@gmail.com>
;; URL: https://github.com/ekyo/ekyo

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration I use on a daily basis.  Use whatever you like.

;;; Code:

(eval-when-compile (require 'cl))

(defvar *emacs-load-start* (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure required packages are installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defvar ekyo-packages
  '(ack-and-a-half
    auto-complete
    buffer-move
    clojure-mode
    coffee-mode
    color-theme-monokai
    color-theme-sanityinc-tomorrow
    deft
    diminish
    expand-region
    gist
    groovy-mode
    haml-mode
    haskell-mode
    helm
    helm-projectile
    ido-ubiquitous
    inf-ruby
    magit
    magithub
    markdown-mode
    paredit
    powerline
    projectile
    python
    rainbow-mode
    sass-mode
    smart-tabs-mode
    smex
    scss-mode
    thesaurus
    volatile-highlights
    yaml-mode
    yari
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(message "%s" "Emacs Prelude is now refreshing its package database...")
(package-refresh-contents)
(message "%s" " done.")

;; install the missing packages
(dolist (p ekyo-packages)
  (when (not (require p nil t))
    (message "installing %s" p)
    (package-install p)
    )
  )

(provide 'ekyo-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Place Backup Files in Specific Directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push "/usr/bin" exec-path)
;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ;; scrolling
 scroll-margin 0                        ;; do smooth scrolling, ...
 scroll-conservatively 100000           ;; ... the defaults ...
 scroll-up-aggressively 0               ;; ... are very ...
 scroll-down-aggressively 0             ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
;; Enable everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1)
                         (ac-flyspell-workaround))
                       ))
(real-global-auto-complete-mode t)
;; delay before showing up
(setq ac-auto-show-menu 0.1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full Screen (require 'wmctrl' installed)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key (kbd "<f11>")
                'full-screen-toggle)

;; Full screen after load
(shell-command "wmctrl -r :ACTIVE: -badd,fullscreen")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename current buffer file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation Hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Re-indents the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "<f10>")
                'iwb)

;; Using smart-tabs
(setq standard-indent 2)
(setq-default tab-width 2) ; or any other preferred value
(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

(global-set-key (kbd "<f12>")
                'indent-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-c q")
                'kill-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thesaurus configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Thesaurus Key
(eval-after-load "thesaurus"
  '(progn (setq thesaurus-bhl-api-key "699761ef74acd451675d335fa614f48e")))

(global-set-key (kbd "C-x t")
                'thesaurus-choose-synonym-and-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell-script hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run a shell command in a shell buffer.
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(eval-after-load "sh-script"
  '(progn
     (define-key sh-mode-map (kbd "C-j") 'sh-send-line-or-region-and-step)
     (define-key sh-mode-map (kbd "C-c C-z") 'sh-switch-to-process-buffer)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Howdoi inside emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun howdoi ()
  (interactive
   (let ((args (read-string "howdoi: " nil 'my-history)))
     (async-shell-command (concat "howdoi " args))
     )))

(global-set-key (kbd "C-c C-h")
                'howdoi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only enable linum-mode during goto-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "C-g")
                'goto-line-with-feedback)
;(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<f5>")
                'linum-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)
(global-set-key (kbd "C-z")
                'undo-tree-visualize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
      do (add-to-list 'helm-c-boring-file-regexp-list ext))
(global-set-key (kbd "C-c C-f")
               'helm-for-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "RET")
                'newline-and-indent)

(global-set-key (kbd "C-/")
                'comment-or-uncomment-region)

(global-set-key (kbd "C-S-p")
                'smex)

(global-set-key (kbd "C-c C-s")
                'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme Adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color
(color-theme-sanityinc-tomorrow-night)
(color-theme-monokai)
;; Remove Scroll Bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;
(set-default 'truncate-lines nil)
(setq truncate-partial-width-windows nil)
(set-face-attribute 'show-paren-match-face nil :underline t)
(setq cursor-type 'bar)
(setq echo-keystrokes 0.01)
(setq frame-title-format '("%f - " user-real-login-name "@" system-name))
(setq inhibit-startup-screen t)
(setq show-paren-delay 0)
(blink-cursor-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(which-function-mode t)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-ignore-extensions t)
(setq ido-use-filename-at-point 'guess)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-enable-prefix nil)
(setq ido-max-prospects 8)
(setq ido-use-filename-at-point 'guess)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-ubiquitous t)
(smex-initialize)

(set-default-font
 "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Remove comment from scratch
(setq initial-scratch-message nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun diminish2 (mode feature &optional to-what)
;;   "MODE, FEATURE, TO-WHAT."
;;   (message "dminish2 %s" mode)
;;   (eval-after-load feature '(diminish mode to-what)))

;; (defun diminish1 (mode &optional to-what)
;;   "MODE TO-WHAT."
;;   (message "diminish1 %s" mode)
;;   (diminish2 mode (symbol-name mode) to-what))

;; (diminish2 'auto-complete-mode "auto-complete" " ☯")
;; (diminish2 'paredit-mode "paredit"             " ☂")
;; (diminish2 'projectile-mode "projectile"       " ⚑")
;; (diminish2 'undo-tree-mode "undo-tree"         " ᚠ")
;; (diminish2 'yas-minor-mode "yasnippet"         " ⌨")

;; (diminish  'eldoc-mode " ✦")
;; (diminish2 'elisp-slime-nav-mode "elisp-slime-nav")
;; (diminish1 'prelude-mode)
;; (diminish1 'rainbow-mode)
;; (diminish2 'volatile-highlights-mode "volatile-highlights")
;; (diminish2 'whitespace-mode "whitespace" " ☠")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-mail-address          "ekyo777@gmail.com")
(setq user-full-name             "Simon Kérouack")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-d-header ()
  ;; Insert a comment at the cursor position
  (interactive)
  (insert (format "/**
   Copyright: © %s %s.
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: %s
*/
" (format-time-string "%Y") user-full-name user-full-name))
  (end-of-line))

(defun insert-lodni-service-boilerplate ()
  ;; Insert boilerplate for a Lodni Service
  (interactive
   (let ((name (read-string "Name: " nil 'my-history)))
     (defvar Name (capitalize name))
     (insert-d-header)
     (insert (format "
module service.%s;
import core.service;

class %sService : Service {
  mixin ServiceMixin;

  public {
    void setup(Config conf) {
  }

  void init() {
    super.init();
  }
}

class %sServiceTest : ServiceTest!%sService {
  mixin TestMixin;
}
" name Name Name Name))
     (end-of-line))
   ))

(defun insert-lodni-plugin-boilerplate ()
  ;; Insert boilerplate for a Lodni Plugin
  (interactive
   (let ((name (read-string "Name: " nil 'my-history)))
     (defvar Name (capitalize name)
       (insert-d-header)
       (insert (format "
module plugin.%s;
import core.plugin;

class %sPlugin : Plugin {
  mixin PluginMixin;

  public {
    void setup(Config conf) {
  }

  void init() {
    super.init();
  }
}

class %sPluginTest : PluginTest!%sPlugin {
  mixin TestMixin;
}
" name Name Name Name))
       (end-of-line)))
   ))

;; Profile .emacs load time
(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*)
                           (second *emacs-load-start*)))))

(provide 'dot-emacs)
;;; dot-emacs.el ends here
