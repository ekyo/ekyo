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

;;; Commentary:p

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
    d-mode
    expand-region
    flymake-coffee
    flymake-d
    flymake-haskell-multi
    flymake-json
    flymake-yaml
    ghc
    ghci-completion
    gist
    groovy-mode
    haml-mode
    haskell-mode
    helm
    helm-projectile
    ido-ubiquitous
    inf-ruby
    linum-relative
    magit
    magithub
    markdown-mode
    paredit
    projectile
    python
    rainbow-mode
    sass-mode
    scion
    scss-mode
    smart-tabs-mode
    smex
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

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; install the missing packages
(dolist (p ekyo-packages)
  (package-require p))

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

(when (require 'sublimity nil t)
  (sublimity-scroll))

(setq sublimity-scroll-weight1 1)
(setq sublimity-scroll-weight2 1)

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
;; CEDET Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cedet)

(add-to-list  'Info-directory-list "~/projects/cedet-bzr/doc/info")

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

(semantic-mode 1)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

(mapc
 (lambda (MODE)
   (add-hook MODE
             (lambda ()
               (when (boundp 'semantic-ia-complete-symbol)
                 (add-to-list 'completion-at-point-functions 'semantic-ia-complete-symbol))
               (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
               (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
               (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
               (local-set-key "\C-c=" 'semantic-decoration-include-visit)
               (local-set-key "\C-cj" 'semantic-ia-fast-jump)
               (local-set-key "\C-cq" 'semantic-ia-show-doc)
               (local-set-key "\C-cs" 'semantic-ia-show-summary)
               (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
               ))) '(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook))

(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key "\C-ct" 'eassist-switch-h-cpp)
                                (local-set-key "\C-xt" 'eassist-switch-h-cpp)
                                (local-set-key "\C-ce" 'eassist-list-methods)
                                (local-set-key "\C-c\C-r" 'semantic-symref)))

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

(defun qt-cedet-setup ()
  "Set up c-mode and related modes. Includes support for Qt code (signal, slots and alikes)."

  ;; add knowledge of qt to emacs
  (setq qt4-base-dir (concat (getenv "QTDIR") "/include"))
  (semantic-add-system-include (concat qt4-base-dir "/Qt") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtGui") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtCore") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtTest") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtNetwork") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtSvg") 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))

  ;; modify the colour of slots to match public, private, etc ...
  (font-lock-add-keywords 'c++-mode '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
  ;; make new font for rest of qt keywords
  (make-face 'qt-keywords-face)
  (set-face-foreground 'qt-keywords-face "BlueViolet")
  ;; qt keywords
  (font-lock-add-keywords 'c++-mode '(("\\<Q_[A-Z]*\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<Q[A-Z][A-Za-z]*\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<Q[A-Z_]+\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<q\\(Debug\\|Wait\\|Printable\\|Max\\|Min\\|Bound\\)\\>" . 'font-lock-builtin-face)))

  (setq c-macro-names-with-semicolon '("Q_OBJECT" "Q_PROPERTY" "Q_DECLARE" "Q_ENUMS"))
  (c-make-macro-with-semi-re)
  )
(when (getenv "QTDIR") (add-hook 'c-mode-common-hook 'qt-cedet-setup))

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

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-faces
 '(my-tab-face            ((((class color)) (:background "#7cfc00"))) t)
 '(my-trailing-space-face ((((class color)) (:background "#ff1493"))) t)
 '(my-long-line-face ((((class color)) (:background "#b22222"))) t))
(add-hook 'font-lock-mode-hook
          (function
           (lambda ()
             (setq font-lock-keywords
                   (append font-lock-keywords
                           '(("\t+" (0 'my-tab-face t))
                             ("^.\\{120,\\}$" (0 'my-long-line-face t))
                             ("[ \t]+$"      (0 'my-trailing-space-face t))))))))
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
;; tmux control from emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tmux-session-name 0)
(setq tmux-window-name 1)
(setq tmux-pane-number 1)

(defun tmux-setup (x y z)
  "Setup global variables for tmux session, window, and pane"
  (interactive "sEnter tmux session name: \nsEnter tmux window name: \nsEnter tmux pane number: ")
  (setq tmux-session-name x)
  (setq tmux-window-name y)
  (setq tmux-pane-number z)
  (message "Tmux Setup, session name: %s, window name: %s, pane number: %s"
           tmux-session-name tmux-window-name tmux-pane-number))

(defun tmux-exec (command)
  "Execute command in tmux pane"
  (interactive)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s '%s' Enter" tmux-session-name tmux-window-name tmux-pane-number command)))



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
;; ERC configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only enable linum-mode during goto-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
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

(global-set-key (kbd "<f6>")
                'linum-relative-toggle)


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
;; Haskell Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

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
;;(color-theme-monokai)
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

(flyspell-mode 0)
(flyspell-prog-mode)

(set-default-font
 "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Remove comment from scratch
(setq initial-scratch-message nil)

;; Set font size
(set-face-attribute 'default nil :height 90)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mode-line-cleaner-alist
  '((auto-complete-mode . " ☯")
    (paredit-mode . " ☂")
    (projectile-mode . " ⚑")
    (undo-tree-mode . "")
    (yas-minor-mode . " υ")
    (emacs-lisp-mode . " EL")
    (eldoc-mode . "")
    (elisp-slime-nav-mode . "")
    (prelude-mode . "")
    (rainbow-mode . "")
    (volatile-highlights-mode . "")
    (whitespace-mode . " ☠")
    (coffee-mode . "☕")))

;; Nice characters to use: (C-x 8 C-h to see all characters)
;; ❄☃Φ
;; Some japanese: のぬるぬるスクロール＆ミニマッ

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(require 'flymake-json)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)

(require 'd-mode)
(add-hook 'd-mode-hook 'flymake-d-load)

(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

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

   License: Subject to the terms of the MIT license,
   as written in the included LICENSE.txt file.

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
    override void setup(Config config) {
    }

    override void init() {
      super.init();
    }
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
     (defvar Name (capitalize name))
     (insert-d-header)
     (insert (format "
module plugin.%s;
import core.plugin;

class %sPlugin : Plugin {
  mixin PluginMixin;

  public {
    override void setup(Config config) {
    }

    override void init() {
      super.init();
    }
  }
}

class %sPluginTest : PluginTest!%sPlugin {
  mixin TestMixin;
}
" name Name Name Name))
     (end-of-line))
   ))

(provide 'dot-emacs)
;;; dot-emacs.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pool of colors to use when coloring IRC nicks.
(setq erc-colors-list '("green" "blue" "red"
                        "dark gray" "dark orange"
                        "dark magenta" "maroon"
                        "indian red" "black" "forest green"
                        "midnight blue" "dark violet"))
;; special colors for some people
(setq erc-nick-color-alist '(("ekyo" . "blue")
                             ("lambdabot" . "red")
                             ))

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is in erc-nick-color-alist,
   use that color, else hash the nick and use a random color from the pool"
  (or (cdr (assoc nick erc-nick-color-alist))
      (nth
       (mod (string-to-number
             (substring (md5 (downcase nick)) 0 6) 16)
            (length erc-colors-list))
       erc-colors-list)))

(defun erc-put-color-on-nick ()
  "Modifies the color of nicks according to erc-get-color-for-nick"
  (save-excursion
    (goto-char (point-min))
    (while (forward-word 1)
      (setq bounds (bounds-of-thing-at-point 'word))
      (setq word (buffer-substring-no-properties
                  (car bounds) (cdr bounds)))
      (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                (and erc-channel-users (erc-get-channel-user word)))
        (put-text-property (car bounds) (cdr bounds)
                           'face (cons 'foreground-color
                                       (erc-get-color-for-nick word)))))))

(add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
