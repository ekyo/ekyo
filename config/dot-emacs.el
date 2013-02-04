;; Install required packages
(defvar prelude-packages
  '(ack-and-a-half auctex auto-complete
                   clojure-mode coffee-mode
                   deft
                   expand-region
                   gist groovy-mode
                   haml-mode haskell-mode
                   ido-ubiquitous inf-ruby
                   magit magithub markdown-mode
                   paredit projectile python
                   rainbow-mode
                   sass-mode smart-tabs-mode smex scss-mode
                   solarized-theme
                   thesaurus
                   volatile-highlights
                   yaml-mode yari yasnippet
                   zenburn-theme
                   )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)

(setq standard-indent 2)

;; ========== Place Backup Files in Specific Directory ==========
(push "/usr/bin" exec-path)
;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

(setq ;; scrolling
 scroll-margin 0                        ;; do smooth scrolling, ...
 scroll-conservatively 100000           ;; ... the defaults ...
 scroll-up-aggressively 0               ;; ... are very ...
 scroll-down-aggressively 0             ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Auto-complete
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1)
                         (ac-flyspell-workaround))
                       ))
(require 'auto-complete)
(real-global-auto-complete-mode t)
(setq ac-auto-show-menu 0.1)

(defun full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

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

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; Set Thesaurus Key
(eval-after-load "thesaurus"
  '(progn (setq thesaurus-bhl-api-key "699761ef74acd451675d335fa614f48e")))

;; Use C-j to run a shell command in a shell buffer.
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



(global-set-key (kbd "C-z")
                'undo)

(global-set-key (kbd "C-x t")
                'thesaurus-choose-synonym-and-replace)

(global-set-key (kbd "C-g")
                'goto-line)

(global-set-key (kbd "<f11>")
                'full-screen-toggle)

(global-set-key (kbd "<f10>")
                'iwb)

(global-set-key (kbd "<f5>")
                'linum-mode)

(global-set-key (kbd "RET")
                'newline-and-indent)

(global-set-key (kbd "C-/")
                'comment-or-uncomment-region)

(global-set-key (kbd "C-c q")
                'kill-current-buffer)

(global-set-key (kbd "<f12>")
                'indent-buffer)

(global-set-key (kbd "C-S-p")
                'smex)

(global-set-key (kbd "C-c C-s")
                'shell)

;; Remove Scroll Bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Auto format
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

;; Theme Adjustments
(set-default 'truncate-lines t)
(set-face-attribute 'show-paren-match-face nil :underline t)
(setq cursor-type 'bar)
(setq echo-keystrokes 0.01)
(setq frame-title-format '("%f - " user-real-login-name "@" system-name))
(setq inhibit-startup-screen t)
(setq show-paren-delay 0)
(setq truncate-partial-width-windows nil)
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
