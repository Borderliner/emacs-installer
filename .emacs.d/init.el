;;; init.el --- A Clean and Modern Emacs Configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0. Startup performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Raise GC threshold during init; restore to a sane default after startup.
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Core Emacs Settings & UI Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'seq) ;; we use seq-* helpers later

;; --- Basic UI ---
(setq-default
 inhibit-startup-message t      ; Disable startup screen
 ring-bell-function 'ignore     ; Silence bell
 indent-tabs-mode nil)          ; Use spaces

(set-default-coding-systems 'utf-8)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(electric-pair-mode 1)
(pixel-scroll-precision-mode 1)

;; --- Font Setup (from Nano Emacs) ---
(set-face-attribute 'default nil :height 110 :family "Caskaydia Cove Nerd Font")
(setq-default line-spacing 0.25) ; 25% more of regular
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)

;; Emoji fallback (robust across platforms/Emacs builds)
(let ((emoji-font
       (catch 'found
         (dolist (name '("Apple Color Emoji" "Noto Color Emoji" "Segoe UI Emoji" "Twitter Color Emoji" "JoyPixels"))
           (when (member name (font-family-list))
             (throw 'found name)))
         nil)))
  (when emoji-font
    ;; Try 'emoji first (Emacs 29+), then fall back to 'symbol and 'unicode.
    (dolist (charset '(emoji symbol unicode))
      (ignore-errors
        (set-fontset-font t charset (font-spec :family emoji-font) nil 'prepend)))))

;; --- Frame Setup (from Nano Emacs) ---
(setq default-frame-alist
      '((height . 40) (width . 120) (left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)))

;; --- File Backups & Auto-Saves (centralized; no foo~ beside files) ---
(setq make-backup-files t
      backup-by-copying t
      backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t))
      auto-save-list-file-prefix (concat user-emacs-directory "auto-saves/sessions_")
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil) ; hide .# lockfiles

;; Create the directories if they don't exist
(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)

;; Ensure TRAMP uses centralized backups too
(with-eval-after-load 'tramp
  (setq tramp-backup-directory-alist backup-directory-alist))

;; --- Clipboard Integration (GUI + TTY; Wayland/X11) ---
(setq select-enable-clipboard t)
;; Ensure UTF-8 selections everywhere (helps with odd terminals)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; Wayland clipboard shims so interprogram-* can call them
(unless (fboundp 'wl-copy)
  (defun wl-copy (text &optional _push)
    (when (executable-find "wl-copy")
      (let ((process-connection-type nil))
        (let ((proc (start-process "wl-copy" nil "wl-copy" "--foreground" "--type" "text/plain")))
          (process-send-string proc text)
          (process-send-eof proc))))))

(unless (fboundp 'wl-paste)
  (defun wl-paste ()
    (when (executable-find "wl-paste")
      (let ((s (shell-command-to-string "wl-paste -n")))
        (if (string-match-p "\\`[ \t\r\n]*\\'" s) nil s)))))

(unless (display-graphic-p)
  ;; Prefer Wayland if available
  (when (and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy"))
    (setq interprogram-cut-function 'wl-copy
          interprogram-paste-function 'wl-paste))
  ;; Only fall back to xclip (X11) if nothing set above (e.g., XWayland present)
  (when (and (null interprogram-cut-function)
             (getenv "DISPLAY") (executable-find "xclip"))
    (setq interprogram-cut-function
          (lambda (text &optional _push)
            (with-temp-buffer
              (insert text)
              (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard"))))
    (setq interprogram-paste-function
          (lambda () (shell-command-to-string "xclip -o -selection clipboard")))))

;; --- History and Recent Files ---
(use-package savehist :ensure nil :init (savehist-mode 1))
(use-package recentf  :ensure nil
  :init (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") #'recentf-open-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Theme (Nano faces + Spolsky theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface nano-default '((t)) "") (defface nano-default-i '((t)) "")
(defface nano-highlight '((t)) "") (defface nano-highlight-i '((t)) "")
(defface nano-subtle '((t)) "") (defface nano-subtle-i '((t)) "")
(defface nano-faded '((t)) "") (defface nano-faded-i '((t)) "")
(defface nano-salient '((t)) "") (defface nano-salient-i '((t)) "")
(defface nano-popout '((t)) "") (defface nano-popout-i '((t)) "")
(defface nano-strong '((t)) "") (defface nano-strong-i '((t)) "")
(defface nano-critical '((t)) "") (defface nano-critical-i '((t)) "")

(defun nano-set-face (name &optional fg bg weight)
  (apply #'set-face-attribute `(,name nil
                                      ,@(when fg `(:foreground ,fg))
                                      ,@(when bg `(:background ,bg))
                                      ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute
         `(,(intern (concat (symbol-name name) "-i")) nil
           :foreground ,(face-background 'nano-default)
           ,@(when fg `(:background ,fg))
           :weight regular)))

(defun nano-link-face (sources faces &optional attributes)
  (let ((attrs (or attributes '(:foreground :background :family :weight :height :slant :overline :underline :box))))
    (dolist (face (seq-filter #'facep faces))
      (dolist (a attrs) (set-face-attribute face nil a 'unspecified))
      (set-face-attribute face nil :inherit sources))))

(defun nano-install-theme ()
  (set-face-attribute 'default nil :foreground (face-foreground 'nano-default)
                      :background (face-background 'nano-default))
  (dolist (item '((nano-default . (variable-pitch variable-pitch-text fixed-pitch fixed-pitch-serif))
                  (nano-highlight . (hl-line highlight))
                  (nano-subtle . (match region lazy-highlight widget-field))
                  (nano-faded . (shadow font-lock-comment-face font-lock-doc-face))
                  (nano-popout . (warning font-lock-string-face))
                  (nano-salient . (success link help-argument-name custom-visibility font-lock-type-face font-lock-keyword-face font-lock-builtin-face))
                  (nano-strong . (font-lock-function-name-face font-lock-variable-name-face minibuffer-prompt))
                  (nano-critical . (error))
                  (nano-faded-i . (help-key-binding))
                  (nano-default-i . (custom-button-mouse isearch))
                  (nano-critical-i . (isearch-fail))
                  ((nano-subtle nano-strong) . (custom-button))
                  ((nano-faded-i nano-strong) . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))
  (set-face-attribute 'header-line nil :background 'unspecified :underline nil
                      :box `(:line-width 1 :color ,(face-background 'nano-default)) :inherit 'nano-subtle)
  (set-face-attribute 'mode-line nil :background (face-background 'default)
                      :underline (face-foreground 'nano-faded) :height 40 :overline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :background (face-background 'default)
                      :underline (face-foreground 'nano-faded) :height 40 :overline nil :box nil))

(defun nano-dark (&rest _args)
  (interactive)
  (nano-set-face 'nano-default "#ECEFF4" "#2E3440")
  (nano-set-face 'nano-strong "#ECEFF4" nil 'regular)
  (nano-set-face 'nano-highlight nil "#3B4252")
  (nano-set-face 'nano-subtle nil "#434C5E")
  (nano-set-face 'nano-faded "#677691")
  (nano-set-face 'nano-salient "#81A1C1")
  (nano-set-face 'nano-popout "#D08770")
  (nano-set-face 'nano-critical "#EBCB8B")
  (nano-install-theme))

;; Load Spolsky from sublime-themes by default
(use-package sublime-themes :init (load-theme 'spolsky t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Minibuffer Completion: Vertico Stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico :init (vertico-mode) (setq resize-mini-windows 'grow-only))
(use-package marginalia :after vertico :init (marginalia-mode))
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Essential Packages & QoL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key :init (which-key-mode) :config (setq which-key-idle-delay 0.3))
(use-package projectile :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(use-package exec-path-from-shell :if (memq system-type '(darwin gnu/linux))
  :init (exec-path-from-shell-initialize))
(use-package yasnippet :init (yas-global-mode 1))

;; Nerd icons + modeline with icons
(use-package nerd-icons :demand t)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-height 24)
  (doom-modeline-minor-modes t)
  (doom-modeline-lsp t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.5 Org-mode: Authoring & Notes (non-intrusive; safe bindings & visuals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use built-in Org (avoid version shadowing by straight).
(use-package org
  :ensure nil
  :straight (:type built-in)   ;; ← important when straight integrates with use-package
  :commands (org-mode org-agenda org-capture)
  :init
  ;; Files & directories
  (defvar my/org-directory (expand-file-name "org/" user-emacs-directory)
    "Root directory for Org files.")
  (defvar my/org-inbox-file (expand-file-name "inbox.org" my/org-directory)
    "Default capture inbox.")
  (defvar my/org-journal-file (expand-file-name "journal.org" my/org-directory)
    "Simple journal file (one big file).")

  ;; Ensure base dir/files exist
  (dolist (f (list my/org-directory my/org-inbox-file my/org-journal-file))
    (unless (file-exists-p f)
      (if (and (stringp f) (string-suffix-p "/" f))
          (make-directory f t)
        (progn (make-directory (file-name-directory f) t)
               (with-temp-file f (insert "#+title: " (file-name-base f) "\n\n"))))))

  ;; Lightweight defaults
  (setq org-directory my/org-directory
        org-default-notes-file my/org-inbox-file
        org-startup-indented t
        org-startup-folded 'content
        org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-ellipsis " …"
        org-return-follows-link t
        org-image-actual-width nil
        org-pretty-entities t
        org-log-done 'time
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  ;; Compute agenda files once
  (setq org-agenda-files
        (let ((root my/org-directory))
          (seq-filter (lambda (p) (string-match-p "\\.org\\'" p))
                      (directory-files-recursively root "\\.org\\'"))))

  :config
  ;; Exporters & tempo
  (require 'ox-md)
  (require 'org-tempo)

  ;; Editing defaults
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)

  ;; Keep code/table faces fixed-pitch
  (dolist (face '(org-code org-block org-table org-verbatim org-formula))
    (set-face-attribute face nil :inherit 'fixed-pitch))

  ;; Standard Org keys (avoid your LSP C-c l)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; Capture templates
  (setq org-capture-templates
        `(("t" "Todo" entry (file ,my/org-inbox-file)
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
          ("n" "Note" entry (file ,my/org-inbox-file)
           "* %^{Title}\n%U\n%?\n")
          ("j" "Journal" entry (file+olp+datetree ,my/org-journal-file)
           "* %U %?\n"))))

;; Visual niceties (all from MELPA; safe)
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom (org-appear-autolinks t))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-variable-pitch nil)
  (org-modern-hide-stars 'leading)
  (org-modern-star '("●" "◉" "○" "•" "∙" "·"))
  (org-modern-todo nil)
  (org-modern-ellipsis " …"))

(use-package org-download
  :after org
  :init
  (setq org-download-method 'directory
        org-download-image-dir (expand-file-name "images" my/org-directory)
        org-download-heading-lvl nil)
  :hook ((dired-mode . org-download-enable)))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-mode))

;; Babel language enablement — load Org's bundled libs directly (no packages).
(with-eval-after-load 'org
  (require 'ob-shell)   ;; ← replaced (use-package ob-shell) to avoid straight installing
  (require 'ob-python)  ;; ← likewise
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t)
     ;; (rust . t) ; requires ob-rust from MELPA if you later want it
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Programming - Rust Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Improve LSP & GC performance
(setq read-process-output-max (* 3 1024 1024))

;; Company (global)
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("TAB"   . company-complete-selection)))

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((rustic-mode . lsp-deferred)
         ;; Bonus fix: buffer-local format-on-save when LSP is active
         (lsp-mode . (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
         (lsp-mode . lsp-enable-which-key-integration))
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map ("C-." . lsp-execute-code-action))
  :custom
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.6)
  (lsp-eldoc-render-all t)
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-run-build-scripts t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-code-actions t))

;; Ensure dependencies before rustic compiles
(with-eval-after-load 'rustic
  (require 'dash)
  (require 'lsp-mode nil t)
  (require 'eglot nil t)
  (require 'project))

;; Rustic
(use-package rustic
  :after (lsp-mode dash project)
  :custom (rustic-format-on-save nil) ; LSP handles formatting
  :config (setq-default company-backends '(company-capf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. OS-Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        ns-function-modifier 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Dashboard (GUI: logo.png, TTY: logo.txt; robust selection)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Paths for your banners
(defvar my/dashboard-logo-file
  (expand-file-name "logo.png" user-emacs-directory)
  "PNG shown in GUI.")

(defvar my/dashboard-ascii-file
  (expand-file-name "logo.txt" user-emacs-directory)
  "Text banner shown in TTY.")

;; Your ASCII banner (written to logo.txt if it doesn't exist)
(defvar my/dashboard-ascii-lines
  '("░▀▀█░█▀▀░█▀█░░░█▀▀░█▄█░█▀█░█▀▀░█▀▀"
    "░▄▀░░█▀▀░█░█░░░█▀▀░█░█░█▀█░█░░░▀▀█"
    "░▀▀▀░▀▀▀░▀░▀░░░▀▀▀░▀░▀░▀░▀░▀▀▀░▀▀▀"
    )
  "Lines used to seed `logo.txt` once if missing.")

;; Make the TTY banner mellow pink by directly tinting the first N lines
(defun my/dashboard-pinkify-banner ()
  "Tint the ASCII banner (from logo.txt) mellow pink in TTY."
  (let ((buf (get-buffer "*dashboard*")))
    (when (and buf (not (display-graphic-p)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (lines (length my/dashboard-ascii-lines))
              ;; xterm-256 mellow pink; ANSI fallback
              (pink (if (>= (display-color-cells (selected-frame)) 256)
                        "color-217" "magenta")))
          (save-excursion
            (goto-char (point-min))
            (dotimes (_ lines)
              (let ((beg (line-beginning-position))
                    (end (line-end-position)))
                (add-text-properties beg end `(face (:foreground ,pink :weight bold))))
              (forward-line 1))))))))

(defun my/ensure-ascii-banner-file ()
  "Create `logo.txt` with `my/dashboard-ascii-lines` if it doesn't exist."
  (unless (file-exists-p my/dashboard-ascii-file)
    (with-temp-file my/dashboard-ascii-file
      (dolist (l my/dashboard-ascii-lines)
        (insert l "\n")))))

;; Quotes MUST be defined before dashboard reads them
(defvar my/zen-quotes
  '("Sitting quietly, doing nothing, Spring comes, and the grass grows by itself."
    "Before I had studied Zen for thirty years, I saw mountains as mountains, and waters as waters."
    "All sentient beings are essentially Buddhas."
    "When you realize nothing is lacking, the whole world belongs to you. - Lao Tzu"
    "The feeling that any task is a nuisance will soon disappear if it is done in mindfulness. - Thich Nhat Hanh"
    "Each morning, we are born again. What we do today is what matters most. - Buddha"
    "Let go, or be dragged. - Zen Proverb"
    "To be beautiful means to be yourself. You don't need to be accepted by others. You need to accept yourself. - Thich Nhat Hanh"
    "If you are depressed, you are living in the past. If you are anxious, you are living in the future. If you are at peace, you are living in the present. - Lao Tzu"
    "Do not dwell in the past, do not dream of the future, concentrate the mind on the present moment. - Buddha"
    "The present moment is filled with joy and happiness. If you are attentive, you will see it. - Thich Nhat Hanh"
    "What you are looking for is already in you. You already are everything you are seeking. - Thich Nhat Hanh"
    "Throughout this life, you can never be certain of living long enough to take another breath. - Huang Po"
    "When thoughts arise, then do all things arise. When thoughts vanish, then do all things vanish. - Huang Po"
    "The resistance to the unpleasant situation is the root of suffering. - Ram Dass"
    "Self-realization is effortless. What you are trying to find is what you already are. - Ramesh Balsekar"
    "Things are as they are. Looking out into the universe at night, we make no comparisons between right and wrong stars. - Alan Watts"
    "Wisdom says we are nothing. Love says we are everything. Between these two our life flows. - Jack Kornfield"
    "If you miss the present moment, you miss your appointment with life. That is very serious! - Thich Nhat Hanh"
    "The real meditation is how you live your life. - Jon Kabat-Zinn"))

(defun my/dashboard-compute-banner ()
  "Return a value suitable for `dashboard-startup-banner`:
PNG/TXT cons in GUI when PNG exists; TXT path otherwise."
  (my/ensure-ascii-banner-file)
  (let ((png my/dashboard-logo-file)
        (txt my/dashboard-ascii-file))
    (cond
     ;; GUI + PNG available → cons (image . text)
     ((and (display-graphic-p)
           (file-exists-p png)
           (image-type-available-p 'png))
      (cons png txt))
     ;; TTY or missing PNG → just the text path
     ((file-exists-p txt) txt)
     ;; Fallback if somehow neither exists
     (t 'official))))

(use-package dashboard
  :commands (dashboard-open)
  :init
  (setq dashboard-banner-logo-title "🌿 Welcome to Zen Emacs"
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents . 5) (bookmarks . 5) (projects . 5))
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator nil
        dashboard-page-separator "\n\n"
        dashboard-footer-messages my/zen-quotes
        dashboard-startup-banner (my/dashboard-compute-banner)
        dashboard-image-banner-max-width  320
        dashboard-image-banner-max-height 320))

(defun my/dashboard-content-present-p ()
  "Return non-nil if any meaningful content buffers exist (files or Dired)."
  (seq-some
   (lambda (b)
     (or (buffer-file-name b)
         (with-current-buffer b (derived-mode-p 'dired-mode))))
   (buffer-list)))

(defun my/dashboard-open ()
  "Open Dashboard; banner selection precomputed above."
  (require 'dashboard)
  (dashboard-open)
  ;; TTY: paint the banner mellow pink
  (my/dashboard-pinkify-banner))

;; Only show dashboard when no files were passed at startup.
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (or (daemonp) (my/dashboard-content-present-p))
              (my/dashboard-open))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Finalization & Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "✅ Emacs configuration loaded successfully!")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 '(custom-safe-themes
   '("c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11"
     "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 )
;;; init.el ends here
