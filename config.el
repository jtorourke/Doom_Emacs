;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "jtorourke"
      user-mail-address "johnt@orourke.one")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14 :weight 'semibold))

;; change splash
(setq fancy-splash-image "/home/john/wallpapers/gruvbox-wallpapers/wallpapers/doomEmacsGruvbox.svg")

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(when (featurep 'native-compile)
  (setq native-comp-jit-compilation-deny-list nil)) ; Reset if needed

(setq org-latex-minted-options '(("frame" "leftline")))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted" nil))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(require 'workgroups2)

;; Ensure vterm windows are dedicated to prevent reuse
(after! vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set-window-dedicated-p (get-buffer-window) t))))

;; Global keybinding to close the output window
(defun kill-async-buffer-and-window()
  (interactive)
  (kill-buffer-and-window))

(map! "<f10>" #'kill-async-buffer-and-window)

;; Modified Python execution with focus
(defun run-python-script ()
  (interactive)
  (save-buffer)
  (async-shell-command
   (format "python3 %s" (shell-quote-argument (buffer-file-name)))))

(after! python
  (map! :map python-mode-map
        :n "<f9>" #'run-python-script
        :i "<f9>" #'run-python-script))

;; Modified Julia execution with focus
(defun run-julia-script ()
  (interactive)
  (save-buffer)
  (async-shell-command
   (format "julia %s" (shell-quote-argument (buffer-file-name)))))

(after! julia-mode
  (map! :map julia-mode-map
        :n "<f9>" #'run-julia-script
        :i "<f9>" #'run-julia-script))

;; Gleam execution with focus
(defun run-gleam-script ()
  (interactive)
  (save-buffer)
  ;; Determine if we're in a Gleam project
  (let* ((default-directory (or (locate-dominating-file default-directory "gleam.toml")
                               default-directory))
         (command (if (string-match-p "\\.gleam$" (buffer-file-name))
                      (format "gleam run -m %s"
                              (file-name-base (buffer-file-name)))
                    "gleam run")))
    (async-shell-command command)
    (when-let ((buf (get-buffer "*Async Shell Command*")))
      (select-window (get-buffer-window buf)))))

;; Gleam test runner
(defun run-gleam-test ()
  (interactive)
  (save-buffer)
  (let* ((default-directory (or (locate-dominating-file default-directory "gleam.toml")
                               default-directory))
         (command "gleam test"))
    (async-shell-command command)
    (when-let ((buf (get-buffer "*Async Shell Command*")))
      (select-window (get-buffer-window buf)))))

;; Hook for gleam-mode (install gleam-mode via doom if not already)
(after! gleam-ts-mode
  (map! :map gleam-ts-mode-map
        :n "<f9>" #'run-gleam-script
        :i "<f9>" #'run-gleam-script
        :n "<f8>" #'run-gleam-test
        :i "<f8>" #'run-gleam-test))

(after! ein
  ;; Set a valid default notebook directory
  (setq ein:jupyter-server-default-notebook-directory "~/learning/"))

;; Set Jupyter path and kernel directories
(setq ein:jupyter-server-command "jupyter-lab"
      ein:kernel-spec-directories (list "~/.local/share/jupyter/kernels"
                                      "/nix/var/nix/profiles/per-user/$USER/profile/share/jupyter/kernels"))

;; Enable tramp for sudo access
(require 'tramp)

(after! tramp
  ;; Define safer sudo-dired function with async support
  (defun sudired ()
    (interactive)
    (require 'tramp)
    (let ((default-directory "/sudo::/"))
      (call-interactively #'dired)))

  ;; Bind to F6 with built-in doom keybinding syntax
  (map! :nvi "<f6>" #'sudired))

(after! org-roam
  ;; Set journal directory
  (setq org-roam-dailies-directory "journal/")

  ;; Custom function to open journal in buffer
  (defun my/org-roam-journal-buffer ()
    "Open or create today's org-roam journal entry in a new buffer."
    (interactive)
    (let* ((daily-dir (expand-file-name org-roam-dailies-directory org-roam-directory))
           (filename (format-time-string "%Y-%m-%d.org"))
           (filepath (expand-file-name filename daily-dir)))
      ;; Create directory if needed
      (unless (file-exists-p daily-dir)
        (make-directory daily-dir t))
      ;; Create template if file doesn't exist
      (unless (file-exists-p filepath)
        (with-temp-file filepath
          (insert (format "#+TITLE: Daily Journal - %s\n" (format-time-string "%A, %B %d, %Y"))
          (insert "#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
          (insert "* Daily Journal: " (format-time-string "%A, %B %d, %Y") "\n")
          (insert ":PROPERTIES:\n")
          (insert ":CREATED: %U\n:LAST_MODIFIED: %U\n")
          (insert ":ID: %(org-id-get-create)\n:END:\n\n")
          (insert "** Morning Reflections\n%U\n- Mood: \n- Energy Level: \n- Focus: \n- What's on your mind?\n\n")
          (insert "** Daily Goals\n- [ ] Priority 1\n- [ ] Priority 2\n- [ ] Priority 3\n\n")
          (insert "** Tasks\n\n\n")
          (insert "** Evening Reflections\n%U\n- Wins/Achievements:\n- Challenges/Lessons:\n- Tomorrow's Focus:\n\n")
          (insert "** Gratitude\n- I'm grateful for:\n1. \n2. \n3. \n\n")
          (insert "** Notes & Ideas\n- Random thoughts:\n- Interesting links:\n- Creative ideas:\n\n")
          (insert "** Daily Review\n- Tasks Completed:\n- Tasks Migrated:\n- Habit Tracking:\n\n")
          (insert "*** Related Notes\n[[id:%(org-id-get-create)][This Journal Entry]]\n")))
      ;; Open in buffer
      (find-file filepath)))

  ;; Keybinding;
  (map! :leader
       (:prefix ("j" . "journal")
        :desc "Open daily journal" "d" #'my/org-roam-journal-buffer))))

;; Add last-modified timestamp to Org files
(defun my/update-last-modified ()
  "Update LAST_MODIFIED property in file header on save."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (time-str (format-time-string "[%Y-%m-%d %a %H:%M]")))
        ;; Update existing property or insert new one
        (if (re-search-forward "^#\\+LAST_MODIFIED:" nil t)
            (progn
              (delete-region (point) (line-end-position))
              (insert " " time-str))
          ;; Insert after existing header content
          (goto-char (point-min))
          (re-search-forward "^#\\+" nil t)
          (end-of-line)
          (insert "\n#+LAST_MODIFIED: " time-str))))))

;; Enable for all Org files
(add-hook 'before-save-hook #'my/update-last-modified)

;; Helper function to find or create a file in the current directory
(defun my/find-or-create-file-in-current-dir ()
  "Return the buffer for a file in the current working directory.
Prompts for a filename. Creates the file if it doesn't exist."
  (interactive)
  (let* ((default-dir default-directory) ; Gets the reliable current directory
         (filename (read-string "Enter filename (without .org): "))
         (full-path (expand-file-name (concat filename ".org") default-dir)))
    ;; Create the file's parent directory if needed (optional)
    (make-directory (file-name-directory full-path) t)
    ;; Find the file and return its buffer
    (find-file-noselect full-path)))

(after! org
  (setq org-capture-templates
        '(("v" "Volleyball Drill" entry
           ;; Use the function as the target
           (function my/find-or-create-file-in-current-dir)
           "* %^{Drill Name}
:PROPERTIES:
:DIFFICULTY: %^{Beginner/Intermediate/Advanced}
:DRILL_TYPE: %^{Warm-up/Passing/Serving/Gameplay}
:DURATION: %^{Duration in minutes}
:EQUIPMENT: %^{Balls/Cones/Net/etc}
:CATEGORY: %^{Category}
:TAGS:
:END:
%?")

          ("b" "Blog Post" entry
           ;; Use the same function for blog posts
           (function my/find-or-create-file-in-current-dir)
           "* %^{Title}
:PROPERTIES:
:EXPORT_DATE: %<%Y-%m-%d %H:%M>
:SUBTITLE: %^{Subtitle}
:CATEGORY: %^{Category}
:TAGS:
:END:
%?"))))


(require 'exercism)

(use-package! gleam-ts-mode
  :mode (rx ".gleam" eos))

(after! treesit
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode)))

(after! gleam-ts-mode
  (unless (treesit-language-available-p 'gleam)
    (gleam-ts-install-grammar)))
;; Whenever you reconfigure a package, make sure to wrap your config in an

;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
