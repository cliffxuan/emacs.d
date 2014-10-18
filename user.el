;;; package --- Summary
;;; Commentary:
;; Emacs configuration

;;; Code:
;; http://cask.github.io/
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Keeps ~Cask~ file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(require 'pallet)

;; Show line number
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d")

(setq case-replace nil)
(setq case-fold-search nil)

;; Hightlight line
(global-hl-line-mode 1)

;; Evil mode
(require 'evil)
(evil-mode 1)
(require 'cl)
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (inferior-python-mode . emacs)
                              (nrepl-mode . insert)
                              (pylookup-mode . emacs)
                              (comint-mode . normal)
                              (shell-mode . insert)
                              (git-commit-mode . insert)
                              (git-rebase-mode . emacs)
                              (term-mode . emacs)
                              (help-mode . emacs)
                              (helm-grep-mode . emacs)
                              (grep-mode . emacs)
                              (bc-menu-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . emacs)
                              (wdired-mode . normal))
  do (evil-set-initial-state mode state))

;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; ipdb set/remove
(defun set-ipdb ()
  "Add debug code and move line down."
  (interactive)
  (back-to-indentation)
  (let ((x (current-column)))
    (insert "import ipdb; ipdb.set_trace()##########\n")
    (insert (make-string x ?\s))
    (save-buffer)))

(defun remove-ipdb ()
  "Remove py debug code, if found."
  (interactive)
  (let ((cur (point)))
    (goto-char (point-min))
    (delete-matching-lines "^[ ]*import ipdb; ipdb.set_trace()##########")
    (save-buffer)
    (goto-char cur)))

(defmacro bind (&rest COMMANDS)
  "Convience macro which create a lambda interactive COMMANDS."
  `(lambda ()
     (interactive)
     ,@COMMANDS))

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is Emacs LISP, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffix-map
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            )
          )
         (f-name (buffer-file-name))
         (f-suffix (file-name-extension f-name))
         (prog-name (cdr (assoc f-suffix suffix-map)))
         (cmd-str (concat prog-name " \""   f-name "\""))
         )

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified.  Do you want to save first? ")
          (save-buffer) ) )

    (if (string-equal f-suffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension f-name))
      (if prog-name
          (progn
            (message "Running…")
            (shell-command cmd-str "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ) ) ))

(evil-leader/set-key
  "t" (bind
        (evil-window-split)
        (windmove-down)
        (eshell))
  "e" 'switch-to-buffer
  "f" (bind
         (dired "."))
  "k" 'kill-buffer
  "l" 'flycheck-list-errors
  "m" 'magit-status
  "o" 'delete-other-windows
  "h" 'delete-window  ;; hide window
  "r" 'run-current-file
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "w" 'delete-trailing-whitespace
  "x" (bind (execute-extended-command nil)))
(evil-leader/set-key-for-mode 'python-mode "b" 'set-ipdb)
(evil-leader/set-key-for-mode 'python-mode "d" 'remove-ipdb)


;; Swap ";" and ":" in evil mode
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;; Themes
(load-theme 'monokai t)
(setq evil-default-cursor (quote (t "Firebrick")))

;; Don't show startup screen
(setq inhibit-startup-screen t)


;; give the underscore the word syntax-class in all C-like buffers.
(modify-syntax-entry ?_ "w")
;; Show keystrokes
;;(setq echo-keystrokes 0.02)

;; Enable Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'powerline)
(powerline-center-evil-theme)

;; Git
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Surround
(require 'surround)
(global-surround-mode 1)

;; Complete brackets, quotes, etc
(electric-pair-mode 1)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; No unnecessary whitespaces
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
;; make it happy
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; Indicates the 79th column for python
(require 'fill-column-indicator)
(setq-default fci-rule-column 79)
(add-hook 'python-mode-hook 'fci-mode)

;; jedi completion
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(require 'virtualenvwrapper)
(venv-initialize-eshell)

;; Ipython integration with fgallina/python.el
(require 'python)
(defun setup-ipython ()
  "Setup ipython integration with `python-mode'."
  (interactive)

  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n"))

(evilnc-default-hotkeys)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in
;; ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; disable automatic line break in html mode
;; (add-hook 'html-mode-hook 'turn-off-auto-fill)
;; (add-hook 'html-model-hook
;;           (function (lambda ()
;;                       (setq evil-shift-width 2))))

(eval-after-load 'html-mode
  '(progn
     (turn-off-auto-fill)
     (setq evil-shift-width 2)
     (setq tab-width 2)))

(custom-set-variables
 '(ls-lisp-verbosity nil))

(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")

(add-hook 'term-exec-hook
  (function
   (lambda ()
     (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))

;; zsh is problematic for managing path
(setq shell-file-name "/bin/bash")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; do not prettify lambdas
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)

(provide 'user)
;;; user.el ends here
