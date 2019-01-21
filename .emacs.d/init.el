(defun path-dos2unix (path)
  (cond ((equal system-type 'cygwin)
         (substring
          (shell-command-to-string
           (concat "cygpath -u \""
                   (replace-regexp-in-string
                    "\\\\" "\\\\\\\\"
                    (replace-regexp-in-string "\"" "" path)) "\"")) 0 -1))
        (t (replace-regexp-in-string "\\\\" "/" path))))
(add-to-list 'load-path (concat (path-dos2unix (or (getenv "ENVDIR")
                                                   (getenv "HOME"))) "/elisp"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "Blue3" :weight bold))))
 '(completions-common-part ((t (:foreground "gray50"))))
 '(completions-first-difference ((t nil)))
 '(diff-context-face ((((class color) (background light)) (:foreground "medium blue"))) t)
 '(highlight ((((class color) (background light)) (:background "lavender"))))
 '(mode-line ((nil (:background "LightSteelBlue2" :box (:line-width -1 :style released-button)))))
 '(outline-2 ((t (:foreground "orange red"))))
 '(region ((t (:background "orange" :distant-foreground "gtk_selection_fg_color"))))
 '(speedbar-button-face ((((class color) (background light)) (:foreground "dark green4"))))
 '(speedbar-file-face ((((class color) (background light)) (:foreground "dark cyan4"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "linen"))))
 '(whitespace-line ((t (:background "light gray")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list (quote (("Okular" "okular %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "Okular")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(ac-auto-show-menu t)
 '(ac-candidate-menu-min 2)
 '(ac-use-comphist nil)
 '(archive-zip-use-pkzip nil)
 '(async-shell-command-buffer (quote new-buffer))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.bck"))))
 '(bs-attributes-list
   (quote
    (("" 1 1 left bs--get-marked-string)
     ("M" 1 1 left bs--get-modified-string)
     ("R" 2 2 left bs--get-readonly-string)
     ("Buffer" bs--get-name-length 10 left bs--get-name)
     ("" 1 1 left " ")
     ("Size" 8 8 right bs--get-size-string)
     ("" 1 1 left " ")
     ("File" 12 12 left bs--get-file-name))))
 '(bs-configurations
   (quote
    (("files" nil nil nil bs-visits-non-file nil)
     ("c files" "\\.[ch]\\|\\.idl" nil "" nil nil)
     ("bat files" "\\.bat" nil "" nil nil)
     ("no files" nil bs-visits-non-file "" nil nil)
     ("all" nil nil nil nil nil)
     ("Compilation" "\\*Compilation" nil "" nil nil)
     ("Javascript" "\\.js" nil "" nil nil))))
 '(bs-default-configuration "all")
 '(bs-max-window-height 1000)
 '(c-backslash-max-column 79)
 '(c-basic-offset 4)
 '(c-default-style "Bernhard")
 '(c-echo-syntactic-information-p t)
 '(calendar-time-display-form
   (quote
    (24-hours ":" minutes
              (if time-zone " (")
              time-zone
              (if time-zone ")"))))
 '(cc-other-file-alist
   (quote
    (("\\.cc$"
      (".hh" ".h"))
     ("\\.hh$"
      (".cc" ".C"))
     ("\\.c$"
      (".h"))
     ("\\.h$"
      (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
     ("\\.C$"
      (".H" ".hh" ".h"))
     ("\\.H$"
      (".C" ".CC"))
     ("\\.CC$"
      (".HH" ".H" ".hh" ".h"))
     ("\\.HH$"
      (".CC"))
     ("\\.cxx$"
      (".hh" ".h"))
     ("\\.cpp$"
      (".hpp" ".h"))
     ("\\.hpp$"
      (".cpp")))))
 '(clearcase-auto-dired-mode nil)
 '(clearcase-diff-on-checkin t)
 '(clearcase-dired-highlight nil)
 '(clearcase-make-backup-files t)
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comment-auto-fill-only-comments t)
 '(company-dabbrev-char-regexp "\\sw\\|\\s_")
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(compilation-scroll-output t)
 '(completion-styles (quote (basic partial-completion emacs22 substring)))
 '(confirm-nonexistent-file-or-buffer t)
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-brace-offset -2)
 '(cperl-close-paren-offset -2)
 '(cperl-continued-statement-offset 2)
 '(cperl-electric-parens-mark nil)
 '(cperl-hairy nil)
 '(cperl-indent-parens-as-block t)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(dabbrev-case-replace nil)
 '(default-frame-alist (quote ((width . 100) (top . 10))))
 '(delete-old-versions (quote other))
 '(directory-free-space-program nil)
 '(dired-backup-overwrite t)
 '(dired-clean-up-buffers-too nil)
 '(dired-dnd-protocol-alist nil)
 '(dired-dwim-target t)
 '(dired-enable-local-variables nil)
 '(dired-find-subdir t)
 '(dired-guess-shell-alist-user
   (quote
    (("\\.jpg$" "display")
     ("\\.(pm|pod)$" "perldoc")
     ("\\.ps$" "gv"))))
 '(dired-listing-switches "-agGlh")
 '(dired-local-variables-file nil)
 '(dired-recursive-copies t)
 '(dired-recursive-deletes (quote top))
 '(display-time-24hr-format t)
 '(ediff-default-filtering-regexp "")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-indent-mode nil)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(electric-pair-pairs (quote ((34 . 34) (39 . 39))))
 '(electric-pair-skip-self nil)
 '(enable-recursive-minibuffers t)
 '(eshell-ask-to-save-history (quote always))
 '(eshell-command-interpreter-max-length 500)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 1280)
 '(eshell-last-dir-ring-size 500)
 '(eshell-save-history-on-exit t)
 '(even-window-sizes nil)
 '(fast-lock-save-events (quote (save-buffer kill-buffer kill-emacs)))
 '(ff-always-try-to-create nil)
 '(ff-case-fold-search t)
 '(ffap-shell-prompt-regexp ".+>")
 '(fill-column 80)
 '(find-file-visit-truename nil)
 '(find-grep-options "-iq")
 '(find-ls-option (quote ("-exec ls -dagGlh {} +" . "-dagGlh")))
 '(focus-follows-mouse nil)
 '(font-lock-global-modes t)
 '(font-lock-maximum-size 256000)
 '(generic-define-mswindows-modes t)
 '(generic-use-find-file-hook t)
 '(glasses-face (quote bold))
 '(glasses-separator "")
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t nil (hl-line))
 '(global-mark-ring-max 1024)
 '(gnus-summary-line-format "%U%R%d%(%[%4L: %-20,20n%]%) %s")
 '(gud-chdir-before-run nil)
 '(hi-lock-auto-select-face nil)
 '(history-delete-duplicates t)
 '(history-length t)
 '(hl-highlight-mode t)
 '(icomplete-mode t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-cache-unc-host-shares-time 10000.0)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere nil)
 '(ido-ignore-directories-merge
   (quote
    ("^[abefghijklmnopqrstuvwxyzABEFGHIJKLMNOPQRSTUVWXYZ]:")))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_tmp" "\\.doc$")))
 '(ido-ignore-unc-host-regexps (quote ("")))
 '(ido-max-dir-file-cache 10000)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(inhibit-startup-screen t)
 '(ispell-dictionary "german")
 '(js-auto-indent-flag t)
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(js2-highlight-level 3)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 1024)
 '(kill-whole-line t)
 '(large-file-warning-threshold 200000000)
 '(lazy-lock-defer-on-scrolling t)
 '(lazy-lock-stealth-load 50)
 '(lazy-lock-stealth-nice 0.25)
 '(lazy-lock-stealth-verbose t)
 '(log-edit-require-final-newline nil)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-verbosity nil)
 '(mark-even-if-inactive t)
 '(mark-ring-max 1024)
 '(menu-bar-mode nil)
 '(message-log-max t)
 '(moccur-use-ee t)
 '(mode-require-final-newline nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(org-agenda-files "~/org/.agenda_files")
 '(org-agenda-use-time-grid nil)
 '(org-clock-continuously nil)
 '(org-clock-idle-time 15)
 '(org-clock-persist t)
 '(org-duration-format (quote ((special . h:mm))))
 '(org-export-headline-levels 0)
 '(org-html-doctype "html5")
 '(org-startup-truncated nil)
 '(outline-auto-activation "ask")
 '(outline-minor-mode t t)
 '(package-load-list
   (quote
    (all
     (eglot nil)
     (lsp-java nil)
     (lsp-mode nil)
     (meghanada nil)
     (magit nil)
     (ztree nil)
     (web-mode nil))))
 '(package-selected-packages
   (quote
    (mvn feature-mode ac-js2 auto-complete csharp-mode electric-spacing highlight-thing hl-anything js2-mode markdown-mode powershell pug-mode refine web-mode yaml-mode yasnippet ztree)))
 '(parens-require-spaces nil)
 '(perl-continued-statement-offset 2)
 '(perl-indent-continued-arguments 2)
 '(perl-indent-level 2)
 '(perl-indent-parens-as-block t)
 '(powershell-indent 2)
 '(ps-bottom-margin 16)
 '(ps-font-size (quote (7 . 10)))
 '(ps-footer-offset 16)
 '(ps-header-offset 16)
 '(ps-inter-column 16)
 '(ps-left-margin 16)
 '(ps-n-up-border-p nil)
 '(ps-n-up-margin 16)
 '(ps-n-up-printing 2)
 '(ps-paper-type (quote a4))
 '(ps-print-header-frame t)
 '(ps-print-only-one-header t)
 '(ps-right-margin 16)
 '(ps-top-margin 16)
 '(python-indent-offset 3)
 '(read-buffer-completion-ignore-case t)
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("[/\\]te?mp[/\\]\\|\\.timelog$\\|\\.ido\\.last$")))
 '(recentf-max-saved-items 1000)
 '(regexp-search-ring-max 160)
 '(require-final-newline nil)
 '(revert-without-query (quote ("")))
 '(rng-nxml-auto-validate-flag nil)
 '(savehist-additional-variables (quote (compile-command)))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode (quote right))
 '(search-ring-max 160)
 '(select-enable-clipboard t)
 '(send-mail-function (quote mailclient-send-it))
 '(sentence-end-double-space nil)
 '(sgml-basic-offset 4)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(smerge-command-prefix "v")
 '(sort-fold-case t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 40)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(speedbar-tag-hierarchy-method
   (quote
    (speedbar-sort-tag-hierarchy speedbar-trim-words-tag-hierarchy)))
 '(speedbar-use-images nil)
 '(split-width-threshold nil)
 '(tab-width 2)
 '(tcl-indent-level 2)
 '(tempo-insert-region t)
 '(timeclock-ask-before-exiting nil)
 '(timeclock-modeline-display t nil (timeclock))
 '(timeclock-use-display-time nil nil (time))
 '(tool-bar-mode nil)
 '(track-eol t)
 '(transient-mark-mode t)
 '(unibyte-display-via-language-environment t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(use-dialog-box nil)
 '(vc-ignore-dir-regexp
   "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\|DavWWWRoot")
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(w3m-local-find-file-function nil)
 '(w3m-local-find-file-regexps (quote (nil)))
 '(warning-suppress-types (quote ((undo discard-info))))
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-part-padding 0)
 '(web-mode-script-padding 4)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))
 '(windmove-wrap-around t)
 '(woman-use-own-frame nil))

;; NT-emacs assumes a Windows command shell, which you change
;; here.
;;
;; (setq process-coding-system-alist '(("bash" . undecided-unix)))
;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)
;;
;; This removes unsightly ^M characters.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(setenv "PAGER" "")
(defun my-eshell-history ()
  (interactive)
  (insert
   (completing-read
    "Eshell history: "
    (delete-dups (ring-elements eshell-history-ring)))))
(defun my-eshell-hook () (local-set-key (kbd "M-r") 'my-eshell-history))
(add-hook 'eshell-mode-hook 'my-eshell-hook)

(when (require 'package nil t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

(when (require 'google-this nil t)
  (google-this-mode 1))
(when (require 'highlight-thing nil t)
  (global-highlight-thing-mode))

;;(global-auto-complete-mode);;(global-company-mode t)

;;(cond ((equal window-system 'w32)
;;       (set-default-font "-*-Lucida Console-normal-r-*-*-12-80-*-*-c-*-*-ansi-")))
(when (display-graphic-p)
  (cond
   ((font-info "Lucida Console")
    (modify-all-frames-parameters '((font . "Lucida Console-8"))))
   ((font-info "Liberation Mono")
    (modify-all-frames-parameters '((font . "Liberation Mono-8"))))
   ((font-info "Noto Mono")
    (modify-all-frames-parameters '((font . "Noto Mono-8")))))
  (modify-all-frames-parameters
   (list (cons 'height (/ (- (nth 4 (assq 'workarea (car (display-monitor-attributes-list)))) 50) (frame-char-height))))))

(when (string= user-real-login-name "root")
  (setq frame-title-format "emacs - root")
  (set-face-attribute 'mode-line nil :background "violet"))

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (modify-syntax-entry ?+ "." web-mode-syntax-table)
  (setq web-mode-comment-prefixing nil)
  (defun my-web-mode-indent (off)
    (setq web-mode-code-indent-offset off
          web-mode-css-indent-offset off
          web-mode-markup-indent-offset off
          web-mode-style-padding off
          web-mode-script-padding 2))
  (defun my-web-mode-hook ()
    (my-web-mode-indent 2)
    (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
    (yas-minor-mode-on))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  )

(require 'generic-x)
;;(modify-syntax-entry ?\\ "_" bat-generic-mode-syntax-table)

(define-generic-mode 'cmd-generic-mode
  '("//")
  nil
  '(("^@.*$%" 0 'font-lock-builtin-face)
    ("\\(!\\||\\).*" . 'font-lock-type-face)
    ("\*\*\*ERROR\*\*\*.*" . 'font-lock-warning-face)
    ("File: .*, Line: .*" . 'font-lock-warning-face))
  '("\\.(cmd|inp)\\'")
  '((lambda ()
      (setq
       align-mode-rules-list
       '((cmd-space
          (regexp . "[^ ]\\(\\s-+[!%]?\\)[^ ]")
          (justify . t)
          ))
       align-mode-exclude-rules-list
       '((cmd-comment
          (regexp . "\\(//.*\\)$"))))))
  "Mode for Emil cmd-files.")

(define-generic-mode 'lint-generic-mode
  '("//")
  '("wlib" "-d" "elib" "function" "emacro" "esym")
  '(("+" 0 'font-lock-type-face)
    ("-" . 'font-lock-warning-face))
  '("\\.lnt\\'")
  nil
  "Mode for Lint-files.")

;; Selbstdefinierte Funktion: Diese Funktion fuehrt in allen Dired-Buffern die
;; Funktion dired-do-flagged-delete aus, loescht also gegebenenfalls die dazu
;; markierten Dateien (mit Rueckfrage) und beendet Emacs.
(defun expunge-and-exit ()
  "Before exiting, expunge in Dired D-flagged files"
  ;;(interactive)

  (mapcar (lambda (buffer)
            (switch-to-buffer buffer)
            (when (equal major-mode 'dired-mode)
              (dired-do-flagged-delete)))
          (buffer-list))
  (timeclock-reread-log)
  (when (timeclock-currently-in-p) (timeclock-out))
  ;;(save-buffers-kill-emacs)
  )
(defun my-save-buffer () (interactive)
       ;;(save-buffer)
       ;;(print "my-save-buffer")
       (setq buffer-backed-up nil)
       (backup-buffer))
(add-hook 'after-save-hook 'my-save-buffer)
(add-hook 'kill-emacs-hook 'expunge-and-exit)

(defun fill-line ()
  "Fill current line."
  (interactive)
  (save-excursion
    (fill-region (line-beginning-position) (line-end-position))))

;;(setq dired-x-hands-off-my-keys nil)
;;(require 'idl-mode nil t)
;;(require 'w3m-load nil t)
;;(require 'tramp nil t)
;;(defalias 'perl-mode 'cperl-mode)

(when (and t (require 'auto-complete nil t))
  (global-auto-complete-mode t)
  ;;(add-to-list 'ac-modes 'org-mode 'text-mode)
  (nconc ac-modes '(org-mode text-mode)))
;; (when (require 'ac-dabbrev)
;;   (set-default 'ac-sources (list ac-source-dabbrev)))
;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(when (require 'yasnippet nil t)
  (mapc
   #'(lambda (mode)
      (yas-define-snippets
       mode
       '(("for" "for (var ${1:i} = ${2:0}; $1 < ${3:collection}.length; $1++) {\n$0\n}" "for1")
         ("f" "function ${1:name}(${2:arg}) {\n$0\n}\n" "function")
         ("if" "if (${1:condition}) {\n$0\n}" "if" )))
      )
   '(js2-mode web-mode))
  (setq yas-snippet-dirs '())
  ;; (yas-global-mode 1)
  )

(setq diff-switches "-c")
;;(setq ediff-combination-pattern '("" "" ""))
(setq ediff-diff-ok-lines-regexp "\\(.?\\)")

(put 'dired-do-delete 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

(cond
 ((or (equal system-type 'windows-nt)
      (equal system-type 'cygwin))
  (defun msdn-help ()
    (interactive "*")
    ;;    (setq perl-command (concat "perl -e \"require Win32::OLE;my $dte = Win32::OLE->GetActiveObject('VisualStudio.DTE.7.1');$dte->ExecuteCommand(\\\"Help.F1Help\\\", \\\"" (current-word) "\\\") if defined $dte; print \\\"Visual Studio .Net Currently not Running\\\" if ! defined $dte; \""))
    (setq perl-command (concat "perl -e \"require Win32::OLE;my $dte = Win32::OLE->CreateObject('VisualStudio.DTE.7.1');$dte->ExecuteCommand(\\\"Help.F1Help\\\", \\\"" (current-word) "\\\") if defined $dte; print \\\"Visual Studio .Net Currently not Running\\\" if ! defined $dte; \""))
    (shell-command perl-command))

  ;;(global-set-key [f1] 'msdn-help)
  ;;(setq coding-system-for-read 'raw-text-dos)
  (prefer-coding-system 'utf-8)
  ;; (setq find-dired-find-program "unixfind")
  ;; (setq find-program "unixfind")
  (setq ps-printer-name t)
  (setq ps-lpr-command "c:\\Programme\\ut\\Ghostview\\gsview\\gsview32.exe")
  ;;(when (require 'gnuserv nil t) (gnuserv-start))
  (defun path-unix2dos (path)
    (cond ((equal system-type 'cygwin)
           (substring
            (shell-command-to-string (concat "cygpath -w \"" path "\"")) 0 -1))
          (t (replace-regexp-in-string "/" "\\\\" path))))
  (defun path2clipboard (arg) (interactive "P")
         (w32-set-clipboard-data (if arg (get-filename) (path-unix2dos (get-filename)))))
  (defun path-yank (arg) (interactive "P")
         (kill-new (if arg (get-filename) (path-unix2dos (get-filename)))))
  (defun totalcmd () (interactive)
         (start-process "totalcmd" nil "c:\\Programme\\TotalCMD\\TOTALCMD.EXE"
                        "/O" (path-unix2dos (get-filename))))
  (defun start () (interactive)
         ;;(32-shell-execute "open" (get-filename)) ;;ms-office problems
         (start-process "start" nil "explorer" (path-unix2dos (get-filename)))
         ))
 (t
  (defun path2clipboard () (interactive)
         (x-select-text (get-filename))
         (kill-new (get-filename)))
  (defun start () (interactive)
         (shell-command
          (concat "xdg-open \"" (get-filename) "\"&")))
  ))

(defun pipe-picture ()
  (interactive)
  (let ((tmp (make-temp-file "pipe-picture")))
    (write-region (point-min) (point-max) tmp)
    (start-process-shell-command "pipe-picture" "pipe-picture" "pipe-picture" tmp)))

(server-start)
;;(setq gnuserv-frame (selected-frame))
(setq frame-title-format '("%f (" user-real-login-name ") - Emacs"))
(setq icon-title-format '("%f (" user-real-login-name ") - Emacs"))

;;(add-hook 'find-file-hooks '(lambda () (hl-line-mode 1)))

(defun c-init ()
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (define-key c-mode-map "\C-cc" 'compile)
  (define-key c++-mode-map "\C-cc" 'compile)
  ;;(require 'expand)
  (defvar menu-bar-c-insert-menu (make-sparse-keymap "Insert"))
  (define-key c-mode-base-map [menu-bar insert]
    (cons "Insert" menu-bar-c-insert-menu))
  ;; (define-key menu-bar-tools-menu [insert]
  ;;   (cons "Insert" menu-bar-c-insert-menu))
  )
(add-hook 'c-initialization-hook 'c-init)

(defconst bernhard-c-style
  '(;(c-basic-offset . 3)
    (c-offsets-alist
     . ((substatement-open . 0)
        (inline-open . 0)
        (case-label . +)
        (arglist-close . c-lineup-close-paren)
        (cpp-macro-cont . +))))
  "Bernhard's C Programming Style")
(c-add-style "Bernhard" bernhard-c-style )

(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  ;;  (c-set-style "GNU")
  (c-toggle-hungry-state 1)
  ;;   (c-setup-filladapt)
  ;;   (filladapt-mode 1)
  ;;   (cond ((not (string-match "IDL" mode-name))
  ;;          (imenu-add-menubar-index)))
  (setq paragraph-start "\f\\|[ \t]*$")
  (setq paragraph-separate "[ \t\f]*$")
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (setq c-basic-offset 4)
;;             (setq c-default-style "bsd")))
;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp-java-enable)

;; (require 'align)
;; (setq
;;  align-rules-list
;;  (append
;;   align-rules-list
;;   '((my-braces
;;      (regexp . "\\(\\s-*\\)[}]")
;;      (repeat . t)
;;      (spacing . 0))
;;     (my-comma-delimiter
;;      (regexp . "\\(\\s-*\\),")
;;      (repeat . t)
;;      (spacing . 0)))
;;   ))

(defun my-compilation-buffer-name-fun (mode)
  (concat "*" mode " " (file-name-directory (get-filename)) "*"))
(setq compilation-buffer-name-function 'my-compilation-buffer-name-fun)

(defun lint-compilation-buffer-name-fun (mode-name)
  (concat "*lint " (get-filename) "*"))
(defun lint () (interactive)
       (let ((compilation-buffer-name-function 'lint-compilation-buffer-name-fun))
         (compile (concat "makeit /lnt " (get-filename)))))
(defun mylint () (interactive)
       (let ((compilation-buffer-name-function 'lint-compilation-buffer-name-fun))
         (compile (concat "lintallcfgs.bat " (get-filename)))))
;;(require 'compile)
(setq compilation-error-regexp-alist
      '(("^\\([0-9]+>\\)?[ \t]*\\(\\(?:[a-zA-Z]:\\)?[^(\t\n]+\\)(\\([0-9]+\\)) \
: \\(?:\\(?:fatal \\)?error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
        ("^\\(?:[0-9]+>\\)?[ \t]*\\([^(\t\n]+\\)(\\([0-9]+\\)) : " 1 2)
        ant java perl))
;; Add color formatting to *compilation* buffer
;; (add-hook 'compilation-filter-hook
;;           (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
(ignore-errors
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (if (boundp 'compilation-filter-start)
            (ansi-color-apply-on-region compilation-filter-start (point))))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(add-hook
 'expand-load-hook
 '(lambda ()
    (add-hook 'expand-expand-hook 'indent-according-to-mode)
    (add-hook 'expand-jump-hook 'indent-according-to-mode)))

(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (local-set-key "\C-c\C-c" 'comment-region)
    (local-set-key [(control tab)] 'lisp-complete-symbol)))

;; In LaTeX immer outline-Modus benutzen und fuer '\"' immer '\"' einsetzen
;; (und nicht '''')
(defvar my-latex-make '("Make" "make %f" TeX-run-command t t))
(defvar my-latex-bibtex
  '("BibTeXVerbose" "bibtex -verbose %s" TeX-run-BibTeX nil nil))
(defun save-region ()
  (interactive)
  (write-region (region-beginning) (region-end) "_region_.tex"))
(defun latex-init ()
  (local-set-key "\C-c\C-r" 'save-region)
  (local-set-key "\"" 'self-insert-command)
  (modify-syntax-entry ?\" "w")
  (outline-minor-mode)
  (if (not (member my-latex-make TeX-command-list))
      (setq TeX-command-list (cons my-latex-make TeX-command-list)))
  (if (not (member my-latex-bibtex TeX-command-list))
      (setq TeX-command-list (cons my-latex-bibtex TeX-command-list))))
(add-hook 'LaTeX-mode-hook 'latex-init)

;;(setq ispell-dictionary "deutsch")
;;(setq ispell-alternate-dictionary "/home/rotter/.ispell_deutsch")
;;(setq ispell-check-comments nil)

(setq european-calendar-style t
      calendar-latitude 48.1
      calendar-longitude 11.5
      calendar-location-name "Muenchen"
      calendar-week-start-day 1)
(add-hook 'diary-hook 'appt-make-list)

(setq-default ztree-diff-filter-list nil)
(setq hexl-options "-hex -iso")
(add-hook 'makefile-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
(windmove-default-keybindings)
;;(dynamic-completion-mode)

(setq backup-ignore
      '("[/\\]te?mp[/\\]" "\.log$" "\.emacs-places$" "\/pwd\.\(adr\|tex\)"
        "ido.last" ".session" ".recentf"))
(defun normal-backup-enable-predicate (name)
  "`backup-enable-predicate' function of Bernhard Rotter."
  (and
   ;;(not (equal "txt" (file-name-extension name)))
   (< (buffer-size) 1000000)
   (let ((res t))
     (dolist (i backup-ignore res)
       (setq res (and res (not (string-match i name))))))))

(defun my-make-backup-file-name (f)
  "Create the non-numeric backup file name for `f'."
  (let ((backup-dir
         (concat
          (or (getenv "HOME") (getenv "APPDATA")) "/.bck/"
          (replace-regexp-in-string ":" "" (file-name-directory f))))
        (basename
         (concat (file-name-sans-extension (file-name-nondirectory f))
                 "_" (format-time-string "%Y%m%d_%H%M")))
        (extension
         (concat (cond ((file-name-extension f) ".")) (file-name-extension f))))
    (make-directory backup-dir t)
    (if (file-exists-p (concat backup-dir basename extension))
        (concat backup-dir basename "-2" extension)
      (concat backup-dir basename extension))))

;; (add-hook
;;  'write-file-functions
;;  '(lambda ()
;;     (if (normal-backup-enable-predicate buffer-file-name)
;;         (write-region nil nil (my-make-backup-file-name buffer-file-name) t 1))
;;     nil))
(push `("^//.*/DavWWWRoot/.*\\([^/]*\\)$"
        ,(concat temporary-file-directory "\\2") t)
      auto-save-file-name-transforms)
(push "DavWWWRoot" inhibit-local-variables-regexps)

;; (load "org-install" t)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)

;; (require 'speedbar)
;; (speedbar-add-ignored-path-regexp "/api/src/")
;; (speedbar-add-ignored-path-regexp "/emilbase/src/")
;; (speedbar-add-ignored-path-regexp "@@.*")
;; (setq speedbar-fetch-etags-arguments '("-o" "-"))

;; (mapcar
;;  (lambda (mode)
;;    (font-lock-add-keywords mode '(("\t\\|\\( +$\\)" 0
;;                                    'my-irregular-space-face t))))
;;  '(bat-generic-mode
;;    c++-mode c-mode idl-mode
;;    emacs-lisp-mode
;;    latex-mode
;;    makefile-mode
;;    perl-mode
;;    text-mode))

;; (make-empty-face 'my-irregular-space-face)
;; (set-face-background 'my-irregular-space-face "beige")

;;(require 'ffap)                      ; load the package
;;(ffap-bindings)                      ; do default key bindings
;; (defun my-ffap (s) ""
;;   (print s t)
;;   nil)
;;(push '(".*" . my-ffap) ffap-alist)
;; (push '(file "--:$+<>@-Z_a-z~\\" "<@" "@>;.,!?") ffap-string-at-point-mode-alistq)
;;(when (require 'ido nil t) (ido-mode t))

;;(global-highlight-changes)

;; (autoload 'tmm "Text mode substitute for menubar" t)
;; (global-set-key [f10] 'tmm-menubar)

;; (set-language-environment "Latin-1")
;; (standard-display-european 1)

;;(pc-bindings-mode)
;;(mouse-avoidance-mode 'exile)
;;(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)
;;(autoload 'javascript-mode "javascript" nil t)
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;; (autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
;; (autoload 'autoit-mode "autoit-mode")
;; (autoload 'powershell-mode "powershell-mode" nil t)
;; (autoload 'powershell "powershell" nil t)
;;(autoload 'csharp-mode "csharp-mode")
;;(setq eglot-server-programs '((java-mode . ("127.0.0.1" "48032"))))
(setq eglot-server-programs '((java-mode . ("/cygdrive/C/Program Files/dev/jdk-11.0.1/bin/java.exe" "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044" "-Declipse.application=org.eclipse.jdt.ls.core.id1" "-Dosgi.bundles.defaultStartLevel=4" "-Declipse.product=org.eclipse.jdt.ls.core.product" "-Dlog.level=ALL" "-noverify" "-Xmx1G" "-jar" "C:\\Program Files\\dev\\jdt-language-server-0.27.0-201810230512\\plugins\\org.eclipse.equinox.launcher_1.5.200.v20180922-1751.jar" "-configuration" "C:\\Program Files\\dev\\jdt-language-server-0.27.0-201810230512\\config_win" "-data" "C:\\Users\\bernh\\Documents\\dev\\eclipse.jdt.ls" "--add-modules=ALL-SYSTEM" "--add-opens" "java.base/java.util=ALL-UNNAMED" "--add-opens" "java.base/java.lang=ALL-UNNAMED" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"))))
(when (require 'mvn nil t)
  (defun my-mvn-test ()
    (interactive)
    (mvn "test" "-Dcucumber.options=\"--tags @SCOPE-553\"")))

(when (require 'js2-mode nil t)
  ;;(autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook '(lambda () (yas-minor-mode-on)))
  )

(defun true () t)
(add-to-list 'auto-mode-alist '("@@.*" true t))
(add-to-list 'auto-mode-alist '("\\.au3" . autoit-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
;;(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.iss\\'" . ini-generic-mode))
(add-to-list 'auto-mode-alist '("\\.keep.*" true t))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.orig" true t))
;;(add-to-list 'auto-mode-alist '("\\.ps1" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.vcproj$" . nxml-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(frm\\|bas\\|cls\\|vbs?\\)$" . visual-basic-mode))
(setq visual-basic-keywords-to-highlight t)
;; (when (load "autoit-mode" t)
;;   (add-to-list 'auto-mode-alist '("\\.au3" . autoit-mode)))
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (when (load "csharp-mode" t)
;;   (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))

;;(require 'cl)
;;(add-hook 'sgml-mode-hook 'xslt-process-mode)
(add-hook 'xml-mode-hook 'xslt-process-mode)
(add-hook 'xsl-mode-hook 'xslt-process-mode)
(add-hook 'xsl-mode-hook 'turn-on-font-lock)
(defadvice xml-mode (after run-xml-mode-hooks act)
  "Invoke `xml-mode-hook' hooks in the XML mode."
  (run-hooks 'xml-mode-hook))

;; (setq auto-mode-alist
;;       (append
;;        (list
;;         '("\\.fo" . xsl-mode)
;;         '("\\.xsl" . xsl-mode))
;;        auto-mode-alist))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in reverse.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\S +" "\\&" beg end))

(defun all2clipboard () (interactive)
       (x-select-text (buffer-string)))

(defun highlight-other-window (point mark) (interactive "r")
       (save-window-excursion
         (let ((string (buffer-substring point mark)))
           (other-window 1)
           (remove-overlays)
           (end-of-buffer)
           (while (search-backward string nil t)
             (overlay-put
              (make-overlay (point) (+ (point) (length string)))
              'face 'highlight)))))

;;(when (require 'windows-path nil t) (windows-path-activate))
;; (defun windows-find-file (path)
;;   (interactive "sPath: ")
;;   (let* ((p1 (replace-regexp-in-string "\\\\" "/" path t t))
;;          (p2 (replace-regexp-in-string "\(.\):" "/cygdrive/\1" p1)))
;;     (find-file p2)))
(defun windows-find-file (path)
  (interactive "sPath: ")
  (let ((p (shell-command-to-string (concat "cygpath -u \"" path "\""))))
    (find-file (replace-regexp-in-string "\n\\'" "" p))))

(defun get-filename ()
  (or (dired-get-filename nil t)
      (buffer-file-name)
      default-directory))
(defun get-pathname ()
  (if (equal major-mode 'dired-mode)
      dired-directory
    (buffer-file-name)))

(defun pop-to-buffer-point-min (process event)
  (save-selected-window
    (pop-to-buffer (process-buffer process))
    (goto-char (point-min))))

(defun call-ediff (diff-fun ignore-fun)
  (add-hook 'ediff-quit-hook 'exit-recursive-edit)
  (while (<= (line-number-at-pos) (count-lines (point-min) (point-max)))
    (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
           (tab (string-match "\t" line))
           (path            (substring line 0  tab))
           (path2 (when tab (substring line (+ tab 1)))))
      (when (or (not ignore-fun) (not (funcall ignore-fun line)))
        (when (funcall diff-fun path path2)
          (recursive-edit))))
    (forward-line))
  (remove-hook 'ediff-quit-hook 'exit-recursive-edit))

(defun ediff-list () (interactive)
       (call-ediff 'ediff nil))

(org-clock-persistence-insinuate)
(remove-hook 'org-cycle-hook 'org-cycle-hide-drawers)
(eval-after-load 'org
  '(progn
     (setq org-level-faces
           (list
            (defface my-org-level-1 '((t :foreground "#e6194b")) "" :group 'org-faces)
            (defface my-org-level-2 '((t :foreground "#3cb44b")) "" :group 'org-faces)
            (defface my-org-level-3 '((t :foreground "#0082c8")) "" :group 'org-faces)
            (defface my-org-level-4 '((t :foreground "#f58231")) "" :group 'org-faces)
            (defface my-org-level-5 '((t :foreground "#911eb4")) "" :group 'org-faces)
            (defface my-org-level-6 '((t :foreground "#008080")) "" :group 'org-faces)
            (defface my-org-level-7 '((t :foreground "#aa6e28")) "" :group 'org-faces)
            (defface my-org-level-8 '((t :foreground "#800000")) "" :group 'org-faces)
            (defface my-org-level-9 '((t :foreground "#808000")) "" :group 'org-faces)
            (defface my-org-level-10 '((t :foreground "#000080")) "" :group 'org-faces)
            (defface my-org-level-11 '((t :foreground "#000000")) "" :group 'org-faces)
            (defface my-org-level-12 '((t :foreground "#808080")) "" :group 'org-faces)
            ))
     (setq org-n-level-faces (length org-level-faces)))
  )

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)

(defun utf-16-nullchar-space() (interactive)
       (let ((disptab (or (window-display-table) standard-display-table)))
         (unless disptab (set-window-display-table (make-display-table)))
         (aset disptab 0 [])))

(defun my-output (symbol)
  (if (boundp symbol)
      (princ (concat (prin1-to-string symbol) ":"
                     (prin1-to-string (eval symbol)) "\n"))
    (princ (concat (prin1-to-string symbol) " is undefined\n")))
  (message nil))

(defun my-complete-enable ()
  ;; (my-output 'item)
  ;; (my-output 'ido-cur-item)
  (not (and
        ;; (boundp 'ido-cur-item)
        ;; (eq ido-cur-item 'file)
        minibuffer-completing-file-name
        (string= "//" (substring default-directory 0 2))))
  )
;; (defun my-handle-remote ()
;;  (ido-mode (if (my-complete-enable) 1 -1)))
;;(add-hook 'minibuffer-setup-hook 'my-handle-remote)
;;(advice-add 'ido-active :before-while #'my-complete-enable)
(advice-add 'icomplete-minibuffer-setup :before-while #'my-complete-enable)
(when (require 'icomplete nil t)
  (define-key icomplete-minibuffer-map (kbd "<C-return>") 'icomplete-force-complete-and-exit)
  ;; (define-key icomplete-minibuffer-map (kbd "<M-return>") 'exit-minibuffer)
  ;; (define-key icomplete-minibuffer-map (kbd "<S-tab>") 'minibuffer-force-complete)
)
(fset 'yes-or-no-p 'y-or-n-p)

(defun yank-dospath () (interactive)
       (when (stringp (w32-get-clipboard-data))
         (insert (path-dos2unix (w32-get-clipboard-data)))))

(setq enable-dir-local-variables nil)

(defun silent-command (cmd)
  (interactive "sCommand: ")
  (call-process-shell-command cmd nil 0 t))
(defun cmd-exe ()
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
    (set-process-query-on-exit-flag proc nil)))

(defun open-in-eclipse ()
  (interactive)
  (let* ((exe "/cygdrive/c/Users/bernh/eclipse/java-2018-09/eclipse/eclipse")
         (path (file-name-nondirectory (buffer-name)))
         (cmd (concat exe " --launcher.openFile " path)))
    (shell-command cmd)))

(defun my-jsbeautify (buf)
  (interactive "i" ;;"bBuffer: \n"
               )
  (unless buf (setq buf (buffer-name)))
  (set-buffer buf)
  (let* ((dir default-directory)
        (tmp (make-temp-file "js-beautify"))
        (out "jsbeautify")
        (coding-system-for-read 'dos)
        (name (buffer-file-name))
        (type (cond ((string-match "\.html$" name) "html")
                    ((string-match "\.js$" name) "js")
                    (t (completing-read "Type: " '("js" "html"))))))
    (cd "~")
    (write-region (point-min) (point-max) tmp)
    (when (get-buffer out)
      (kill-buffer out))
    (call-process-shell-command (concat "js-beautify.cmd --type " type) tmp out)
    (cd dir)
    (ebuffers out (buffer-name))
    (delete-file tmp)))

(defun dired-sort-size ()
  (interactive)
  (dired-sort-other
   (if (string-match "S" dired-actual-switches)
       (replace-regexp-in-string "S" "" dired-actual-switches)
     (concat dired-actual-switches "S"))))

(when (require 'dired nil t)
  (define-key dired-mode-map [left] 'dired-jump)
  (define-key dired-mode-map [right] 'dired-view-file)
  (define-key dired-mode-map "z" 'dired-sort-size))

(add-hook
 'archive-mode-hook
 '(lambda ()
    (define-key archive-mode-map [left] 'dired-jump)
    (define-key archive-mode-map [right] 'archive-view)))
(add-hook
 'view-mode-hook
 '(lambda ()
    (define-key view-mode-map [left] 'View-kill-and-leave)
    (define-key view-mode-map "d" [left ?d ?v])))

(add-hook
 'image-mode-hook
 '(lambda()
    (define-key image-mode-map [left] 'image-kill-buffer)
    (define-key image-mode-map [return] 'pipe-picture)
    (define-key image-mode-map "n" [left ?n ?f])
    (define-key image-mode-map "p" [left ?p ?f])
    (define-key image-mode-map "d" [left ?d ?f])))

;;(global-set-key "\C-x\C-c" 'expunge-and-exit)

;; (global-set-key [S-down] '(lambda () (interactive) (scroll-up 1)))
;; (global-set-key [S-up] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [M-up] 'dired-jump)
(global-set-key [M-right] 'bs-cycle-previous)
(global-set-key [M-left] 'bs-cycle-next)

(define-key isearch-mode-map [f1] 'other-window)
(global-set-key [f1] 'other-window)
(global-set-key [C-f1] 'delete-other-windows)
(global-set-key [S-f1] 'delete-window)
(global-set-key [C-S-f1] 'utf-16-nullchar-space)
(global-set-key [M-f2] 'compile)
(global-set-key [f2] 'next-error)
(global-set-key [S-f2] 'previous-error)
(global-set-key [f3] 'bs-show)
(global-set-key [S-f3] 'all2clipboard)
(global-set-key [C-f3] 'yank-dospath)
(global-set-key [f4] 'recentf-open-files)
(global-set-key [C-f4] '(lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'start)
(global-set-key [S-f6] 'my-jsbeautify)
(global-set-key [C-f6] 'cmd-exe)
(global-set-key [f7] 'ffap)
(global-set-key [C-f7] 'ffap-other-window)
(global-set-key [f8] '(lambda () (interactive) (find-tag t t)))
(global-set-key [C-f8] 'imenu)
(global-set-key [S-f8] 'which-function-mode)
(global-set-key [f11] 'dabbrev-expand)
(global-set-key [f12] 'save-buffer)
(global-set-key [C-f12] 'save-some-buffers)
(global-set-key [(control /)] 'indent-region)
;;(global-set-key [(meta ?Q)] 'fill-line)
(global-set-key [(control return)] 'hippie-expand)
(global-set-key [kp-subtract] 'undo)
(global-set-key (kbd "C-<delete>") 'c-hungry-delete-forward)
(global-set-key (kbd "C-<backspace>") 'c-hungry-delete-backwards)
(define-key function-key-map [(control tab)] [?\M-\t])
(defun yank-pop-backwards ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-backwards)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [f9] 'open-in-eclipse)

(when (timeclock-currently-in-p) (timeclock-out))
(timeclock-in nil "dummy")
(recentf-mode 1)
(when (file-exists-p recentf-save-file) (recentf-open-files))
