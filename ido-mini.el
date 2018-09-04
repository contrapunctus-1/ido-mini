;;;; Hark, fatuous serfs in the lands of Helm and Ido who know naught
;;;; of what's possible!
;;;; <dramatic pause>
;;;; <imperiously> Install `ido-mini' in your init.el.

;;;; helm-mini's informatively-colored candidates list and convenience
;;;; of a single command to swich to buffers, open recentf files, and
;;;; create new buffers, and ido's smarter sorting and snappier speed.

;;;; Plus, some improvements of its own -
;;;; 1. If a recentf entry is already visited by a buffer, put the
;;;;    entry at the end of the candidates list.
;;;; 2. (temporarily disabled) tweak `im/use-paths' to search and
;;;;    display not just buffer names but also their file paths,
;;;;    wherever applicable.
;;;; 3. (planned) show documentation of functions from smex, akin to
;;;;    C-j in helm-M-x

;;;; No fifty commands to do the work of one, no uninformatively
;;;; colorless candidates list, no helm slowing down your init or
;;;; hanging your Emacs, and no more [Display not ready]!

;;;; IMPORTANT
;;;; Key customization is sadly NOT done the usual way in Ido.
;;;; Instead, you write a function to change keybindings (you want to
;;;; use the `ido-completion-map') and add that to `ido-setup-hook'.
;;;; See section 'Customization' in (find-library "ido")

;;;; thanks to wilfredh for the initial code that got me started -
;;;; https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d

;; TODO
;; 1. When called twice in succession, quit ido-mini
;; 2. color the matched substring in the candidates?
;; 3. store only search terms in input history, not the selected
;;    buffer names/file paths
;; 4. Mimic exact C-j (`ido-select-text') and RET behaviour
;;    (`ido-exit-minibuffer')
;; 5. What if we search for files (perhaps only in user-specified
;;    directories, when provided) when there are no matches in the
;;    buffer list as well as in recentf?

;; add variables to
;; - toggle displaying paths with buffer names

;;;; VARIABLES ----

(defgroup ido-mini nil
  "Variables for ido-mini completion library.")

(defface im/unsaved-buffer
  '((t (:foreground "orange")))
  "Face used by `ido-mini' to indicate buffers which have
been edited but not saved."
  :group 'ido-mini)
(defface im/buffer-changed
  '((t (:foreground "red" :background "black")))
  "Face used by `ido-mini' to indicate buffers whose file has
been edited outside Emacs."
  :group 'ido-mini)
(defface im/buffer-file-missing
  '((t (:foreground "Indianred2")))
  "Face used by `ido-mini' to indicate buffers whose file does
not exist on disk."
  :group 'ido-mini)

(defcustom im/use-paths nil
  "If non-nil, display file paths of the associated files of
buffers, where applicable (see function `im/add-paths').
Additionally, completion will search for buffer names as well as
their file paths.

Users may also find it useful to set this to nil and to enable
buffer uniquifying via `toggle-uniquify-buffer-names'.")

(defcustom im/buffer-list-functions '(im/buffer-names
                                      im/buffers-clean
                                      im/buffers-bury-visible
                                      im/buffers-color)
  "List of functions run sequentially over the output
of `(buffer-list)', with the result of one being the input of the
next (using `->list'). Each should accept exactly one argument.
The resulting list is used by `ido-mini' for completion
candidates.")

(defcustom im/recentf-list-functions '(im/recentf-bury-visited
                                       im/recentf-color)
  "List of functions run sequentially over `recentf-list', with
the result of one being the input of the next (using `->list').
Each should accept exactly one argument. The resulting list is
used by `ido-mini' for completion candidates.")

;;;; LIBRARY FUNCTIONS ----

(defun ->list (var functions)
  "Assuming FUNCTIONS is a list of functions (FN1 FN2 FN3),
return the result of (FN3 (FN2 (FN1 VAR)))"
  ;; (--reduce-from (funcall it acc) var functions)
  (dolist (fn functions)
    (setq var (funcall fn var)))
  var)

(defun im/buffer-names (&optional buffer-list)
  "Return a list of the name of all buffers returned
by (buffer-list)."
  (let ((buffer-list (if buffer-list buffer-list (buffer-list))))
    (-map 'buffer-name buffer-list)))
(defun im/buffers-clean (buffer-names)
  "Remove buffers whose names start with a space."
  (cl-flet ((leading-space-p
             (el)
             (string-match-p "^ " el)))
    (-remove #'leading-space-p buffer-names)))
(defun im/buffers-bury-visible (buffer-names)
  "Put visible buffers at the end of list, in reverse order of
  appearance (e.g. (vb1 b1 vb2 b2 ...) becomes (b1 b2 ... vb2
  vb1))"
  (let* ((bufs-wins-alist (-zip buffer-names
                                (-map 'get-buffer-window-list
                                      buffer-names)))
         (bufs-with-wins  (-filter 'cdr bufs-wins-alist))
         (bufs-wo-wins    (-remove 'cdr bufs-wins-alist)))
    (append
     (-map 'car bufs-wo-wins)
     (reverse (-map 'car bufs-with-wins)))))
(defun im/buffers-color (buffer-names)
  (-map
   (lambda (buffer-name)
     (let ((buffer (get-buffer buffer-name)))
       (cond
        ((let ((bfn (buffer-file-name buffer)))
           (and bfn (not (file-exists-p bfn))))
         (propertize buffer-name 'face 'im/buffer-file-missing))
        ;; buffers with unsaved files
        ((and (buffer-file-name buffer)
              (buffer-modified-p buffer))
         (propertize buffer-name 'face 'im/unsaved-buffer))
        ;; buffer modified outside emacs
        ((not (verify-visited-file-modtime buffer))
         (propertize buffer-name 'face 'im/buffer-changed))
        ;; buffers with files
        ((buffer-file-name buffer)
         (propertize buffer-name 'face 'font-lock-type-face))
        ;; dired buffers
        ((with-current-buffer buffer
           (derived-mode-p 'dired-mode))
         (propertize buffer-name 'face 'dired-directory))
        ;; TODO make this light gray
        ((string-match-p "^\\*" buffer-name)
         (propertize buffer-name 'face 'italic))
        (t buffer-name))))
   buffer-names))

(defun im/recentf-bury-visited (recentf)
  "Returns the contents of `recentf-list', with files being
visited by a buffer placed at the end of the list."
  (append
   (-remove #'get-file-buffer recentf)
   (-filter #'get-file-buffer recentf)))
(defun im/recentf-color (recentf)
  "Color recentf-list."
  (-map (lambda (el) (propertize el 'face 'ido-virtual))
        recentf))

(defun im/select-text (ido-choice)
  "Mimic superficial behaviour of `ido-select-text'. If there is
an exact match for the search string, select it, else create a
new buffer using the search string as the name.")
(defun im/exit-minibuffer (ido-choice)
  "Mimic superficial behaviour of `ido-exit-minibuffer'. If there
is any match for the search string, select it, else print
[Confirm] and after another RET, create a buffer using the search
string as the name.")
(defun im/save-search-string ()
  "Save search string from ido-completing-read into history.")

;; (ido-completing-read "Buffer:"
;;                      (append (->list (buffer-list)
;;                                      im/buffer-list-functions)
;;                              (->list recentf-list
;;                                      im/recentf-list-functions)))

;;;; IDO-MINI ----

(defun ido-mini ()
  "A helm-mini replacement using Ido. Switch to a buffer or a
recentf entry with ido. If `im/use-paths' is non-nil,
search for and display the whole file path instead of just the
file name.

Using ido-vertical in conjunction may be beneficial.

It can be extended conveniently - see the variables
`im/buffer-list-functions' and `im/recentf-list-functions'.

Based off code from wilfredh -
https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d"
  (interactive)
  (let*
      ((buffers-sorted         (->list (buffer-list)
                                       ;; '(im/buffer-names
                                       ;;   im/buffers-clean
                                       ;;   im/buffers-bury-visible)
                                       im/buffer-list-functions
                                       ))
       (recentf-sorted         (im/recentf-bury-visited recentf-list))
       (buffer-count           (length buffers-sorted))

       (processed-buffers      (->list (buffer-list)
                                       im/buffer-list-functions))
       (processed-recentf      (->list recentf-list
                                       im/recentf-list-functions))
       (candidates             (append processed-buffers
                                       processed-recentf))
       (ido-choice             (completing-read "Switch to buffer: "
                                                candidates
                                                nil nil nil
                                                'ido-buffer-history))
       (chosen-index           (-elem-index ido-choice candidates)))
    ;; if chosen-index is nil, we create a buffer with that name
    (if chosen-index
        ;; is the chosen candidate in the buffer-list or recentf-list?
        (if (< chosen-index buffer-count)
            (switch-to-buffer (nth chosen-index buffers-sorted))
          (find-file (nth (- chosen-index buffer-count)
                          recentf-sorted)))
      (switch-to-buffer (get-buffer-create ido-choice)))))
