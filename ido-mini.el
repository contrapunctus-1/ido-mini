
;; ;; thanks to wilfredh -
;; ;; https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d

;; (defun wh/ido-switch-buffer-with-filename ()
;;   "Switch to a buffer with ido, including the filename in the prompt."
;;   (interactive)
;;   (let* ((bufs (buffer-list))
;;          (bufs-with-paths
;;           (--map (with-current-buffer it
;;                    (if (buffer-file-name)
;;                        (format "%s <%s>" (buffer-name) (buffer-file-name))
;;                      (buffer-name)))
;;                  bufs))
;;          (chosen-index
;;           (-elem-index
;;            (ido-completing-read
;;             "Switch to buffer: " bufs-with-paths)
;;            bufs-with-paths)))
;;     (switch-to-buffer (nth chosen-index bufs))))

;; (defun im/buffer-dc-alist ()
;;   "Return an alist of buffer names and buffer-display-count
;; values."
;;   (-zip (im/buffer-names)
;;         (--map (with-current-buffer it
;;                  buffer-display-count)
;;                (buffer-list))))
;; (defun im/buffer-dc-alist-sorted ()
;;   "Return alist created by `im/buffer-dc-alist', sorted by
;;   buffer-display-count values."
;;   (--sort (> (cdr it) (cdr other))
;;           (im/buffer-dc-alist)))
;; (defun im/buffer-list-dc-sorted ()
;;   "Like `buffer-list' but sorted by buffer-display-count
;;   values."
;;   (-map 'car (im/buffer-dc-alist-sorted)))


;; TODO - make all operations on buffer list and recentf-list -
;; sorting, addition of paths, coloring into their own single
;; functions, and the functions user-definable via "-function"
;; variables.

;; TODO - colors!
;; - dired buffers                  - major-mode
;; - unsaved buffers                - buffer-modified-p
;; - buffers modified outside emacs - verify-visited-file-modtime
;; - buffers with deleted files     - buffer-file-name -> file-exists-p
;; - and maybe the matched substring in the candidates

;; TODO - clean input history, do not store file paths
;; TODO - C-j to create buffer with the search string as name.

;; add variables to
;; - toggle displaying paths with buffer names

(defface im/unsaved-file
  '((t :inherit font-lock-type-face)))

(defvar im/use-paths nil
  "If non-nil, display file paths of the associated files of
buffers, where applicable (see function `im/add-paths').
Additionally, completion will search for buffer names as well as
their file paths.

Users may also find it useful to set this to nil and to enable
buffer uniquifying via `toggle-uniquify-buffer-names'.")
(defvar im/buffer-list-functions nil
  "List of functions run sequentially over the output
of `(buffer-list)', with the result of one being the input of the
next (using `->list'). Each should accept exactly one argument.")
(defvar im/recentf-list-functions nil
  "List of functions run sequentially over `recentf-list', with
the result of one being the input of the next (using `->list').
Each should accept exactly one argument.")


(defun im/buffer-names ()
  "Return a list of the name of all buffers returned
by (buffer-list)."
  (-map 'buffer-name (buffer-list)))
(defun ->list (var functions)
  "Assuming FUNCTIONS is a list of functions (FN1 FN2 FN3),
return the result of (FN3 (FN2 (FN1 VAR)))"
  (dolist (fn functions)
    (setq var (funcall fn var)))
  var)
;; (->list 1 '((lambda (a) (+ a 1))
;;             (lambda (a) (+ a 2))
;;             (lambda (a) (+ a 3)))) ;; => 7

(defun im/prep-buffer-list ()
  "Return a list of buffer names using `buffer-list', processed
the way ido-switch-buffer does."
  (cl-flet ((leading-space-p
             (el)
             (string-match-p "^ " el)))
    (let* ((bufs (im/buffer-names))
           ;; Remove buffers whose names start with a space
           (bufs-main       (-remove #'leading-space-p
                                     bufs))
           (bufs-space      (-filter #'leading-space-p
                                     bufs))
           ;; Put visible buffers at the end of list, in reverse order of appearance
           ;; (e.g. (vb1 b1 vb2 b2 ...) becomes (b1 b2 ... vb2 vb1))
           (bufs-wins-alist (-zip bufs-main
                                  (-map 'get-buffer-window-list
                                        bufs-main)))
           (bufs-with-wins  (-filter 'cdr bufs-wins-alist))
           (bufs-wo-wins    (-remove 'cdr bufs-wins-alist)))
      (append
       (-map 'car bufs-wo-wins)
       (reverse (-map 'car bufs-with-wins))))))
(defun im/recentf-list ()
  "Returns the contents of `recentf-list', with files being
visited by a buffer placed at the end of the list."
  (append
   (-remove #'get-file-buffer recentf-list)
   (-filter #'get-file-buffer recentf-list)))
(defun im/add-paths (&optional buflist)
  "Add paths to a list of buffer names. If BUFLIST is nil or
omitted, use output from im/prep-buffer-list."
  (let ((bufs (if buflist buflist (im/prep-buffer-list))))
    (if im/use-paths
        (--map (with-current-buffer it
                 (if (buffer-file-name)
                     (format "%s <%s>" (buffer-name) (buffer-file-name))
                   (buffer-name)))
               (im/prep-buffer-list))
      bufs)))
(defun im/color-recentf ()
  "Color output from im/recentf-list."
  (-map (lambda (el)
          (propertize el 'face 'ido-virtual))
        ;; recentf-list
        (im/recentf-list)))
(defun im/color-buffer-names ()
  ""
  (-map (lambda (buffer)
          (if (buffer-modified-p)
              (propertize buffer 'face 'ido-indicator)))))

;; (defun im/clean)


(defun ido-mini ()
  "A helm-mini replacement using Ido. Switch to a buffer or a
recentf entry with ido. If `im/use-paths' is non-nil,
search for and display the whole file path instead of just the
file name.

Using ido-vertical in conjunction may be beneficial.

Based off code from wilfredh -
https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d"
  (interactive)
  (let*
      ((bufs                 (im/prep-buffer-list))
       (bufs-with-paths      (im/add-paths bufs))
       (recentf-list-colored (im/color-recentf))
       (bufs-and-recentf     (append bufs-with-paths
                                     recentf-list-colored))
       (chosen-index
        (-elem-index (ido-completing-read "Switch to buffer: "
                                          bufs-and-recentf
                                          nil nil nil
                                          'ido-buffer-history)
                     bufs-and-recentf)))

    ;; is the chosen candidate in the buffer-list or recentf-list?
    (if (< chosen-index (length bufs))
        (switch-to-buffer (nth chosen-index
                               bufs))
      (find-file (nth (- chosen-index (length bufs))
                      (im/recentf-list))))))

;; pipeline

;;                      recentf-list  -> sorted recentf -> color
;;                                                             \
;;                                                            combine -> ido-completing-read -> select from sorted buffer/recentf list
;;                                                            /
;; (buffer-list) -> buffer names -> sort -> add paths -> color
;;                       |           |         |
;;                (im/buffer-names)  |  (im/add-paths)
;;                        (im/prep-buffer-list)
