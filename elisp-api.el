(:name "Number"
       :functions (fixnump
                   bignump
                   floatp
                   integerp
                   numberp
                   natnump
                   zerop
                   isnan
                   eql
                   =
                   eq
                   /=
                   <
                   <=
                   >
                   >=
                   max
                   min
                   abs
                   frexp
                   ldexp
                   copysign
                   logb
                   float
                   truncate
                   floor
                   ceiling
                   round
                   1+
                   1-
                   +
                   -
                   *
                   /
                   %
                   mod
                   ffloor
                   fceiling
                   ftruncate
                   fround
                   ash
                   lsh
                   logand
                   logior
                   lognot
                   logcount
                   sin
                   cos
                   tan
                   asin
                   acos
                   atan
                   exp
                   log
                   expt
                   sqrt
                   random))

(:name "Character"
       :functions (char-equal
                   char-to-string
                   string-to-char))

(:name "String"
       :functions (stringp
                   string-or-null-p
                   char-or-string-p
                   make-string
                   string
                   substring
                   substring-no-properties
                   concat
                   split-string
                   store-substring
                   clear-string
                   char-equal
                   string=
                   string-equal
                   string-collate-equalp
                   string<
                   string-lessp
                   string-collate-lessp
                   string-version-lessp
                   string-prefix-p
                   string-suffix-p
                   compare-strings
                   string-distance
                   assoc-string
                   number-to-string
                   string-to-number
                   char-to-string
                   string-to-char
                   format
                   format-message
                   downcase
                   upcase
                   capitalize
                   upcase-initials))

(:name "Symbol"
       :description "A symbol is an object with a unique name, it has 4 components: a name, a variable value, a function definition and a property list."
       :functions (symbolp
                   make-symbol
                   gensym
                   intern
                   intern-soft
                   mapatoms
                   unintern

                   symbol-name
                   symbol-value
                   symbol-function

                   ;; plist
                   get
                   put
                   setplist
                   symbol-plist))


(:name "Cons Cell, Atom and List"
       :functions (consp
                   atom
                   listp
                   nlistp
                   null
                   proper-list-p

                   car
                   cdr
                   car-safe
                   cdr-safe
                   pop
                   nth
                   nthcdr
                   last
                   safe-length
                   caar
                   cadr
                   cdar
                   cddr
                   butlast
                   nbutlast

                   ;; building cons cells and lists
                   cons
                   list
                   make-list
                   append
                   copy-tree
                   flatten-tree
                   number-sequence
                   push
                   add-to-list
                   add-to-ordered-list
                   setcar
                   setcdr
                   nconc
                   memq
                   delq
                   remq
                   member
                   delete
                   remove
                   member-ignore-case
                   delete-dups
                   assoc
                   rassoc
                   assq
                   alist-get
                   rassq
                   assoc-default
                   copy-alist
                   assq-delete-all
                   assoc-delete-all
                   rassq-delete-all
                   plist-get
                   plist-put
                   lax-plist-get
                   lax-plist-put
                   plist-member))

(:name "Sequence"
       :description "The “sequence” type is the union of two other Lisp types: lists and arrays.

4 types of arrays: vectors, strings, char-tables and bool-vectors.

The common property that all sequences have is that each is an ordered collection of elements."
       :functions (sequencep
                   length
                   elt
                   copy-sequence
                   reverse
                   nreverse
                   sort
                   seq-elt
                   seq-length
                   seqp
                   seq-drop
                   seq-take
                   seq-take-while
                   seq-drop-while
                   seq-do
                   seq-map
                   seq-map-indexed
                   seq-mapn
                   seq-filter
                   seq-remove
                   seq-reduce
                   seq-some
                   seq-find
                   seq-every-p
                   seq-count
                   seq-sort
                   seq-sort-by
                   seq-set-equal-p
                   seq-position
                   seq-uniq
                   seq-subseq
                   seq-concatenate
                   seq-mapcat
                   seq-partition
                   seq-intersection
                   seq-group-by
                   seq-into
                   seq-min
                   seq-max
                   seq-doseq
                   seq-let
                   seq-random-elt))

(:name "Array"
       :functions (arrayp
                   aref
                   aset
                   fillarray))

;; vector (setq foo [1 two])
(:name "Vector"
       :functions (vectorp
                   vector
                   make-vector
                   vconcat))

(:name "Char-Table"
       :functions (make-char-table
                   char-table-p
                   char-table-subtype
                   char-table-parent
                   set-char-table-parent
                   char-table-extra-slot
                   set-char-table-extra-slot
                   char-table-range
                   set-char-table-range
                   map-char-table
                   ))

(:name "Bool-Vector"
       :functions (make-bool-vector
                   bool-vector
                   bool-vector-p
                   bool-vector-exclusive-or
                   bool-vector-intersection
                   bool-vector-set-difference
                   bool-vector-not
                   bool-vector-subsetp
                   bool-vector-count-consecutive
                   bool-vector-count-population))

;; A ring is a fixed-size data structure that supports insertion, deletion, rotation, and modulo-indexed reference and traversal.
;; Rings like the kill ring and the mark ring are implemented as simple lists, not using the ring package
(:name "Ring"
       :functions (make-ring
                   ring-p
                   ring-size
                   ring-length
                   ring-elements
                   ring-copy
                   ring-empty-p
                   ring-ref
                   ring-insert
                   ring-remove
                   ring-insert-at-beginning
                   ring-resize))

(:name "Hash Table"
       :functions (make-hash-table
                   gethash
                   puthash
                   remhash
                   clrhash
                   maphash
                   define-hash-table-test
                   sxhash-equal
                   sxhash-eq
                   sxhash-eql
                   hash-table-p
                   copy-hash-table
                   hash-table-count
                   hash-table-test
                   hash-table-weakness
                   hash-table-rehash-size
                   hash-table-rehash-threshold
                   hash-table-size))

;; (:name "Record"
;;        :description "The purpose of records is to allow programmers to create objects with new types that are not built into Emacs."
;;        :functions ())

(:name "Control Structure"
       :functions (progn
                   prog1
                   prog2

                   ;; Conditional
                   if
                   when
                   unless
                   cond
                   pcase

                   not
                   and
                   or
                   xor

                   while
                   dolist
                   dotimes))

(:name "Control Structure - Generators"
       :description "A “generator” is a function that produces a potentially-infinite stream of values.

Each time the function produces a value, it suspends itself and waits for a caller to request the next value."
       :functions (iter-defun
                    iter-lambda
                    iter-yield
                    iter-yield-from
                    iter-next
                    iter-close
                    iter-do))

;; (:name "Control Structure - error handling")

(:name "Keymaps"
       :functions (kbd
                   make-sparse-keymap
                   make-keymap
                   copy-keymap

                   keymap-parent
                   set-keymap-parent
                   make-composed-keymap

                   define-prefix-command
                   current-active-maps
                   key-binding

                   current-global-map
                   current-local-map
                   current-minor-mode-maps
                   use-global-map
                   use-local-map
                   set-transient-map

                   lookup-key
                   local-key-binding
                   global-key-binding
                   minor-mode-key-binding

                   define-key
                   substitute-key-definition
                   suppress-keymap

                   command-remapping

                   global-set-key
                   global-unset-key
                   local-set-key
                   local-unset-key

                   accessible-keymaps
                   map-keymap
                   where-is-internal
                   describe-bindings)
       :variables (global-map))

(:name "Documentation"
       :functions (documentation-property
                   documentation
                   face-documentation
                   Snarf-documentation

                   substitute-command-keys

                   key-description
                   single-key-description
                   text-char-description
                   read-kbd-macro

                   apropos
                   describe-prefix-bindings)
       :variables (doc-directory))

(:name "Files"
       :functions (find-file
                   find-file-literally
                   find-file-noselect
                   find-file-other-window
                   find-file-read-only

                   create-file-buffer
                   after-find-file

                   save-buffer
                   save-some-buffers
                   write-file

                   insert-file-contents
                   insert-file-contents-literally

                   append-to-file
                   write-region

                   with-temp-file

                   file-locked-p
                   lock-buffer
                   unlock-buffer
                   ask-user-about-lock

                   file-exists-p
                   file-readable-p
                   file-executable-p
                   file-writable-p
                   file-accessible-directory-p
                   access-file
                   file-ownership-preserved-p
                   file-modes

                   file-symlink-p
                   file-directory-p
                   file-regular-p

                   file-truename
                   file-chase-links
                   file-equal-p
                   file-name-case-insensitive-p
                   file-in-directory-p
                   vc-responsible-backend

                   file-newer-than-file-p
                   file-attributes
                   file-nlinks

                   file-acl
                   file-selinux-context
                   file-extended-attributes

                   locate-file
                   executable-find

                   add-name-to-file
                   rename-file
                   copy-file
                   make-symbolic-link
                   delete-file
                   set-file-modes
                   set-default-file-modes
                   default-file-modes
                   read-file-modes
                   file-modes-symbolic-to-number
                   set-file-times
                   set-file-extended-attributes
                   set-file-selinux-context
                   set-file-acl

                   ;; file name components
                   file-name-directory
                   file-name-nondirectory
                   file-name-sans-versions
                   file-name-extension
                   file-name-sans-extension
                   file-name-base

                   file-name-absolute-p
                   file-relative-name

                   file-name-as-directory
                   directory-name-p
                   directory-file-name
                   abbreviate-file-name

                   expand-file-name
                   substitute-in-file-name
                   file-name-quote
                   file-name-unquote
                   file-name-quoted-p

                   make-temp-file
                   make-temp-name
                   make-nearby-temp-file
                   temporary-file-directory

                   ;; file name completion
                   file-name-all-completions
                   file-name-completion

                   locate-user-emacs-file
                   convert-standard-filename

                   directory-files
                   directory-files-recursively
                   locate-dominating-file
                   directory-files-and-attributes
                   file-expand-wildcards
                   insert-directory

                   make-directory
                   make-empty-file
                   copy-directory
                   delete-directory

                   find-file-name-handler
                   file-local-copy
                   file-remote-p
                   unhandled-file-name-directory
                   file-local-name)
       :variables (default-directory))

(:name "Buffers"
       :functions (bufferp
                   current-buffer
                   set-buffer
                   save-current-buffer
                   with-current-buffer
                   with-temp-buffer

                   ;; names
                   buffer-name
                   rename-buffer
                   get-buffer
                   generate-new-buffer-name

                   ;; buffer file name
                   buffer-file-name
                   get-file-buffer
                   find-buffer-visiting
                   set-visited-file-name

                   ;; modification
                   buffer-modified-p
                   set-buffer-modified-p
                   restore-buffer-modified-p
                   not-modified
                   buffer-modified-tick
                   buffer-chars-modified-tick
                   with-silent-modifications

                   ;; modification time
                   verify-visited-file-modtime
                   clear-visited-file-modtime
                   visited-file-modtime
                   set-visited-file-modtime
                   ask-user-about-supersession-threat

                   ;; read-only buffers
                   read-only-mode
                   barf-if-buffer-read-only

                   ;; buffer list
                   buffer-list
                   other-buffer
                   last-buffer
                   bury-buffer
                   unbury-buffer

                   ;; creating buffers
                   get-buffer-create
                   generate-new-buffer

                   ;; killing
                   kill-buffer

                   buffer-live-p

                   ;; indirect buffers
                   make-indirect-buffer
                   clone-indirect-buffer
                   buffer-base-buffer

                   ;; swapping
                   buffer-swap-text

                   ;; gap: make insertion/deletion faster
                   gap-position
                   gap-size)

       :variables (buffer-file-name
                   buffer-file-truename
                   buffer-file-number

                   buffer-read-only
                   inhibit-read-only

                   buffer-list-update-hook

                   kill-buffer-query-functions
                   kill-buffer-hook
                   buffer-offer-save
                   buffer-save-without-query))

(:name "Windows"
       :functions (windowp
                   window-live-p
                   window-valid-p
                   selected-window
                   selected-window-group

                   ;; frames
                   window-frame
                   window-list
                   frame-root-window
                   window-parent
                   window-top-child
                   window-left-child
                   window-child
                   window-combined-p
                   window-next-sibling
                   window-prev-sibling
                   frame-first-window
                   window-at-side-p
                   window-in-direction
                   window-tree

                   ;; sizes
                   window-total-height
                   window-total-width
                   window-total-size
                   window-pixel-height
                   window-pixel-width
                   window-full-height-p
                   window-full-width-p
                   window-body-height
                   window-body-width
                   window-body-size
                   window-mode-line-height
                   window-header-line-height
                   window-max-chars-per-line
                   window-min-size

                   ;; resizing
                   window-resizable-p
                   window-resize
                   adjust-window-trailing-edge
                   fit-window-to-buffer
                   fit-frame-to-buffer
                   shrink-window-if-larger-than-buffer
                   balance-windows
                   balance-windows-area
                   maximize-window
                   minimize-window

                   ;; preserving window sizes
                   window-preserved-size

                   ;; splitting
                   split-window
                   split-window-right
                   split-window-below

                   ;; deleting
                   delete-window
                   delete-windows-on

                   ;; recombining windows
                   set-window-combination-limit
                   window-combination-limit

                   ;; selecting
                   select-window
                   save-selected-window
                   with-selected-window
                   frame-selected-window
                   set-frame-selected-window
                   window-use-time

                   ;; cyclic ordering
                   next-window
                   previous-window
                   other-window
                   walk-windows
                   one-window-p
                   get-lru-window
                   get-mru-window
                   get-largest-window
                   get-window-with-predicate

                   ;; buffers and windows
                   window-buffer
                   set-window-buffer
                   get-buffer-window
                   get-buffer-window-list
                   replace-buffer-in-windows

                   ;; switching
                   switch-to-buffer
                   switch-to-buffer-other-window
                   switch-to-buffer-other-frame
                   pop-to-buffer

                   ;; display
                   display-buffer
                   display-buffer-same-window
                   display-buffer-reuse-window
                   display-buffer-reuse-mode-window
                   display-buffer-pop-up-window
                   display-buffer-in-previous-window
                   display-buffer-use-some-window
                   display-buffer-in-direction
                   display-buffer-below-selected
                   display-buffer-at-bottom
                   display-buffer-pop-up-frame
                   display-buffer-in-child-frame
                   display-buffer-no-window

                   split-window-sensibly

                   ;; window history
                   window-prev-buffers
                   set-window-prev-buffers
                   window-next-buffers
                   set-window-next-buffers
                   switch-to-prev-buffer
                   switch-to-next-buffer

                   ;; dedicated windows
                   window-dedicated-p
                   set-window-dedicated-p

                   ;; quiting
                   quit-window
                   quit-restore-window

                   ;; side windows
                   display-buffer-in-side-window
                   window-main-window
                   window-toggle-side-windows

                   ;; atom windows
                   window-atom-root
                   window-make-atom
                   display-buffer-in-atom-window

                   ;; window and point
                   window-point
                   set-window-point

                   ;; positions
                   window-start
                   window-group-start
                   window-end
                   window-group-end
                   set-window-start
                   set-window-group-start
                   pos-visible-in-window-p
                   pos-visible-in-window-group-p
                   window-line-height

                   ;; scrolling
                   scroll-up
                   scroll-down
                   scroll-up-command
                   scroll-down-command
                   scroll-other-window
                   scroll-other-window-down
                   recenter
                   recenter-window-group
                   recenter-top-bottom

                   ;; vertical fractional scrolling
                   window-vscroll
                   set-window-vscroll

                   ;; horizontal scrolling
                   scroll-left
                   scroll-right
                   window-hscroll
                   set-window-hscroll

                   ;; coordinates
                   window-edges
                   window-body-edges
                   window-at
                   coordinates-in-window-p
                   window-pixel-edges
                   window-body-pixel-edges
                   window-absolute-pixel-edges
                   window-absolute-pixel-position
                   window-largest-empty-rectangle

                   ;; configurations
                   current-window-configuration
                   set-window-configuration
                   save-window-excursion
                   window-configuration-p
                   compare-window-configurations
                   window-configuration-frame
                   window-state-get
                   window-state-put
                   window-swap-states

                   ;; parameters
                   window-parameter
                   window-parameters
                   set-window-parameter

                   set-frame-window-state-change
                   frame-window-state-change
                   window-old-buffer
                   window-old-pixel-width
                   window-old-pixel-height
                   window-old-body-pixel-width
                   window-old-body-pixel-height
                   frame-old-selected-window
                   old-selected-window
                   old-selected-frame)

       :variables (buffer-display-count
                   buffer-display-time))

;; (:name "Scripting"
;;        :functions ()
;;        :variables (command-line-args command-line-args-left))
