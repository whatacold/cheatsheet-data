;; format: (category (api1 api2))
("Number" (fixnump
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

("Character" (char-equal
              char-to-string
              string-to-char))

("String" (stringp
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

;; A “symbol” is an object with a unique name.
("Symbol" (symbolp
           symbol-name
           make-symbol
           gensym
           intern
           intern-soft
           mapatoms
           unintern
           get
           put
           symbol-plist
           setplist
           symbol-value
           symbol-function))

("Cons Cell, Atom and List" (consp
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

;; sequence
;; - list
;; - array
;;   - vector
;;   - string
;;   - char-table
;;   - bool-vector

("Sequence" (sequencep
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

("Array" (arrayp
          aref
          aset
          fillarray))

;; vector (setq foo [1 two])
("Vector" (vectorp
           vector
           make-vector
           vconcat))

("Char-Table" (make-char-table
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

("Bool-Vector" (make-bool-vector
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
("Ring" (make-ring
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

("Hash Table" (make-hash-table
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

; ("Scripting" (command-line-args command-line-args-left))
