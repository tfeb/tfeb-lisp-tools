;;;; Module loading.
;;;

(defpackage :org.tfeb.tools.require-module
  (:use :cl)
  (:export
   #:*module-path-descriptions*
   #:add-module-path-descriptions
   #:define-module-path-descriptions
   #:module-path-descriptions-for-function
   #:*module-component-separators*
   #:*module-component-rewriter*
   #:locate-module
   #:define-require-module-wrapper
   #:remove-require-module-wrapper
   #:require-module
   #:require-modules
   #:requires
   #:needs
   #:after-require-module
   #:clear-module-caches
   #:*module-providers*
   #:provide-module
   #:provides))

(in-package :org.tfeb.tools.require-module)

(defvar *module-path-descriptions*
  ;; The default list of (logical) path descriptions.  Each element
  ;; should have a wild name component with a single wildcard, into
  ;; which the name is spliced using "*" as a from-wildcard.  If it
  ;; does not have a wild name, then the name is used as the last
  ;; component of the directory only.
  '())

(defun ensure-pathname (d)
  ;; Ensure a pathname description is a pathname, or NIL
  (typecase d
    (null nil)
    (list (apply #'make-pathname d))
    (function (ensure-pathname (funcall d)))
    (symbol (ensure-pathname (funcall (symbol-function d))))
    (t (pathname d))))

(defun add-module-path-descriptions (descs &key (after nil)
                                           (before (not after))
                                           (uniquely t) (test #'equal)
                                           (host nil) (replace nil)
                                           (module-path-descriptions
                                            *module-path-descriptions*))
  ;; Add some module path descriptions to a path description list: by
  ;; default add them at the start, and only add them if they are not
  ;; already there.  Do not fash about the efficiency of this: you do
  ;; not have that many descriptions.  Returns the new path
  ;; description list.
  ;;
  ;; Note that this no longer modifies an existing list, which is a
  ;; big incompatible change, hidden away because
  ;; DEFINE-MODULE-PATH-DESCRIPTIONS now does this.
  (let ((mpds (if replace
                  (loop for d in module-path-descriptions
                        unless (equalp (pathname-host
                                        (ensure-pathname d))
                                       host)
                        collect d)
                (copy-list module-path-descriptions))))
    (if before
        (dolist (desc (reverse descs))
          (if uniquely
              (pushnew desc mpds :test test)
            (push desc mpds)))
      (dolist (desc descs)
        (when (or (not uniquely) (not (member desc mpds :test test)))
          (setf mpds (nconc mpds descs)))))
    mpds))

(defmacro define-module-path-descriptions (host/options &body descs)
  ;; Define module path descriptions for a single host.  This modifies
  ;; a path description list, which by default is
  ;; *MODULE-PATH-DESCRIPTIONS*: you can alter this with a keyword.
  (multiple-value-bind (host options) (if (consp host/options)
                                          (values (car host/options)
                                                  (cdr host/options))
                                        (values host/options '()))
    ;; Just pick out the bit of OPTIONS we care about, which is the
    ;; descriptions list name
    (destructuring-bind (&key (module-path-descriptions
                               '*module-path-descriptions*)
                              &allow-other-keys) options
      `(progn
         (setf ,module-path-descriptions
               (add-module-path-descriptions
                (list
                ,@(loop for d in descs
                        collect (typecase d
                                  (list
                                   (case (first d)
                                     (lambda d)
                                     (otherwise
                                      `'(:host ,host ,@d))))
                                  (t d))))
                :host ',host
                ,@options))
         ',host))))

#||
(define-module-path-descriptions "CLEY"
  (:directory (:absolute "LIB" "MODULES")
   :name "*-LOADER"
   :type "LISP")
  (:directory (:absolute "LIB" "MODULES")
   :name "LOADER"
   :type "LISP")
  (:directory (:absolute "LIB" "MODULES")
   :name "*"
   :type "LISP"))

(define-module-path-descriptions ("QL" :after t)
  "QL:LOCAL-PROJECTS;*-LOADER.LISP"
  "QL:LOCAL-PROJECTS;LOADER.LISP"
  "QL:LOCAL-PROJECTS;*.LISP")
||#

(defun module-path-descriptions-for-function (function
                                                    pathname-specs)
  ;; For each pathname spec make a functiony path description which
  ;; calls FUNCTION, and if does not return NIL then merges either the
  ;; result of a suitable MAKE-PATHNAME call (for a listy pathname
  ;; spec) or a call to PATHNAME (for any other pathname spec) with
  ;; its result.  If the function returns NIL just return NIL.
  (mapcar (lambda (ps)
            (typecase ps
              (list
               (lambda ()
                 (let ((p (funcall function)))
                   (if p
                       (merge-pathnames (apply #'make-pathname
                                               :defaults p
                                               ps)
                                        p)
                     nil))))
              (t
               (lambda ()
                 (let ((p (funcall function)))
                   (if p
                       (merge-pathnames (pathname ps) p)
                     nil))))))
          pathname-specs))

(defvar *module-component-separators*
  ;; Characters which separate module components
  '(#\.))

(defvar *module-component-rewriter*
  ;; if not NIL, a function designator which is used to rewrite module
  ;; components.
  nil)

(defun locate-module (module-name &key (module-path-descriptions
                                        *module-path-descriptions*)
                                  (hints '())
                                  (module-component-separators
                                   *module-component-separators*)
                                  (module-component-rewriter
                                   *module-component-rewriter*)
                                  (verbose nil)
                                  (debug nil))
  ;; MODULE-NAME is a string or symbol, maybe with . chars in.  This
  ;; function encodes the search order, which is hairy but basically
  ;; as follows (this is just a sample).
  ;; Component is :COM.CLEY.WELD, implied pathnames from
  ;; MODULE-PATH-DESCRIPTIONS CLEY:LIB;MODULES;*-LOADER.LISP and
  ;; CLEY:LIB;MODULES;LOADER.LISP.  Then search for the following paths,
  ;; (using upcase variants of the module name only)
  ;; (1a) CLEY:LIB;MODULES;COM;CLEY;WELD;WELD-LOADER.LISP,
  ;; (1b) CLEY:LIB;MODULES;COM;CLEY;WELD-LOADER.LISP,
  ;; (2a) CLEY:LIB;MODULES;COM;CLEY;WELD;LOADER.LISP,
  ;; (2b) CLEY:LIB;MODULES;WELD;WELD-LOADER.LISP,
  ;; (3) CLEY:LIB;MODULES;WELD-LOADER.LISP,
  ;; (4) CLEY:LIB;MODULES;WELD;LOADER.LISP,
  ;; (5) if there are hints try them last
  ;; For each of these, take the compiled file if it exists and is newer
  ;; (or if the source does not exist)
  ;; If it exists and is older take the source file.
  ;; In the above, (1a) and (1b) come from the first elt of
  ;; MODULE-PATH-DESCRIPTIONS, (2a) and (2b) come from the second, (3) comes
  ;; from the first ignoring all but the last part of the module namd, and (4)
  ;; comes from the second in the same way.
  ;;
  ;; If the elements of MODULE-PATH-DESCRIPTIONS do not specify
  ;; logical pathnames then for each try (each number above), try the
  ;; name as is, then downcased if different, then upcased if
  ;; different.  So in the above, we'd get for (1a) and (1b):
  ;; (1a.s) /cley/lib/modules/COM/CLEY/WELD/WELD-loader.lisp,
  ;; (1b.s) /cley/lib/modules/COM/CLEY/WELD-loader.lisp,
  ;; (1a.d) /cley/lib/modules/com/cley/weld/weld-loader.lisp,
  ;; (1b.d) /cley/lib/modules/com/cley/weld-loader.lisp,
  ;; (upcase versions are not different so are skipped)
  ;; .. and so on
  ;;
  ;; If the module name has no dots then the directoriless versios of
  ;; the searches are not different and are skipped.
  ;;
  ;; In any case duplicate probes are explicitly avoided, on the
  ;; grounds that touching the filesystem is more expensive than
  ;; almost anything.
  ;;
  ;; Yes, it's complicated.
  ;;
  ;; MODULE-PATH-DESCRIPTIONS is the list of module path descriptions,
  ;; with the default being *MODULE-PATH-DESCRIPTIONS*.  VERBOSE, if
  ;; true, makes it chatter about what it's doing.  DEBUG, if true
  ;; will cause it to note duplicate probes (this is really for
  ;; debugging path descriptons, not this function).
  ;;
  ;; Return 5 values: the file to load, the source file & its write
  ;; date, the compiled file and its write date.  If the file to load
  ;; it NIL then the other values will all be NIL; otherwise the file
  ;; to load will be the same as one of the other files, but only one
  ;; of them may exist, in which case the other pair of values will be
  ;; NIL.  'The file to load' is the newest of the source & compiled
  ;; files if both exist.  All pathnames returned are truenames by
  ;; virtue of having come from PROBE-FILE.
  (when verbose
    (format t "~&Looking for module ~S~%" module-name))
  (check-type module-name (or symbol string) "a symbol or string")
  (let* ((name (string module-name))
         (nlist (if (find-if (lambda (c)
                               (member c module-component-separators))
                             name)
                    (loop with len = (length name)
                          for c upfrom 0
                          for opos = 0 then (+ pos 1)
                          for pos = (or (and (<= opos len)
                                             (position-if
                                               (lambda (c)
                                                 (member c
                                                         module-component-separators))
                                               name :start opos))
                                        len)
                          until (> opos len)
                          collect (if module-component-rewriter
                                      (funcall module-component-rewriter
                                               (subseq name opos pos))
                                    (subseq name opos pos))
                          into results
                          finally (return results))
                  (list (if module-component-rewriter
                            (funcall module-component-rewriter name)
                          name))))
         (ndir (butlast nlist))
         (nname (first (last nlist)))
         (probed (make-hash-table :test #'equal)))
    (when (null nname)
      (error "Didn't get a name from ~A" module-name))
    (labels ((found (it source source-date compiled compiled-date)
               ;; Just return directly what we found
               (return-from locate-module
                 (values it source source-date compiled compiled-date)))
             (probe (pathlist from)
               ;; Find something that looks good to load in PATHLIST. Only
               ;; returns if it found nothing
               (dolist (path pathlist)
                 (when verbose
                   (format t "~&Probing ~A~@[~% as     ~A~]~% from ~:W~%"
                           path
                           (and (typep path 'logical-pathname)
                                (translate-logical-pathname path))
                           from))
                 (let* ((already (gethash path probed))
                        (p (and (not already) (probe-file path)))
                        (pt (and p (file-write-date p)))
                        (cp (probe-file (compile-file-pathname path)))
                        (cpt (and cp (file-write-date cp))))
                   (cond ((and pt cpt)  ;both files exist
                          (let ((got (if (> cpt pt) cp p)))
                            (when verbose
                              (format t "~&Found ~A~%" got))
                            (found got p pt cp cpt)))
                         (pt            ;only p exists
                          (when verbose
                            (format t "~&Found ~A~%" p))
                          (found p p pt cp cpt))
                         (cpt           ;only cp exists
                          (when verbose
                            (format t "~&Found ~A~%" cp))
                          (found cp p pt cp cpt))
                         (already
                          (when debug
                            (format *debug-io* "~&Evaded duplicate probe for ~A: ~A~%"
                                    module-name path)))
                         (t
                          (setf (gethash path probed) t))))))
             (merge-path (wild-path dirlist name)
               ;; Do the work of merging DIRLIST and NAME into
               ;; WILD-PATH Returns a list of merges (note!).  This
               ;; implements part of the search order: for foo.bar.zap
               ;; and .../modules/*-loader return
               ;; .../modules/foo/bar/zap/zap-loader,
               ;; .../modules/foo/bar/zap-loader; for foo.bar.zap and
               ;; ../modules/loader return .../modules/foo/bar/zap/loader
               ;; only.
               (if (wild-pathname-p wild-path)
                   (let ((results
                          (list (translate-pathname
                                 name "*"
                                 (merge-pathnames
                                  (make-pathname
                                   :host (pathname-host wild-path)
                                   :directory `(:relative ,@dirlist ,name))
                                  wild-path))
                                (translate-pathname
                                 name "*"
                                 (merge-pathnames
                                  (make-pathname
                                   :host (pathname-host wild-path)
                                   :directory `(:relative ,@dirlist))
                                  wild-path)))))
                     (unless (notany #'wild-pathname-p results)
                       (error
                        "One of the paths ~{~A~^, ~} is still wild after merging"
                        results))
                     results)
                 (let ((results
                        (list (merge-pathnames
                               (make-pathname
                                :host (pathname-host wild-path)
                                :directory `(:relative ,@dirlist ,name))
                               wild-path))))
                   (when (wild-pathname-p (first results))
                     (error
                      "Path ~A is still wild after merging" (first results)))
                   results))))
      ;; Note the loops below are escaped from if PROBE finds
      ;; what it's looking for.
      ;;
      ;; If there are hints, try them
      (probe hints ':hints)
      ;; Then look for the whole path along all the elements
      (dolist (d module-path-descriptions)
        (let ((path (ensure-pathname d)))
          (typecase path
            (logical-pathname
             ;; consider only upcase variants
             (probe (merge-path path (mapcar #'string-upcase ndir)
                                (string-upcase nname))
                    d))
            (pathname
             ;; Consider: thing, thing downcased if different, thing
             ;; upcased if different (yes, this is just a hardwired
             ;; order)
             (probe (merge-path path ndir nname) d)
             (let ((ndir-downcase (mapcar #'string-downcase ndir))
                   (nname-downcase (string-downcase nname)))
               (unless (and (equal ndir-downcase ndir)
                            (string= nname-downcase nname))
                 (probe (merge-path path ndir-downcase nname-downcase) d)))
             (let ((ndir-upcase (mapcar #'string-upcase ndir))
                   (nname-upcase (string-upcase nname)))
               (unless (and (equal ndir-upcase ndir) (string= nname-upcase nname))
                 (probe (merge-path path ndir-upcase nname-upcase) d))))
            (null)                      ;d was function returned nil
            (t                          ;what?
             (warn "unexpected 'pathname' ~A (type ~S)" path (type-of path))))))
      ;; Now search ignoring the dir components, if there were any
      (unless (null ndir)
        (dolist (d module-path-descriptions)
          (let ((path (ensure-pathname d)))
            (typecase path
              (logical-pathname
               ;; consider only upcase variants
               (probe (merge-path path '() (string-upcase nname)) d))
              (pathname
               ;; Consider: thing, thing downcased if different, thing
               ;; upcased if different (yes, this is just a hardwired
               ;; order)
               (probe (merge-path path '() nname) d)
               (let ((nname-downcase (string-downcase nname)))
                 (unless (string= nname-downcase nname)
                   (probe (merge-path path '() nname-downcase) d)))
               (let ((nname-upcase (string-upcase nname)))
                 (unless (string= nname-upcase nname)
                   (probe (merge-path path '() nname-upcase) d))))
              (null)
              (t
               (warn "unexpected 'pathname' ~A (type ~S)" path (type-of path)))))))
      ;; Failed
      (found nil nil nil nil nil))))

(defvar *require-module-wrappers*
  ;; A list of (name . function) where the functions wrapped around a
  ;; module being required.  Each function must take at least two
  ;; arguments.  The first is a function of no arguments: it should
  ;; call this function to do the work of requiring the module and
  ;; return its value.  The second is the name of the module being
  ;; required. See DEFINE-MODULE-WRAPPER.  Additional arguments are
  ;; passed from REQUIRE-MODULE and should therefore probably be
  ;; optional.
  '())

(defmacro define-require-module-wrapper (name (next module . args)
                                              &body decls/forms)
  ;; Define or redefine a named module wrapper.  I don't like it that
  ;; the first argument is entirely magic.
  (let ((next-fn (make-symbol (concatenate 'string (symbol-name name) "-NEXT"))))
    (multiple-value-bind (decls forms)
        (loop for tail on decls/forms
              for form = (first tail)
              while (eq (car form) 'declare)
              collect form into declarations
              finally (return (values declarations tail)))
      `(progn
         (setf (cdr (or (assoc ',name *require-module-wrappers*)
                        (car (push '(,name) *require-module-wrappers*))))
               (lambda (,next-fn ,module ,@args)
                 ,@decls
                 (block ,name
                   (flet ((,next () (funcall ,next-fn)))
                     (declare (inline ,next))
                     ,@forms))))
         ',name))))

(defun remove-require-module-wrapper (name)
  ;; Remove a wrapper: return true  it was there
  (if (member name *require-module-wrappers* :key #'car)
      (progn
        (setf *require-module-wrappers*
              (delete name *require-module-wrappers* :key #'car))
        t)
    nil))

#+LispWorks
(define-require-module-wrapper forget-lw-systems (next module
                                                  &key (forget-systems t)
                                                  &allow-other-keys)
  ;; A wrapper which will forget LW systems.  This is here both as an
  ;; example, and because it needs to be here before the very first
  ;; module is required if that module is an LW system.
  (declare (ignore module))
  (if forget-systems
      (let ((systems (scm:all-systems)))
        (unwind-protect
            (next)
          (dolist (new (set-difference (scm:all-systems) systems
                                       :key #'scm:module-name))
            (lw:delete-system new))))
    (next)))

(defun maybe-use-module-package (m &key (use nil) (verbose nil) (quiet nil))
  (when use
    (let ((p (find-package m)))
      (if p
          (let ((pn (package-name p)))
            (when verbose
              (format t "~&Using package ~S~:[ (~S)~;~*~]"
                      m (string= (string m) pn) pn))
            (use-package p))
        (unless quiet
          (warn "No package for ~S" m))))))

(eval-when (:compile-toplevel)
  ;; I am not sure this has to be true, although I expect it is
  (let ((c (compile-file-pathname *compile-file-pathname*)))
    (unless (equal c (compile-file-pathname c))
      (warn
       "COMPILE-FILE-PATHNAME is not idempotent: dynamic compilation may fail"))))

(defvar *write-date-cache*
  ;; maps truenames to write dates
  (make-hash-table :test #'equal))

(defvar *module-descendants*
  ;; maps modules to all their descendant modules (doing this means
  ;; more entries but saves building the graph)
  (make-hash-table :test #'equal))

(defvar *ambient-arguments*
  ;; REQUIRE-MODULE binds this to all its explicit keyword arguments,
  ;; so recursive incantations can use the same values.
  '())

(defvar *ambient-modules*
  ;; All the parent modules of the module being processed
  '())

(defun clear-module-caches ()
  (clrhash *write-date-cache*)
  (clrhash *module-descendants*)
  (values))

(defun require-module (m &rest arguments &key
                         (verbose (getf *ambient-arguments* ':verbose nil))
                         (debug (getf *ambient-arguments* ':debug nil))
                         (quiet (getf *ambient-arguments* ':verbose nil))
                         (test (getf *ambient-arguments* ':string #'string=))
                         (pretend (getf *ambient-arguments* ':pretend nil))
                         (force (getf *ambient-arguments* ':force nil))
                         (once (getf *ambient-arguments* ':once t))
                         (cache (getf *ambient-arguments* ':cache t))
                         (reload nil)   ;not ambient
                         (compile (getf *ambient-arguments* ':compile nil))
                         (use nil)      ;not ambient
                         (fallback (getf *ambient-arguments* ':fallback nil))
                         (error (getf *ambient-arguments* ':fallback t))
                         (module-path-descriptions
                          (getf *ambient-arguments*
                                ':module-path-descriptions
                                *module-path-descriptions*))
                         (hints '())    ;not ambient
                         (module-component-separators
                          (getf *ambient-arguments*
                                ':module-component-separators
                                *module-component-separators*))
                         (module-component-rewriter
                          (getf *ambient-arguments*
                                ':module-component-rewriter
                                *module-component-rewriter*))
                         (wrapper-arguments (getf *ambient-arguments*
                                                  ':wrapper-arguments '())))
  ;; Require a module, using LOCATE-MODULE to find it and invoking any
  ;; wrappers.
  (when debug
    (format *debug-io* "~&module    ~S~%~
                        ~@[within    ~S~%~]~
                        arguments ~:S~%~
                        ambient   ~:S~%"
            m *ambient-modules* arguments *ambient-arguments*))
  (check-type m (or symbol string) "a symbol or string")
  (when (or force reload)
    ;; This is horrible but it avoids having to second-guess REQUIRE
    (setf *modules* (remove (string m) *modules* :test test)))
  (if (member (string m)
              *modules*
              :test test)
      (progn
        (maybe-use-module-package m :use use :verbose verbose :quiet quiet)
        (values m nil))
    (multiple-value-bind (location source source-date compiled compiled-date)
        (locate-module m
                       :module-path-descriptions module-path-descriptions
                       :hints hints
                       :module-component-rewriter module-component-rewriter
                       :verbose verbose :debug debug)
      (assert (or (not location)
                  (and (eql location compiled) compiled-date)
                  (and (eql location source) source-date))
          (location source source-date compiled compiled-date)
        "Oops: LOCATE-MODULE's values make no sense")
      (when debug
        (format *debug-io* "~&location  ~S~%~
                            source    ~S~%          (~S)~%~
                            compiled  ~S~%          (~S)~%"
                location source source-date compiled compiled-date))
      (when (and source (not (null *ambient-modules*)))
        ;; Only displace any hint if we found a source (FASL is no
        ;; good as path descriptions should always refer to sources),
        ;; and if there are ambient modules.  But displace all of them
        ;; in that case.
        (let ((the-hints (list source)))
          (dolist (p *ambient-modules*)
            (let ((found (assoc (string m) (gethash p *module-descendants*)
                                :test #'string=)))
              (if found
                  (setf (cdr found) the-hints)
                (push (cons (string m) the-hints)
                      (gethash p *module-descendants*)))))))
      (let ((*load-verbose* verbose)
            (*load-print* verbose)
            (*compile-verbose* verbose)
            (*compile-print* verbose)
            (*ambient-arguments* (append arguments *ambient-arguments*))
            (*ambient-modules* (cons (string m) *ambient-modules*))
            (did-load nil))
        (cond
         (location
          ;; found a location
          (if (not pretend)
              (labels
                  ((wrapping-require (wtail)
                     (if (null wtail)
                         (multiple-value-bind (to-load to-load-date)
                             (cond
                              ((eql location compiled) ;just load it
                               (values location compiled-date))
                              (compile  ;source but asked to compile
                               (multiple-value-bind (cf warnings-p failed-p)
                                   (compile-file location)
                                 (declare (ignore warnings-p))
                                 (if (not failed-p)
                                     (values cf (file-write-date cf))
                                   (progn
                                     (warn "Compiling ~A failed: using source"
                                           location)
                                     (values location source-date)))))
                              ((not compiled) ;no compiled file: don't warn
                               (values location source-date))
                              (t        ;out of date compiled file: do warn
                               (unless quiet
                                 (warn "Source ~A is newer than bin ~A, using source"
                                       location compiled))
                               (values location source-date)))
                           (let ((last-write-date (gethash to-load
                                                           *write-date-cache*)))
                             (when debug
                               (format *debug-io* "~&chosen    ~S~%          (~S)~%"
                                       to-load last-write-date))
                             (if (or force
                                     (not once)
                                     (not last-write-date)
                                     (> to-load-date last-write-date))
                                 (progn
                                   (when debug
                                     (format *debug-io* "~&          [loading]~%"))
                                   (when verbose
                                     (format t "~&Loading ~S from ~A"
                                         m to-load))
                                   (require m to-load)
                                   (setf did-load t)
                                   (when cache
                                     (setf (gethash location *write-date-cache*)
                                           to-load-date)))
                               (progn
                                 (when debug
                                   (format *debug-io* "~&          [already]~%"))
                                 (when verbose
                                   (format t "~&Module ~S from ~A already loaded~%"
                                           m location)))))
                           (maybe-use-module-package
                            m :use use :verbose verbose :quiet quiet))
                       (progn
                         (when verbose
                           (format t
                                   "~&Wrapper ~S" (car (first wtail))))
                         (apply (cdr (first wtail))
                                (lambda ()
                                  (wrapping-require (rest wtail)))
                                m
                                wrapper-arguments)))))
                (wrapping-require *require-module-wrappers*))
            (format t "~&Would load ~S from ~A" m location))
          (when reload
            (dolist (module/hint (reverse (gethash (string m) *module-descendants*)))
              (when debug
                (format *debug-io* "~&Reloading ~A (hint ~A) because ~A~%"
                        (car module/hint) (cdr module/hint) m))
              ;; Remaining arguments will come from ambient
              (require-module (car module/hint) :hints (cdr module/hint))))
          (values m did-load))
       (fallback
        ;; No location but there's a fallback: just call it, and
        ;; assume it loaded the module
       (when verbose
          (format t "~&Trying fallback for ~S" m))
       (let ((result (funcall fallback m)))
         (maybe-use-module-package m :use use :verbose verbose :quiet quiet)
         (values result t)))
       (error
        ;; Signal an error
        (error "No location found for ~S" m))
       (t
        ;; No error, just fail
        (values nil nil)))))))

(defun require-modules (module-specifications
                        &rest keywords &key &allow-other-keys)
  ;; Require a list of module specifications.  Return a list of list
  ;; of the values returned by REQUIRE-MODULE for each module.  A
  ;; module specification is either the name of a module or a cons of
  ;; (module name . keyword-arguments)
  (loop for m in module-specifications
        collect (multiple-value-list
                 (if (atom m)
                     (apply #'require-module m keywords)
                   (apply #'require-module (first m)
                          (append (rest m) keywords))))))

(defun requires (&rest module-specifications)
  ;; NOSPREAD version of require-modules, with a fallback to REQUIRE
  (require-modules module-specifications :fallback #'require))

(defmacro needs (&rest module-specifications)
  ;; What do we need?  Things we need are always needed.
  ;; Note this quotes its arguments, which is an incompatible change
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (requires ,@(loop for m in module-specifications
                       collect `',m))))

;;; Hooks which can be run after modules are required
;;;

(defvar *after-require-module-hooks*
  ;; Hooks run after a module is loaded (will be backwards)
 '())

(defvar *in-require-module*
  ;; Are we being required?
  nil)

(defmacro after-require-module (&body forms)
  "Run FORMS after this module is provided

A block called AFTER-REQUIRE-MODULE is wrapped around them."
  `(progn
     (when *in-require-module*
       (push (lambda ()
               (block after-require-module
                 ,@forms))
             *after-require-module-hooks*))
     (values)))

(define-require-module-wrapper after-require-module
    (next module
          &key (run-after-hooks t) &allow-other-keys)
    (declare (ignore module))
  (let ((*after-require-module-hooks* '())
        (*in-require-module* t))
    (multiple-value-prog1
        (next)
      (when run-after-hooks
        (dolist (f (nreverse *after-require-module-hooks*))
          (funcall f))))))

;;; Module provider recording
;;;

(defvar *module-providers*
  ;; an alist of (module . provider) added to by PROVIDE-MODULE
 '())

(defun provide-module (module &optional (provider *load-truename*))
  (prog1
      (provide module)
    (when provider
      (let ((entry (or (assoc (string module) *module-providers*
                              :test #'string=)
                       (car (push (list (string module))
                                  *module-providers*)))))
        (setf (cdr entry) provider)))))

(defun provides (&rest module-names)
  ;; Counterpart to NEEDS: modules aren't provided until the file is
  ;; loaded however
  (dolist (m module-names module-names)
    (provide-module m)))

;;; Bootstrap
;;;

(provides :org.tfeb.tools.require-module)

(pushnew ':org.tfeb.tools.require-module *features*) ;help init files
