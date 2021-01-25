;;;; Module loading.
;;;

(defpackage :org.tfeb.tools.require-module
  (:use :cl)
  (:export
   #:*module-path-descriptions*
   #:add-module-path-descriptions
   #:define-module-path-descriptions
   #:locate-module
   #:define-require-module-wrapper
   #:remove-require-module-wrapper
   #:require-module
   #:require-modules
   #:after-require-module
   #:*module-providers*
   #:provide-module))

(in-package :org.tfeb.tools.require-module)

(defvar *module-path-descriptions*
  ;; The default list of (logical) path descriptions.  Each element
  ;; should have a wild name component with a single wildcard, into
  ;; which the name is spliced using "*" as a from-wildcard.  If it
  ;; does not have a wild name, then the name is used as the last
  ;; component of the directory only.
  '())

(defun ensure-pathname (d)
  ;; Ensure a pathname description is a pathname
  (typecase d
    (list (apply #'make-pathname d))
    (function (ensure-pathname (funcall d)))
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

(defun locate-module (module-name &key (module-path-descriptions
                                        *module-path-descriptions*)
                                  (verbose nil))
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
  ;; (4) CLEY:LIB;MODULES;WELD;LOADER.LISP.
  ;; For each of these, take the compiled file if it exists and is newer
  ;; (or if the source does not exist)
  ;; If it exists and is older take the source file and warn.
  ;; In the above, (1a) and (1b) come from the first elt of
  ;; MODULE-PATH-DESCRIPTIONS, (2a) and (2b) come from the second, (3) comes
  ;; from the first ignoring all but the last part of the module namd, and (4)
  ;; comes from the second in the same way.
  ;; If the elements of MODULE-PATH-DESCRIPTIONS do not specify logical
  ;; pathnames then for each try (each number above), try the name as is,
  ;; then downcased, then upcased.  So in the above, we'd get for (1a) and (1b):
  ;; (1a.s) /cley/lib/modules/COM/CLEY/WELD/WELD-loader.lisp,
  ;; (1b.s) /cley/lib/modules/COM/CLEY/WELD-loader.lisp,
  ;; (1a.d) /cley/lib/modules/com/cley/weld/weld-loader.lisp,
  ;; (1b.d) /cley/lib/modules/com/cley/weld-loader.lisp,
  ;; (1a.u) /cley/lib/modules/COM/CLEY/WELD/WELD-loader.lisp,
  ;; (1b.u) /cley/lib/modules/COM/CLEY/WELD-loader.lisp
  ;; .. and so on
  ;;
  ;; Yes, it's complicated.
  ;;
  ;; MODULE-PATH-DESCRIPTIONS is the list of module path descriptions,
  ;; with the default being *MODULE-PATH-DESCRIPTIONS*.  VERBOSE, if
  ;; true, makes it chatter about what it's doing.
  ;;
  (when verbose
    (format t "~&Looking for module ~S~%" module-name))
  (let* ((name (etypecase module-name
                 (string module-name)
                 (symbol (symbol-name module-name))))
         (nlist (if (find #\. name)
                    (loop with len = (length name)
                          for c upfrom 0
                          for opos = 0 then (+ pos 1)
                          for pos = (or (and (<= opos len)
                                             (position #\. name :start opos))
                                        len)
                          until (> opos len)
                          collect (subseq name opos pos) into results
                          finally (return results))
                  (list name)))
         (ndir (butlast nlist))
         (nname (first (last nlist))))
    (when (null nname)
      (error "Didn't get a name from ~A" module-name))
    (labels ((probe (pathlist from)
               ;; Find something that looks good to load in PATH
               (loop for path in pathlist
                     for found =
                     (progn
                       (when verbose
                         (format t "~&Probing ~A~@[~% as     ~A~]~% from ~:W~%"
                                 path
                                 (and (typep path 'logical-pathname)
                                      (translate-logical-pathname path))
                                 from))
                       (let* ((p (probe-file path))
                              (pt (and p (file-write-date p)))
                              (cp (probe-file (compile-file-pathname path)))
                              (cpt (and cp (file-write-date cp))))
                         (cond ((and pt cpt)
                                (let ((got
                                       (if (> cpt pt)
                                           cp
                                         (progn
                                           (warn
                                            "Loader source ~A is newer than bin ~A, loading source"
                                            p cp)
                                           p))))
                                  (when verbose
                                    (format t "~&Found ~A~%" got))
                                  got))
                               (pt
                                (when verbose
                                  (format t "~&Found ~A~%" p))
                                p)
                               (cpt
                                (when verbose
                                  (format t "~&Found ~A~%" cp))
                                cp)
                               (t nil))))
                     when found return found
                     finally (return nil)))
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
      ;; First look for the whole path along all the elements
      (loop for d in module-path-descriptions
            for path = (ensure-pathname d)
            for found =
            (typecase path
              (logical-pathname
               ;; consider only upcase variants
               (probe (merge-path
                       path
                       (mapcar #'string-upcase ndir)
                       (string-upcase nname))
                      d))
              (pathname
               ;; Consider: thing, thing downcased, thing upcased
               ;; (yes, this is just a hardwired order)
               (or (probe (merge-path path ndir nname) d)
                   (probe (merge-path
                           path
                           (mapcar #'string-downcase ndir)
                           (string-downcase nname))
                          d)
                   (probe (merge-path
                           path
                           (mapcar #'string-upcase ndir)
                           (string-upcase nname))
                          d))))
            when found do (return-from locate-module found))
      ;; Now search ignoring the dir components
      (loop for d in module-path-descriptions
            for path = (ensure-pathname d)
            for found =
            (typecase path
              (logical-pathname
               ;; consider only upcase variants
               (probe (merge-path path '() (string-upcase nname)) d))
              (pathname
               ;; Consider: thing, thing downcased, thing upcased
               ;; (yes, this is just a hardwired order)
               (or (probe (merge-path path '() nname) d)
                   (probe (merge-path
                           path '() (string-downcase nname))
                          d)
                   (probe (merge-path
                           path '() (string-upcase nname))
                          d))))
            when found do (return-from locate-module found))
      ;; Or give up
      nil)))

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

(eval-when (:compile-toplevel)
  ;; I am not sure this has to be true, although I expect it is
  (let ((c (compile-file-pathname *compile-file-pathname*)))
    (unless (equal c (compile-file-pathname c))
      (warn
       "COMPILE-FILE-PATHNAME is not idempotent: dynamic compilation may fail"))))

(defun require-module (m &rest keywords &key
                         (verbose nil)
                         (test #'string=)
                         (pretend nil)
                         (compile nil)
                         (error t)
                         (module-path-descriptions *module-path-descriptions*)
                         &allow-other-keys)
  ;; Require a module, using LOCATE-MODULE to find it and invoking any
  ;; wrappers.
  (if (member (etypecase m
                (string m)
                (symbol (symbol-name m)))
              *modules*
              :test test)
      (values m nil)
    (let ((location (locate-module m
                                   :module-path-descriptions module-path-descriptions
                                   :verbose verbose)))
      (unless location
        (if error
            (error "No location found for ~S" m)
          (return-from require-module (values nil nil))))
      (if (not pretend)
          (let ((wrapper-arguments
                 (if (not (null keywords))
                     (loop for (k v . more) = keywords then more
                           unless (member k '(:verbose :test
                                              :pretend :compile
                                              :error
                                              :module-path-descriptions))
                           collect k and collect v
                           until (null more))
                   '())))
            (labels
                ((wrapping-require (wtail)
                   (if (null wtail)
                       (let ((effective-location
                              (if compile
                                  (let ((cf (compile-file-pathname location)))
                                    (when (not (equal cf location))
                                      (compile-file location
                                                    :output-file cf
                                                    :verbose verbose
                                                    :print verbose))
                                    cf)
                                location)))
                         (when verbose
                           (format t "~&Loading ~S from ~A"
                                   m effective-location))
                         (require m effective-location))
                     (progn
                       (when verbose
                         (format t
                                 "~&Wrapper ~S" (car (first wtail))))
                       (apply (cdr (first wtail))
                              (lambda ()
                                (wrapping-require (rest wtail)))
                              m
                              wrapper-arguments)))))
              (wrapping-require *require-module-wrappers*)))
        (format t "~&Would load ~S from ~A" m location))
      (values m t))))

(defun require-modules (ms &rest keywords &key &allow-other-keys)
  ;; Require a list of modules.  Return a list of list of the values
  ;; returned by REQUIRE-MODULE for each module
  (loop for m in ms
        collect (multiple-value-list (apply #'require-module m keywords))))

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

;;; Bootstrap
;;;

(provide-module :org.tfeb.tools.require-module)

(pushnew ':org.tfeb.tools.require-module *features*) ;help init files
