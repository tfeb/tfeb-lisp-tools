# [TFEB.ORG Lisp tools](https://github.com/tfeb/tfeb-lisp-tools "TFEB.ORG Lisp tools")
This repo contains a system which will be a collection of fairly miscellaneous Common Lisp tools, which I have written over the years in order to generally get stuff done.  Here, a *tool* is a thing which helps with building programs, loading modules and similar things, rather than something you might use *in* a program which would be a [hack](../tfeb-lisp-hax/ "TFEB.ORG Lisp hax").

- `require-module` provides variants of `require` which will search for modules, as well as the mechanisms to control the search, and also a variant of `provide` which keeps records of the file which provided a module;
- `install-providers` makes use of the records of module providers kept by `require-module` in order to copy them to places they will be found.
- `build-modules` provides a way of compiling a collection of single-file modules, using `require-module` to locate their sources.
- `feature-expressions` provides some tools for reasoning about implementation features after read time.
- `deprecations` provides tools for marking functions, generic functions and macros as deprecated, causing an optional compile-time warning and allowing reporting and mapping over deprecated things.

I hope to add more tools as I disentangle them from the things they're currently entangled with and modernise them where needed.  All of the tools are intended to be portable CL except where documented.

The descriptions here are at best partial[^1].

## General
### Portability
These tools purport to be portable Common Lisp.  If they're not that's either a bug in the tools or a bug in the CL implementation.  In the former case I very definitely want to know, and I am also willing to add workarounds for the latter although I may be unable to test them on implementations I don't use.

### Zero history
All of these tools have long and varied histories.  However these histories are entangled with a lot of other code which is not public, so that history is not represented in the publication repo where you are probably reading this.

### Versions
I planned to use [semantic versioning](https://semver.org/ "Semantic versioning"), where the major version number only changes on incompatible changes.  However the result of this would be that a complete new tool being added or a huge extension to an existing one would only be a minor version, which seems wrong.  So what I'm doing is to modify this: a major version means either an incompatible change, a complete new tool, or a very major extension of an existing tool.

### Naming conventions
All of these tools make use of, and often work best with, *domain-structured names*: packages, modules, features and so on have names which start with a DNS domain name in reverse order and then may continue to divide further.  In this case the prefix is `org.tfeb.tools`: `org.tfeb` is the DNS component and `tools` is the division within the DNS part.

This naming convention ensures uniqueness of things like package names.  It does make typing package-qualified names somewhat irritating: the solution is not to do that but to construct packages which import or use the symbols you need.  CL's default package system is very well up to solving this problem, rumours to the contrary notwithstanding: [conduit packages](https://github.com/tfeb/conduit-packages "conduit packages") can make it a little simpler to express what you want to do, and [UIOP's `define-package`](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fPACKAGE "define-package") can also do something similar, I think.

Nothing actually cares that names correspond to real DNS domains: some things do care that the namespace is hierarchically structured and big-endian.

---

## Requiring modules with searching: `require-module`
Although CL now has competent tools for defining and distributing 'systems' of code, in the form of [ASDF](https://common-lisp.net/project/asdf/ "ASDF") and particularly [Quicklisp](https://www.quicklisp.org/ "Quicklisp"), not all bits of code are large enough to justify their use.  In particular it's in the nature of the incremental and  exploratory programming style encouraged by CL that people[^2] tend to write a bunch of little tools and utilities, living in single source files, which help them get their work done and which they reuse over time.  Some of these may be portable, some not, and many of them can be quite small: a tool I use very often is under 50 lines excluding noise, but including docstrings and comments, and I have had smaller ones.

Turning these small tools into ASDF systems[^3] either means combining them into some larger conglomeration of code such as this one, or doubling the number of files you have to deal with because of the system definition files for each small tool.  Combining them into a larger conglomeration of code means everything takes longer and costs more; doubling the number of files means the same.  Also, of course, the ancestor of `require-module` predates the common availability of portable system definition tools by many years.

Well, CL has `require`, `provide` and `*modules*` and although these are deprecated, they're not going away any time soon, or in fact ever.  So a reasonable approach is to make all the small tools into modules by adding suitable `provide` incantations, and then to use `require` to load them.  This still leaves the problem that `require` needs to know where to find the module being required is in the filesystem, and I didn't want to fill code with explicit pathnames, even though logical pathnames make things at least a little better.

`require-module` provides a souped-up variant of `require` which has a portable mechanism for searching for and locating files corresponding to modules.  In particular it understands how to combine domain-structured module names (`org.tfeb.hax.collecting` say) with a search list of pathnames to locate a file corresponding to the name which, when loaded, should provide the module.  There are mechanisms to define what directories should be searched and in which order, and to wrap code around the process of requiring a module in the manner of CLOS around methods.  Finally it also provides a wrapper for `provide` which keeps a record of the file which provided a given module, which can be used by `install-providers` (below) to install modules where it expects them to be.

`require-module` can also take advantage of the fact that `require` never actually checks that the file or files it loads provides the module being required.  So `require-module` will happily load any file it can find, and doesn't care whether the module is provided or not.

`require-module` also keeps a cache of the truenames and write dates of the files it has loaded: unless you ask to bypass the cache, files will only ever be loaded once until they are modified, no matter how many times they are required.

Finally, `require-module` also keeps a structure which maps between modules it has loaded and all the descendant modules of them – modules which were loaded in the process of loading some other module – together with a note as to the file they came from.  Using this structure you can ask it to reload a module if it has changed together with any descendant modules which have changed.  This is currently experimental.

### Logical pathnames
`require-module` doesn't need you to use logical pathnames, but it does work well with them and I have usually used it that way until recently.  Using logical pathnames gives you two advantages:

- the mechanism for maintaining the search list lets you add & replace a bunch of paths for the same host, and if that's a logical host you can invent new ones, so maintenance becomes easier;
- there is another level of translation between the names of modules and the physical pathname they come from – for instance by changing the translations of a logical host you can switch between variants of things easily.

In summary you don't have to use logical pathnames, but it can make things easier.

### Case
Module names are case sensitive: `require`uses `string=` to compare them.  Depending on how you write module names this can make life a little complicated[^4].  This is made more interesting because [logical pathnames are really all upper-case](https://www.lispworks.com/documentation/HyperSpec/Body/19_ca.htm "logical pathname syntax").  `require-module` tries to be smart about this:

- if it is checking a logical pathname as a potential location it uppercases module names;
- if it is checking a physical pathname then it first tries the name as it is, then downcased, then upcased, where those variants differ.

This typically works reasonably well: if you use module names which are symbols, are searching a physical pathname, are using a case-sensitive filesystem and prefer lowercase filenames, then it will find a lowercase filename on the second try.

### The search algorithm
The search process starts by splitting up a domain-structured module name into a list of strings (the set of characters which separate components can be controlled: see below), which it will then variously use as directory and name components for the search:

- `"org.tfeb.hax"` is turned into `("org" "tfeb" "hax")`;
- `:org.tfeb.hax` is turned into `("ORG" "TFEB" "HAX")` assuming standard reader settings.

These lists, and parts of them, are then used as parts of pathname specifications after possible case fiddling.

There is a list of path specifications to search: see below for how the list is maintained and exactly what can be in it.  A path specification should generally specify a wild or partly-wild lisp source file name.  Each path specification is searched in turn, and finally a desperation search is done on all of them using only the last part of the module name.  The search continues until either a hit is found or there's nothing left to search.

**Example: searching for `:org.tfeb.tools.require-module` in `"CLEY:LIB;MODULES;*.LISP"`.** This is a logical path specification, so only upper-case variants will be tried.  Both the compiled file (via `compile-file-pathname`) and the source file for the following pathnames will be tried[^5]: if both exist then the compiled file will be taken if it is newer, otherwise the source file (with a warning):

1. `"CLEY:LIB;MODULES;ORG;TFEB;TOOLS;REQUIRE-MODULE;REQUIRE-MODULE.LISP"`;
2. `"CLEY:LIB;MODULES;ORG;TFEB;TOOLS;REQUIRE-MODULE.LISP"`;
3. (all the other path specifications are searched the same way);
4. `"CLEY:LIB;MODULES;REQUIRE-MODULE;REQUIRE-MODULE.LISP"`(desperation search, part 1);
5. `"CLEY:LIB;MODULES;REQUIRE-MODULE.LISP"` (desperation search, part 2).

**Example: searching for `"org.tfeb.HAX"` in `"/local/lisp/*-loader.lisp"`.**  This is a physical path specification so things are more complicated:

1. `"/local/lisp/org/tfeb/hax/HAX-loader.lisp"`(case as is);
2. `"/local/lisp/org/tfeb/HAX-loader.lisp"` (case as is);
3. `"/local/lisp/org/tfeb/hax/hax-loader.lisp"` (downcase 1);
4. `"/local/lisp/org/tfeb/hax-loader.lisp"` (downcase 2);
5. `"/local/lisp/ORG/TFEB/HAX/HAX-loader.lisp"` (upcase 1);
6. `"/local/lisp/ORG/TFEB/HAX-loader.lisp"` (upcase 2);
7. (all the other path specifications);
8. (equivalent desperation searches, trying downcase and upcase variants as before, a total of 6 more searches).

In fact the search algorithm is cleverer than this in several ways:

- if downcasing or upcasing a module name results in no change from something already probed, it is not probed again;
- for module name has only one component (`"foo"` say) then the desperation search using the last part of the name only is omitted as a whole, as it would all duplicate the previous search;
- finally, a table of pathnames probed in the current search is kept, and if a probe would be a duplicate – for instance because the search list has duplicate entries – it is simply omitted, with a possible debugging note.

None of these tricks should alter which file is found for a module: they are all intended just to eliminate duplicate probes of the filesystem.

### Package, module, feature
Everything below is exported from `org.tfeb.tools.require-module`.  `:org.tfeb.tools.require-module` will also be present in both `*modules*` and `*features*` once this module is loaded – the latter helps a lot with conditionals in init files.

### Requiring and locating modules
**`require-module`** will search for and `require` modules.  It has one mandatory argument which is the module name, and a fairly large number of keyword arguments.  Values for most of the keyword arguments are dynamically passed down[^6], so that recursive calls to `require-module` will get the same values as the parent call: exceptions are noted below.  The keywords and their default values are as follows.

- `verbose` will cause it to tell you what it's doing (on `*standard-output*`), default `nil`.
- `debug` will turn on some debugging output to `*debug-io*`, and in particular will cause `locate-module` to talk about evaded duplicate searches due to the search list.  Default `nil`.
- `quiet` will cause some warnings not to happen (`quiet` and `verbose` can both be true).  Default `nil`.
- `test` specifies the comparison function to use for module names, by default `#'string=` which is the right default.
- `pretend` will cause it not to actually require the module, default `nil`.
- `force` will cause it to forcibly require the module (by removing it from `*modules*`as the first step), default `nil`.
- `once` will cause it to check the cache of loaded truenames and only load a file if it either has not been loaded previously or if its write date is newer than the cached date, default `t`.
- `cache` will update the cache of loaded truenames when a file is loaded, default `t`.
- `reload` will cause it to reload any dependent modules if possible.  Default is `nil` & this is not inherited from the ambient value.  Note that all dependent modules will be reloaded: not just direct children.
- `compile` will cause it to attempt to compile the module if it gets a source file name, default `nil`.
- `use` will cause it to use a package with the same name as the module after it is loaded if it exists, default `nil`.  This is not inherited from the ambient value.
- `fallback` , if given, should be a final fallback function which will be be used to require a module if no location for it can be found.  This function is called after the functions on `*module-fallback-loaders*` are called, and only if none of them loaded the module.  Its default value is `nil`.  If this function is called it is assumed to have loaded the module regardless of its return value.  Thus no error will be signalled if it is given.
- `error` means that failure to require a module is an error, default `t`.
- `module-path-descriptions` is the list of module path-descriptions, default `*module-path-descriptions*`.
- `hints` is a list of pathname designators which are hints as to where the module lives: if given, these are searched first.  Default is `()`, & this is not inherited from the ambient value.  This is used by reloading, but can also be used explicitly.
- `module-component-separators` is a list of characters which separate module components.  The default is the value of `*module-component-separators*` (which, by default, is `(#\.)`).
- `module-component-rewriter`, if non-`nil`, is a designator for the function used to rewrite components (see below).  The default is the value of `*module-component-rewriter*`.
- `wrapper-arguments` is a list of keyword arguments passed to wrappers, default `'()`.

Wrappers are not yet documented.  Reloading is experimental.

`require-module` returns:

- its first argument and `t` if the module was loaded;
- its first argument and `nil` if the module was already loaded (no search is done in this case);
 - the first value returned by a fallback loader and `t`if one of them loaded it;
- the first value returned by the `fallback` function and `t` if it is given and no location was found (note that no error happens here: it's up to the fallback to decide if there should be an error).
- `nil` and `nil` if there is no fallback loaders, no fallback, `error` is `nil` and the module was not found;
-  an undefined value or values if `pretend` is given.

`require-module` relies on `require` to do the actual work of loading the file and most of the work of maintaining `*modules*`.  It will only search (and thus only call `require` on the results of the search) if the module is not already present on `*modules*`, either because it was not there before the call or because it's just removed it due to the `force` option.

It is not an error if a module doesn't define a package with its own name when the `use` option is given, but there will be a warning unless `quiet` is also given.

**`*module-fallback-loaders*`** is a list of function designators which `require-module` will use as fallbacks by default.  Each entry should be designator for a function of one argument: the module name.  If the function returns true then the module is assumed to be loaded, and its return value will be the first value of `require-module`.  These functions are called before the function given as the `fallback` argument, if any, and it will only be called if none of them loads the module.

The initial value of `*module-fallback-loaders*` is `()`.

**`locate-module`** locates a module: it's what `require-module` uses to find things.  It has one mandatory argument which is the module name, and two keyword arguments.

- `module-path-descriptions`is the list of module path-descriptions, default `*module-path-descriptions*`.
- `hints` is a list of possible pathname designators for the module; if given it is searched first.  the default is `()`.  This is used by module reloading but can be used explicitly.
- `module-component-separators` is a list of characters which separate module components.  The default is the value of `*module-component-separators*` (which, by default, is `(#\.)`).
- `module-component-rewriter` is the module component rewriter, the default being the value of `*module-component-rewriter*`.
- `verbose` makes it say what it's doing, default `nil`.
- `debug` enables some debugging output.

`locate-module` will return 5 values:

1. the selected pathname, or `nil`;
2. the source pathname, or `nil`;
3. the write date of the source pathname, or `nil` if none was found;
4. the compiled pathname, or `nil`;
5. the write date of the compiled file, or `nil` if none was found.

If the selected pathname is `nil` then all the other values will be `nil`.  Otherwise it will be the newest of the source and compiled files, or the only one of them found if both do not exist.  In particular it will be `eq` to exactly one of the other pathnames in this case.  All pathnames returned will be truenames.

The reason for this behaviour is that the file located is often a tiny 'loader' shim which never gets compiled[^7], so in this case there's nothing wrong if there is no compiled file.  If there is a compiled file but it is out of date it's best to return the source file: it's current, and anything that calls `locate-module` can choose to compile the file before loading, which is something `require-module` can do.

`locate-module` does not either use or update any caches.

**`require-modules`** is a shim around `require-module` which expects a list of module descriptions instead of a module name.  It takes all the same keyword arguments as `require-module`.  A *module description* is either

- a module name;
- or a cons of a module description and some arguments to `require-modules`.

In the second case either `require-module` (if the nested description is not itself a cons) or `require-modules` is called with the two sets of arguments appended to each other, with those from the module specification first.  Because CL uses the first of any repeated keyword arguments, this means that individual module specifications can override the keyword arguments provided to the function as a whole.  `require-modules` returns a list of lists of the two values returned by each `require-module` it calls.

**`requires`** is a NOSPREAD version of `require-modules` with a fallback to `require`.  So `requires` lets you say that you want one or more modules, and if `require-module` doesn't know how to get them then the system should try `require` in case it does.

**`needs`**  lets you express a dependency on modules at compile time: it is `requires` wrapped in a suitable `eval-when`, but its arguments are quoted.  Note that `needs` quotes its arguments: an older version didn't so this is an incompatible change.  If you use strings or keywords as module names this doesn't matter, but it makes things like `(needs (:org.tfeb.hax.collecting :use t))` more natural[^8].

The recursive behaviour of `require-modules` is inherited by both `requires` and `needs`, and lets you say things like this:

```lisp
(needs ((:org.tfeb.hax.collecting :org.tfeb.hax.iterate)
        :compile t :use t))
```

which is sometimes convenient.

**`clear-module-caches`** is a function of no arguments which will clear both the cache of loaded files & their write dates and the structure (not really a cache) which maintains the notion of the descendants of a module.

### The search list
The list of path descriptions is **`*module-path-descriptions*`**.  Each entry in this list is a *path description*, which is one of:

- a list, which should consist of suitable arguments to `make-pathname`;
- a designator for a function of no arguments – the function is called and it should return something suitable to be an element of the searchlist (this process is iterated: if the function returns a function then that will be called in turn);
- `nil` which is ignored (this is useful so a function can return `nil` as a way of declining to provide a useful value;
-  something else, which should be a pathname designator (probably either a list of a string) which is handed to `pathname` to turn into a pathname.

The initial value of `*module-path-descriptions*` is `()`.

It is perfectly possible to maintain `*module-path-descriptions*` manually, and that's how it worked for a long time.  There is now a macro which makes this a little easier, perhaps, and a utility which helps with entries which are functions.

**`define-module-path-descriptions`** defines module path descriptions.  It does this for a particular host (this is one of the reasons logical pathnames are useful), and there are a bunch of options: the most simple ones control whether to add the descriptions before or after the existing ones, and whether to replace existing descriptions for the same host.  Rather than describe it in detail here are a couple of examples (yes, this is copping out);

```lisp
(define-module-path-descriptions ("QL" :after t)
  "QL:LOCAL-PROJECTS;*-LOADER.LISP"
  "QL:LOCAL-PROJECTS;LOADER.LISP"
  "QL:LOCAL-PROJECTS;*.LISP")
```

This will add a bunch of pathname descriptions for a logical host named `"QL` and it will add them after any existing ones.  It will replace any descriptions for the `"QL"` logical host.

```lisp
(define-module-path-descriptions "-"
  (lambda ()
    (merge-pathnames "*-loader.lisp" (pathname (sb-posix:getcwd))))
  (lambda ()
    (merge-pathnames "loader.lisp" (pathname (sb-posix:getcwd))))
  (lambda ()
    (make-pathname "*.lisp" (pathname (sb-posix:getcwd)))))
```

This is what my SBCL init file looked like before `module-path-descriptions-for-function` exists, where it was the first thing that adds to `*module-path-descriptions*`.  The `"-"` host doesn't exist: it's just there because the macro needs something to be there.  Each form in the body is a function which will return a pathname based on the current directory, which means that subdirectories of the current directory get searched first.

There is a weirdness here which is worth noting: `define-module-path-descriptions` doesn't normally evaluate the forms in its body.  But it *does* arrange for them to be evaluated if they are lists whose first element is `lambda`, which is why the above works.  This is a horrible hack.

`define-module-path-descriptions` uses an (exported) function called `add-module-path-descriptions` to do most of its work.  You'll need to look at the source to what it does.

**`module-path-descriptions-for-function`** is a function which takes two arguments and returns a list of functions suitable for entries in `*module-path-descriptions*`.  Its arguments are:

- a designator for a function of no arguments which should return either a pathname or `nil`;
- a list of pathname specs, which are either pathname designators or partial lists of arguments to `make-pathname`.

For each pathname spec this will return a a functiony path description which calls the function, and if does not return `nil` then merges either the result of a suitable `make-pathname` call (for a listy pathname spec) or a call to `pathname` (for any other pathname spec) with its result.  If the function returns NIL just return NIL.

A good example of how to use this function is to provide searching depending on the current file being loaded or compiled:

```lisp
(setf *module-path-descriptions*
      (append *module-path-descriptions*
              (module-path-descriptions-for-function
               (lambda ()
                 (or *compile-file-truename* *load-truename*))
               '("*-loader.lisp"
                 "loader.lisp"
                 "*.lisp"))))
```

You can't (easily) use this function with `define-module-path-descriptions`: you need to use it as above.  Note that the function will get called for each pathname spec.

### Providing modules
As an interface to `install-providers`, below, there is a function called **`provide-module`**: it does exactly what `provide` does (it relies on `provide` to do the work) but also maintains an alist, **`*module-providers*`** which maps from module names to the files that provided them (from `*load-truename*`).

Additionally, **`provides`** is a counterpart to `requires` and `needs`:  it lets you state what module or modules a given file provides.  Unlike `needs` it's not a macro because modules should not be provided until they are loaded.

### Running code after a module is provided
**`after-require-module`** is a macro which can be used in a module, to arrange for the forms in its body to be run after the module has been provided.  The forms are wrapped in a block also called `after-require-module`.   This can be used any number of times, and if the module is being loaded some other way it will simply do nothing.  For example:

```lisp
(defun interface (...)
  ...)

...

(after-require-module
  (register-interface #'interface))
```

This is particularly useful for modules consisting of several files (with a 'loader' file to load them all): the forms given in `after-require-module` are run after the whole module is loaded.

### Errors
**`require-module-error`** is a condition class, and all errors signalled directly by any of the functions in the packages are of this class or, potentially, subclasses of it.  Currently it's a subclass of `simple-error` but it may not always be so.  Errors signalled by things `require-module` may call, such as `require` may not be of this class however.

### Other functionality
There is a mechanism for adding wrappers around the process of actually providing a module (after its file has been located).  This is not yet documented here, but its main use has been to arrange to forget about system definitions for modules which involve some system definition tool, so the LispWorks development environment doesn't get cluttered up with system definitions that are not interesting.  It's also used to implement `after-require-module`, above.  This mechanism is subject to change.

**`*module-component-separators*`** is the default list of characters which can separate module names.  Its initial value is `(#\.)`, meaning that names are separated by `.` characters.  You could, for instance, set it to be `(#\. #\/)` which would allow module named like `"org.tfeb.ts/test"` to parse as `("org" "tfeb" "ts" "test")`.

**`*module-component-rewriter*`** is a variable which may either be `nil` (the default) or a function designator, and which provides the default module component rewriter.  If it is a function designator that function is called on each component of a dotted module name (for `"ORG.TFEB.FOSH"` the components are `"ORG"`, `"TFEB"` & `"FOSH"`) and the value it returns is used as the name of the component.  This is useful for components which have names which are not valid pathname components: for instance it can be used to rewrite a name like `"series/conduit"` into `"series-conduit"`(and this was its original purpose).

### Examples
Given

```lisp
(defvar *my-mpds* '())

(define-module-path-descriptions ("TFB"
                                  :module-path-descriptions *my-mpds*)
  "TFB:LIB;MODULES;*.LISP"
  "TFB:LIB;MODULES;*-LOADER.LISP"
  "TFB:LIB;MODULES;LOADER.LISP")
```

then

```lisp
> *my-mpds*
("TFB:LIB;MODULES;*.LISP"
 "TFB:LIB;MODULES;*-LOADER.LISP"
 "TFB:LIB;MODULES;LOADER.LISP")

> (locate-module :org.tfeb.pretend
                 :module-path-descriptions *my-mpds*
                 :verbose t)
> (locate-module :org.tfeb.pretend
                            :module-path-descriptions *my-mpds*
                            :verbose t)
Looking for module :org.tfeb.pretend
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend/pretend.lisp
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend.lisp
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend/pretend-loader.lisp
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend-loader.lisp
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND;LOADER.LISP.NEWEST
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend/loader.lisp
Probing TFB:LIB;MODULES;PRETEND;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/pretend/pretend.lisp
Probing TFB:LIB;MODULES;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/pretend.lisp
Probing TFB:LIB;MODULES;PRETEND;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/pretend/pretend-loader.lisp
Probing TFB:LIB;MODULES;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/pretend-loader.lisp
Probing TFB:LIB;MODULES;PRETEND;LOADER.LISP.NEWEST
 as     /Users/tfb/lib/lw/modules/pretend/loader.lisp
nil
nil
nil
nil
nil
```

But instead if we had

```lisp
(defvar *my-mpds* '())

(define-module-path-descriptions ("-"
                                  :module-path-descriptions *my-mpds*
                                  :after t)
  "/Local/packages/lispworks/lib/modules/*.lisp"
  "/Local/packages/lispworks/lib/modules/*-loader.lisp"
  "/Local/packages/lispworks/lib/modules/loader.lisp")
```

then

```lisp
> *my-mpds*
("/Local/packages/lispworks/lib/modules/*.lisp"
 "/Local/packages/lispworks/lib/modules/*-loader.lisp"
 "/Local/packages/lispworks/lib/modules/loader.lisp")

> (locate-module :org.tfeb.utilities.permutations
                 :module-path-descriptions *my-mpds*
                 :verbose t)
Looking for module :org.tfeb.utilities.permutations
Probing /Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS/PERMUTATIONS.lisp
Probing /Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.lisp
Found /System/Volumes/Data/Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.lisp
#P"/System/Volumes/Data/Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.lisp"
#P"/System/Volumes/Data/Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.lisp"
3823180214
nil
nil
```

This second example is betraying the fact that I'm on a Mac: the mac's filesystem is case-insensitive / case-preserving, so the file is being found with an uppercase filename, when its name is 'really' lowercase.  If you provide the `debug`argument to `locate-module` you will get even more verbose output.

### Falling back to Quicklisp
If you have Quicklisp, this works:

```lisp
(needs
 (:org.tfeb.hax.collecting)
 (:cl-ppcre :fallback ql:quickload))
```

Which means you can write small programs which rely on Quicklisp to fetch and load things without either an ASDF system definition, or a bunch of explicit `eval-when`s in the sources to load things at compile time.  In fact it is pretty much possible to use `needs` to informally define systems without a central system definition, even when those systems have dependencies on Quicklisp or other systems.

Another way of doing this, more globally, is

```lisp
(defun load-module/ql (m)
  (handler-case
      (ql:quickload m :verbose nil :silent t)
    (ql:system-not-found () nil)))

(pushnew 'load-module/ql *module-fallback-loaders*)
```

I have essentially this code in my init files.

### Deficiencies
There are no docstrings for anything: there should at least be brief ones.  There are no error or warning conditions defined which there should be.  Wrappers are not documented.

### Notes
`require-module` does quite a lot of processing of pathnames.  It is all intended to be portable but it also turns out to explore some of the boundaries of what implementations support.  As an example, SBCL can't currently deal with making partly-wild pathnames, so in SBCL you often need to provide stringy logical pathnames in configurations[^9].

When making module path descriptions based on the current file being compiled or loaded always use truenames and always prefer `*compile-file-truename*` as otherwise you may get the name of some parent file which has asked to compile the file of interest.

All of the functions accept strings or symbols as module names: they'll complain about anything else rather than blindly calling `string`.

`needs` has changed incompatibly so it now quotes its arguments.

`locate-module` used to return only what is now its first value: this is a compatible change, I think.

A consequence of the caching of truenames is that, so long as the graph of modules and their requirements is a DAG, each file will be loaded just once.  That means it's perfectly fine for any file in the DAG just to say `(needs ...)` to express the files it relies on: if they're already loaded, they won't be loaded again unless they've changed.  If the graph has cycles you're in all sorts of trouble, of course.

## Installing modules automagically: `install-providers`
I now use makefiles to install my personal CL modules and systems, and either ASDF or the LispWorks system definition tool to build them once installed[^10].  Previously I used an ancestor of this code.  It lives in the `org.tfeb.tools.install-providers` package and will add `:org.tfeb.tools.install-providers` to `*modules*`.

**`install-providers`** will install a set of modules from the files they were originally loaded from into a directory tree under a specified root.  It has one argument which is the root under which to install things.  The remaining keyword arguments are:

- `providers` is an alist of `(module-name . providing-file)` which tells it what to install, the default being `*module-providers*` from `require-module`;
- `really` says to really copy the files, the default is `nil` in which case it will just tell you what it would do;
- `clear` will cause it to reset `*module-providers*` to `()` after installing, this is true if no explicit list is given and if it is actually installing, and it will refuse to clear the list in any case if an explicit list *is* given;
- `filter` is a function which will be called with the module name and the source & destination paths, and which should return true if the module is to be installed.

The function returns an alist of `(source target)` of the files copied.

### Example
```lisp
 > *module-providers*
(("ORG.TFEB.TOOLS.INSTALL-PROVIDERS"
  . #P"/Users/tfb/src/lisp/systems/tools/install-providers.64xfasl")
 ("ORG.TFEB.LW.LW-COMMANDS"
  . #P"/Local/packages/lispworks/lib/modules/org/tfeb/lw/lw-commands.64xfasl")
 ("ORG.TFEB.TOOLS.REQUIRE-MODULE"
  . #P"/Users/tfb/src/lisp/systems/tools/require-module.64xfasl"))

> (install-providers "/tmp/")
Would
 ensure dirs for /tmp/org/tfeb/tools/install-providers.64xfasl
 copy /Users/tfb/src/lisp/systems/tools/install-providers.64xfasl
   to /tmp/org/tfeb/tools/install-providers.64xfasl
Would
 ensure dirs for /tmp/org/tfeb/lw/lw-commands.64xfasl
 copy /Local/packages/lispworks/lib/modules/org/tfeb/lw/lw-commands.64xfasl
   to /tmp/org/tfeb/lw/lw-commands.64xfasl
Would
 ensure dirs for /tmp/org/tfeb/tools/require-module.64xfasl
 copy /Users/tfb/src/lisp/systems/tools/require-module.64xfasl
   to /tmp/org/tfeb/tools/require-module.64xfasl
nil
```

## Building installed modules: `build-modules`
I have a significant collection of single-file modules which generally don't have overriding ASDF or other system definitions.  `require-module` and its wrappers – `needs` is the interface I use most commonly – will arrange for modules which exist only as sources or for which the FASLs are out of date to be compiled on demand if needed.  Sometimes it's nice to point at a whole collection of module files in a source directory and say 'compile all of the installed versions of these that need to be compiled': that's what `build-modules` is for.

**`compile-installed-modules`** ensures installed copies of a number of single-file modules are compiled, using `locate-module` to find the installed modules to consider.  Arguments:

- `prefix`is a string designator for the module prefix, canonicalised to an upper-case string;
- `files`is a list of pathname designators (typically source files, but only the name components of the pathnames are used) corresponding to the modules to be compiled.

Keyword arguments:

`omit` is a list of pathname designators (or a single pathname designator) of files to omit – generally these will be wild pathnames – default `()`;
`only` is the opposite of `omit` – if given it means consider only these files – default `()`;
`force`, if true, says to compile the modules even if the compiled files seems already to be up to date, default `nil`;
`verbose` says to be more verbose, default `nil`;
`pretend` says to pretend, default `nil`.

`compile-installed-modules` returns a list of the things it compiled.  Each element of the list is a list of the local source file, the corresponding module source file, and the module name.

### An example
Pretending to compile the modules in the tools source directory.

```lisp
> (compile-installed-modules
   "org.tfeb.tools" (directory "*.lisp")
   :omit '("sysdcl" "*-cometh" "*-loader")
   :pretend t :verbose t)
Would skip /System/Volumes/Data/Local/tfb/packages/quicklisp/local-projects/org/tfeb/tools/ensuring-features.lisp
 for ORG.TFEB.TOOLS.ENSURING-FEATURES
 from /Users/tfb/src/lisp/systems/tools/ensuring-features.lisp
 as fasl is newer
Would skip /System/Volumes/Data/Local/tfb/packages/quicklisp/local-projects/org/tfeb/tools/require-module.lisp
 for ORG.TFEB.TOOLS.REQUIRE-MODULE
 from /Users/tfb/src/lisp/systems/tools/require-module.lisp
 as fasl is newer
Would compile /System/Volumes/Data/Local/tfb/packages/quicklisp/local-projects/org/tfeb/tools/build-modules.lisp
 for ORG.TFEB.TOOLS.BUILD-MODULES
 from /Users/tfb/src/lisp/systems/tools/build-modules.lisp
Would skip /System/Volumes/Data/Local/tfb/packages/quicklisp/local-projects/org/tfeb/tools/install-providers.lisp
 for ORG.TFEB.TOOLS.INSTALL-PROVIDERS
 from /Users/tfb/src/lisp/systems/tools/install-providers.lisp
 as fasl is newer
((#P"/Users/tfb/src/lisp/systems/tools/build-modules.lisp"
  #P"/System/Volumes/Data/Local/tfb/packages/quicklisp/local-projects/org/tfeb/tools/build-modules.lisp"
  "ORG.TFEB.TOOLS.BUILD-MODULES"))
```

Here only `build-modules` itself would need to be compiled, as the FASL is out of date with the source.

### Notes
`build-modules` is only really useful for single-file modules which deal with their own interdependencies: it's not anything like a replacement for a system definition tool.  I have lots of these however, so it's useful for me.

### Package, module
`build-modules` lives in`org.tfeb.tools.build-modules` and provides  `:org.tfeb.tools.build-modules`.

## Implementation features: `feature-expressions`
CL's `#+` and `#-` syntax lets you evaluate [feature expressions](https://www.lispworks.com/documentation/HyperSpec/Body/24_aba.htm "Feature Expressions") – boolean expressions based on the [`*features*` variable](https://www.lispworks.com/documentation/HyperSpec/Body/v_featur.htm#STfeaturesST "*features*") – at *read* time, but without cleverness no later than that.  This is ideal to cope with syntax that can't be read unless some feature is present, such as packages which may not exist, or for differences between implementations, as a compiled file whose source was read by one implementation is not likely to be useful in another.

But sometimes you want to check features of the implementation later than read time, or you want to know that a feature expression that was true at read time (and hence probably at compile time) is still true at load time (and hence probably at run time).  Sometimes also you might want to evaluate a feature expression directly, or to more generally write conditionals based on feature expressions.  This is what `feature-expressions` lets you do.

**`evaluate-feature-expression`** is a function which evaluates feature expressions as described in [the spec](http://www.lispworks.com/documentation/HyperSpec/Body/24_aba.htm "Feature Expressions"), with the possible restriction that operators (`or`, `and` etc) need to be symbols in the `CL` package.   It has a compiler macro which will compile feature expressions which satisfy `constantp` into equivalent code.

**`feature-case`** is a macro which dispatches on feature expressions.  It is like `typecase` rather than `case`: its body is a series of clauses of which the first elements are either feature expressions or one of the symbols `otherwise` or `t`, and whose remaining elements are forms to be evaluated if the feature expression is true, or in the case of `otherwise` or `t`, in any case.  An example:

```lisp
(feature-case
  (:LispWorks
   ...)
  ((or :SBCL :CMUCL)
   ...)
  (otherwise
   ...))
```

One result of these rules is that, if you expect there to be features named `otherwise` or `t` (which seems unlikely), you would need to check for them by a feature expression like, for instance `(or otherwise)`.

**`ensuring-features`** is a macro designed to be used at top level, and whose purpose is to make assertions about various features at various times in the processing of the file.  Its body consists of  number of clauses.  Each clause consists of `(<time> [<feature-expression> ...])`, where `<time>` either a specification suitable for [`eval-when`](https://www.lispworks.com/documentation/HyperSpec/Body/s_eval_w.htm "eval-when") or `t`, which is a shorthand for `(:compile-toplevel :load-toplevel :execute)`.  Each `<feature-expression>` is a feature expression, to be evaluated by `evaluate-feature-expression` as above.

`ensuring-features` expands into suitable `eval-when` expressions which will ensure that the specified feature expressions are true at the appropriate times, signalling an informative error if not.

### Example
```lisp
(ensuring-features
 ((:load-toplevel)
  :org.tfeb.tools.feature-expressions))
```

This will ensure that `:org.tfeb.tools.feature-expressions` is present as a feature at load time (but it need not be at compile time or any other time).  `feature-expressions` contains this form in its own source code: the feature will not be present when it is being compiled, but will be when it is being loaded.

### Package, module, feature
`feature-expressions` lives in `org.tfeb.tools.feature-expressions`, provides `:org.tfeb.tools.feature-expressions` and also pushes a feature with this name onto `*features*`.

## Deprecating code: `deprecations`
Sometimes you don't get things right the first time, so you want to mark old interfaces as deprecated.   `deprecations` lets you define things as deprecated: something which is deprecated will cause a warning if code that uses it is compiled, unless such warnings are suppressed, and once code which uses deprecated functionality has been compiled it is possible to introspect about what files referred to what deprecated functionality.

You can deprecate functions, generic functions, macros and symbol macros.  You *can't* deprecate variables, because I could not work out a way to do this without changing the semantics of code which refers to them: see below.

All of the defining macros for deprecated functionality are plug-compatible with the standard defining macros: `(defun foo ...)` becomes `(define-deprecated-function foo ...)` with no other changes needed.

Deprecations may be signalled as a subclass of `style-warning` at compile time, and are also recorded.  You can inhibit the compile-time warnings and also establish a local dynamic extent for deprecation recording so you can know which deprecations were noticed during which compilation.

### Interface
**`define-deprecated-function`** will define a deprecated function.  It is just like `defun` except that compilation of code which calls the function will, unless inhibited, signal a warning, and will in any case record information about it.  If the function has a docstring, this is used as the deprecation notice.

**`define-deprecated-generic-function`** does the same thing for generic functions.  In this case the deprecation notice is pulled from the `:documentation` clause, if any.  As with functions the deprecation warning happens at the time code calling this function is compiled.

**`define-deprecated-macro`** defines a deprecated macro: the deprecation notice comes from the docstring, if any.  The deprecation warning will happen when the macro is expanded.

**`define-deprecated-symbol-macro`** defines a deprecated symbol macro.  These can't have deprecation notices as there is no room in the syntax for such a thing (symbol macros don't have docstrings).  The deprecation warning happens when the symbol macro is expanded.

**`deprecation-warning`** is a subclass of `style-warning` used for deprecations.  It has readers:

- `deprecation-warning-thing` is the name of the deprecated thing;
- `deprecation-warning-what` is the sort of thing that was deprecated, which may be `:function`, `:generic-function`, `:macro`, `:symbol-macro` or possibly some other keyword symbol (but in fact not);
- `deprecation-warning-notice` is the deprecation notice, or `nil`;
- `deprecation-warning-location` is the truename of the file where the deprecation was noticed, or `nil` if there is no file.

**`*inhibit-deprecation-warnings*`**, if true, will cause deprecation warnings to be inhibited during compilation.  If it is true then deprecations will be recorded but no warnings will happen.  Default value is `nil`.

**`map-deprecations`** maps a function over information about deprecated functionality: the function is called with four arguments:

- the location of the deprecation as above;
- a list of the names of every deprecated thing in that location, with each name occurring only once;
- a list of the 'whats' of the deprecated things, one for each thing, which are keywords, as above;
- a list of the deprecation notices for each thing, as above.

`map-deprecations` returns no values.

**`clear-deprecations`** will clear the recorded deprecations.  Returns no values.

**`report-deprecations`** produces a report on users of deprecated functionality.  It has two keyword arguments:

- `stream` is the stream to print on, default `*standard-output*`;
- `clear` will clear the set of deprecations after producing the report, default `nil`.

It also returns no values.

**`with-deprecations`** is a macro which establishes a dynamic context for reporting and warning about deprecations.  Syntax is `(with-deprecations (&key (inhibit nil)) ...)`: within the dynamic extent of the macro's body the deprecation reporting functions will report only deprecations which occurred during the extent of the body (and `clear-deprecations`, `map-deprecations` etc will see only those deprecations).  You can also optionally inhibit declaration warnings during the dynamic extent of the body.

### Example
Given a file containing this:

```lisp
(define-deprecated-generic-function bar (x)
  (:documentation "use fish")
  (:method (x)
   x))

(define-deprecated-function foo (x)
  "use new-foo"
  (bar x))

(defun bone (x)
  (foo x))
```

Then compiling this file without deprecations inhibited will produce two warnings which might appear as follows:

```lisp
Warning in foo: deprecated generic function bar in /path/to/x.lisp (use fish)
Warning in bone: deprecated function foo in /path/to/x.lisp (use new-foo)
```

Alternatively you can choose to inhibit warnings and report:

```lisp
> (with-deprecations (:inhibit t)
    (compile-file "x.lisp")
    (report-deprecations))
[... no deprecation warnings ...]
/path/to/x.lisp:
 generic function bar (use fish)
 function foo (use new-foo)
```

Note that if the order of the definition of `bar` and `foo` are inverted you'll only get one warning & recorded deprecation: that's because it's all done by compiler macros.

### Notes
Deprecations for functions and generic functions are implemented by compiler macros: if you already have compiler macros for deprecated functionality things will not work as you expect.  Deprecations for macros and symbol macros are implemented by adding a form to the macro definition, which I think should be safe.

Because things are implemented by compiler macros you won't see most deprecations unless you compile your code.

An earlier version of the code attempted to implement deprecations for global variables by defining the global variable name as a symbol macro which expanded to a secret version of the name, producing a deprecation warning along the way.  This doesn't work because, if `*foo*` is deprecated then `(let ((*foo* ...)) ...)` binds `*foo*` lexically, and you can't have symbol macros for symbols declared special.  So such a system inevitably breaks the semantics of the program.  I don't think there is any way to deal with this problem portably.

Because deprecation warnings *are* warnings and because the warning class is documented & has documented readers you can handle them, muffle them, and so on.  For instance:

```lisp
> (handler-case
      (compile-file "x.lisp")
    (deprecation-warning (w)
      (error "deprecated: ~A" w)))
```

This came, with permission, from an idea by a friend of mine in an answer on [Stack overflow](https://stackoverflow.com/a/71393319 "Deprecated functions"), but it is now a lot more elaborate than that code was.

### Package, module
`deprecations` lives in `org.tfeb.tools.deprecations` and provides `:org.tfeb.tools.deprecations`.

---

The TFEB.ORG tools are copyright 2002, 2012, 2020-2022 Tim Bradshaw.  See `LICENSE` for the license.

---

[^1]:	This README also has footnotes which GitHub does not support.  You'll just have to make sense of that.

[^2]:	Well, I don't know how other CL programmers work: I do know that I do this though.

[^3]:	Or systems in one of the system definition facilities which existed before ASDF, of course.

[^4]:	I write them as keyword symbols, so my modules always end up with upper-case names.

[^5]:	Note that, here and below, I am writing pathnames as strings.  This is just because I am being lazy: the system works in terms of pathnames, not strings.

[^6]:	The combination of `&rest args &key ...` and CL's carefully-thought-out leftmost-first-duplicates-allowed keyword argument handling makes this delightfully simple.

[^7]:	And, in fact, such loader shims often *can't* be compiled as they rely on things changing during the load of their source.

[^8]:	It's hard to see a case where having `needs` *not* quote its arguments is useful since whatever values it uses would need to be available at compile-time anyway, which means you'd already almost certainly have to use `eval-when`.  In any case, if you want what `needs` does without the autoquoting, you should now use `(eval-when (...) (requires ...))`.

[^9]:	SBCL's behaviour is reasonable: what *should* `(make-pathname :name "*-loader" ...)` do?  Unfortunately it also leaves no way of making pathnames which have partly-wild components other than parsing namestrings.

[^10]:	A key to making this work with ASDF is to turn off output translations and keep the compiled files alongside their sources.