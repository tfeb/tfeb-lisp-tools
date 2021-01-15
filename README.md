# TFEB.ORG tools
This repo contains a system which will be a collection of fairly miscellaneous Common Lisp tools, which I have written over the years in order to generally get stuff done.  Currently only two are published here:

- `require-module` provides variants of `require` which will search for modules, as well as the mechanisms to control the search, and also a variant of `provide` which keeps records of the file which provided a module;
- `install-providers` makes use of the records of module providers kept by `require-module` in order to copy them to places they will be found.

I hope to add more tools as I disentangle them from the things they're currently entangled with and modernise them where needed.  All of the tools are intended to be portable CL except where documented.

The descriptions here are at best partial[^1].

## General
### Portability
These tools purport to be portable Common Lisp.  If they're not that's either a bug in the tools or a bug in the CL implementation.  In the former case I very definitely want to know, and I am also willing to add workarounds for the latter although I may be unable to test them on implementations I don't use.

### Zero history
All of these tools have long and varied histories.  However these histories are entangled with a lot of other code which is not public, so that history is not represented in the publication repo where you are probably reading this.

### Naming conventions
All of these tools make use of, and often work best with, *domain-structured names*: packages, modules, features and so on have names which start with a DNS domain name in reverse order and then may continue to divide further.  In this case the prefix is `org.tfeb.tools`: `org.tfeb` is the DNS component and `tools` is the division within the DNS part.

This naming convention ensures uniqueness of things like package names.  It does make typing package-qualified names somewhat irritating: the solution is not to do that but to construct packages which import or use the symbols you need.  CL's default package system is very well up to solving this problem, rumours to the contrary notwithstanding: [conduit packages](https://github.com/tfeb/conduit-packages "conduit packages") can make it a little simpler to express what you want to do.

Nothing actually cares that names correspond to real DNS domains: some things do care that the namespace is hierarchically structured and big-endian.

## Requiring modules with searching: `require-module`
Although CL now has competent tools for defining and distributing 'systems' of code, in the form of [ASDF](https://common-lisp.net/project/asdf/ "ASDF") and particularly [Quicklisp](https://www.quicklisp.org/ "Quicklisp"), not all bits of code are large enough to justify their use.  In particular it's in the nature of the incremental and  exploratory programming style encouraged by CL that people[^2] tend to write a bunch of little tools and utilities, living in single source files, which help them get their work done and which they reuse over time.  Some of these may be portable, some not, and many of them can be quite small: a tool I use very often is under 50 lines excluding noise, but including docstrings and comments, and I have had smaller ones.

Turning these small tools into ASDF systems[^3] either means combining them into some larger conglomeration of code such as this one, or doubling the number of files you have to deal with because of the system definition files for each small tool.  Combining them into a larger conglomeration of code means everything takes longer and costs more; doubling the number of files means the same.  Also, of course, the ancestor of `require-module` predates the common availability of portable system definition tools by many years.

Well, CL has `require`, `provide` and `*modules*` and although these are deprecated, they're not going away any time soon, or in fact ever.  So a reasonable approach is to make all the small tools into modules by adding suitable `provide` incantations, and then to use `require` to load them.  This still leaves the problem that `require` needs to know where to find the module being required is in the filesystem, and I didn't want to fill code with explicit pathnames, even though logical pathnames make things at least a little better.

`require-module` provides a variant of `require` which has a portable mechanism for searching for and locating files corresponding to modules.  In particular it understands how to combine domain-structured module names (`org.tfeb.hax.collecting` say) with a search list of pathnames to locate a file corresponding to the name which, when loaded, should provide the module.  There are mechanisms to define what directories should be searched and in which order, and to wrap code around the process of requiring a module in the manner of CLOS around methods.  Finally it also provides a wrapper for `provide` which keeps a record of the file which provided a given module, which can be used by `install-providers` (below) to install modules where it expects them to be.

### Logical pathnames
`require-module` doesn't need you to use logical pathnames, but it does work well with them and I have usually used it that way.  Using logical pathnames gives you two advantages:

- the mechanism for maintaining the search list lets you add & replace a bunch of paths for the same host, and if that's a logical host you can invent new ones, so maintenance becomes easier;
- there is another level of translation between the names of modules and the physical pathname they come from – for instance by changing the translations of a logical host you can switch between variants of things easily.

In summary you don't have to use logical pathnames, but it can make things easier.

### Case
Module names are case sensitive: `require`uses `string=` to compare them.  Depending on how you write module names this can make life a little complicated[^4].  This is made more interesting because logical pathnames are really all upper-case.  `require-module` tries to be smart about this:

- if it is checking a logical pathname as a potential location it uppercases module names;
- if it is checking a physical pathname then it first tries the name as it is, then downcased, then upcased.

This typically works reasonably well: if you use module names which are symbols, are searching a physical pathname, and prefer lowercase filenames, then it will find a lowercase filename on the second try.

### The search algorithm
The search process starts by splitting up a domain-structured module name into a list of strings, which it will then variously use as directory and name components for the search:

- `"org.tfeb.hax"` is turned into `("org" "tfeb" "hax")`;
- `:org.tfeb.hax` is turned into `("ORG" "TFEB" "HAX")`.

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

### Package, module, feature
Everything below is exported from `org.tfeb.tools.require-module`.  `:org.tfeb.tools.require-module` will also be present in both `*modules*` and `*features*` once this module is loaded – the latter helps a lot with conditionals in init files.

### Requiring and locating modules
**`require-module`** will search for and `require` modules.  It has one mandatory argument which is the module name, and a number of keyword arguments:

- `verbose` will cause it to tell you what it's doing, default `nil`;
- `test` specifies the comparison function to use for module names, by default `#'string=` which is the right default;
- `pretend` will cause it not to actually require the module, default `nil`;
- `compile` will cause it to attempt to compile the module if it gets a source file name, default `nil`;
- `error` means that failure to require a module is an error, default `t`;
- `module-path-descriptions` is the list of module path-descriptions, default `*module-path-descriptions*`.

Other keyword arguments are allowed which are passed to possible wrapper functions, not described here (see the source).

`require-module` returns:

- its first argument and `t` if the module was loaded;
- its first argument and `nil` if the module was already loaded (no search is done in this case);
- `nil` and `nil` if `error` is `nil` and the module was not found.

`require-module` relies on `require` to do the actual work of loading the file and maintaining `*modules*`.  It will only search (and thus only call `require` on the results of the search) if the module is not already present on `*modules*`.

**`locate-module`** locates a module: it's what `require-module` uses.  It has one mandatory argument which is the module name, and two keyword arguments:

- `verbose` makes it say what it's doing, default `nil`;
- `module-path-descriptions`is the list of module path-descriptions, default `*module-path-descriptions*`.

It returns the file it found, or `nil` if it didn't find anything.

**`require-modules`** is a tiny shim around `require-module` which expects a list of module names instead of one.  All the other arguments are the same.  It returns a list of lists of the two values returned by each `require-module` it calls.

### The search list
The list of path descriptions is **`*module-path-descriptions*`**.  Each entry in this list isa *path description*, which is one of:

- a list, which should consist of suitable arguments to `make-pathname`;
- a function of no arguments – the function is called and it should return something suitable to be an element of the searchlist (this process is iterated: if the function returns a function then that will be called in turn);
- something else, which should be a pathname designator (probably either a list of a string) which is handed to `pathname` to turn into a pathname.

The initial value of `*module-path-descriptions*` is `()`.

It is perfectly possible to maintain `*module-path-descriptions*` manually, and that's how it worked for a long time.  There is now a macro which makes this a little easier, perhaps.

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
    (make-pathname :name "*-loader" :type "lisp"
                   :defaults (pathname (sb-posix:getcwd))))
  (lambda ()
    (make-pathname :name "loader" :type "lisp"
                   :defaults (pathname (sb-posix:getcwd))))
  (lambda ()
    (make-pathname :name "*" :type "lisp"
                     :defaults (pathname (sb-posix:getcwd)))))
```

This is from my SBCL init file, where it is the first thing that adds to `*module-path-descriptions*`.  The `"-"` host doesn't exist: it's just there because the macro needs something to be there.  Each form in the body is a function which will return a pathname based on the current directory, which means that subdirectories of the current directory get searched first.

There is a weirdness here which is worth noting: `define-module-path-descriptions` doesn't normally evaluate the forms in its body.  But it *does* arrange for them to be evaluated if they are lists whose first element is `lambda`, which is why the above works.  This is a horrible hack.

`define-module-path-descriptions` uses a function called `add-module-path-descriptions` to do most of its work.  You'll need to look at the source to what it does.

### Providing modules
As an interface to `install-providers`, below, there is a function called **`provide-module`**: it does exactly what `provide` does (it relies on `provide` to do the work) but also maintains an alist, **`*module-providers*`** which maps from module names to the files that provided them (from `*load-truename*`).

### Other functionality
There is a mechanism for adding wrappers around the process of actually providing a module (after its file has been located).  I'm not going to document this here, but its main use has been to arrange to forget about system definitions for modules which involve some system definition tool, so the LispWorks development environment doesn't get cluttered up with system definitions that are not interesting.

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
Looking for module :org.tfeb.pretend
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend/pretend.lisp
 from "TFB:LIB;MODULES;*.LISP"
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend.lisp
 from "TFB:LIB;MODULES;*.LISP"
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend/pretend-loader.lisp
 from "TFB:LIB;MODULES;*-LOADER.LISP"
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend-loader.lisp
 from "TFB:LIB;MODULES;*-LOADER.LISP"
Probing TFB:LIB;MODULES;ORG;TFEB;PRETEND;LOADER.LISP.NEWEST
 as     /Users/tfb/lib/lw/modules/org/tfeb/pretend/loader.lisp
 from "TFB:LIB;MODULES;LOADER.LISP"
Probing TFB:LIB;MODULES;PRETEND;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/pretend/pretend.lisp
 from "TFB:LIB;MODULES;*.LISP"
Probing TFB:LIB;MODULES;PRETEND.LISP
 as     /Users/tfb/lib/lw/modules/pretend.lisp
 from "TFB:LIB;MODULES;*.LISP"
Probing TFB:LIB;MODULES;PRETEND;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/pretend/pretend-loader.lisp
 from "TFB:LIB;MODULES;*-LOADER.LISP"
Probing TFB:LIB;MODULES;PRETEND-LOADER.LISP
 as     /Users/tfb/lib/lw/modules/pretend-loader.lisp
 from "TFB:LIB;MODULES;*-LOADER.LISP"
Probing TFB:LIB;MODULES;PRETEND;LOADER.LISP.NEWEST
 as     /Users/tfb/lib/lw/modules/pretend/loader.lisp
 from "TFB:LIB;MODULES;LOADER.LISP"
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
 from "/Local/packages/lispworks/lib/modules/*.lisp"
Probing /Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.lisp
 from "/Local/packages/lispworks/lib/modules/*.lisp"
Found /Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.64xfasl
#P"/Local/packages/lispworks/lib/modules/ORG/TFEB/UTILITIES/PERMUTATIONS.64xfasl"
```

This second example is betraying the fact that I'm on a Mac: the mac's filesystem is case-insensitive / case-preserving, so the file is being found with an uppercase filename, when its name is 'really' lowercase.

### Notes
`require-module` does quite a lot of processing of pathnames.  It is all intended to be portable but it also turns out to explore some of the boundaries of what implementations support.  As an example, SBCL can't currently deal with making partly-wild logical pathnames, so in SBCL you often need to provide stringy logical pathnames in configurations.

## Installing modules automagically: `install-providers`
I now use makefiles to install my personal CL modules and systems, and either ASDF or the LispWorks system definition tool to build them once installed[^6].  Previously I used an ancestor of this code.  It lives in the `org.tfeb.tools.install-providers` package and will add `:org.tfeb.tools.install-providers` to `*modules*`.

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

----

The TFEB.ORG tools are copyright 2002, 2012, 2020-2021 Tim Bradshaw.  See `LICENSE` for the license.

----

[^1]:	This README also has footnotes which GitHub does not support.  You'll just have to make sense of that.

[^2]:	Well, I don't know how other CL programmers work: I do know that I do this though.

[^3]:	Or systems in one of the system definition facilities which existed before ASDF, of course.

[^4]:	I write them as keyword symbols, so my modules always end up with upper-case names.

[^5]:	Note that, here and below, I am writing pathnames as strings.  This is just because I am being lazy: the system works in terms of pathnames, not strings.

[^6]:	A key to making this work with ASDF is to turn off output translations and keep the compiled files alongside their sources.