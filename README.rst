My Emacs Workflow
========================

Welcome to my Emacs-based workflow.

* `Main goals of my configuration`_
* `Why Emacs instead of VSCode, Zed or (Neo)Vim?`_
* `Supported Languages`_
* `Supported Markup Languages`_
* `LaTeX tools`_
* `System Dependencies`_

Main goals of my configuration
---------------------------------------

* Support of things I need
* Fast enough for me and my machine
* Easy to read configuration

Why Emacs instead of VSCode, Zed or (Neo)Vim?
-----------------------------------------------------------

1. I love Emacs key-bindings
2. I love Emacs Lisp
3. I love Emacs' extensibility. I can do everything in my Emacs

Supported Languages
-------------------------

* Go
* Bash

TODO:
* Protobuf
* Dockerfile
* YAML
* C
* Python

Supported Markup Languages
----------------------------------

* LaTeX
* reStructuredText

Also I use **Org Mode** sometimes and would like to setup Markdown, Sphinx, more Org Mode and read about TeXinfo.

LaTeX tools
--------------

For latex I have following tools:

* ``pdflatex`` - Default thing for me
* ``lualatex`` - Use it for system-wide fonts only =)
* ``xelatex``  - lualatex alternative which I don't use


All my LaTeX documentation compiled by ``pdflatex``. I use ``lualatex`` only for Resume/CV pdf compilation (because I use system-wide fonts with russian language support).

Also you can read about producing multi-page documentation from LaTeX.

System Dependencies
-------------------------

1. ``mu`` - Mail client
2. ``ripgrep`` - Faster grep alternative
3. ``python`` - Python =)
4. ``git`` - Main VCS
5. ``lualatex`` - PDF writing
6. ``make`` - M-x compile
7. ``sphinx`` - Documentation
8. ``hunspell`` - Interactive spelling
9. ``hunspell-ru`` - Russian dictionaries for Hunspell
