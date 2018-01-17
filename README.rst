Intro
-----

I am a emacs beginner, know little about elisp. I use emacs only when there's no other substitutions.

This is my personal configurations of emacs, maybe useful for emacs beginners.

Usage
-----

I am using emacs 25.x, so the following is only tested on this specific version.

* sudo apt install emacs25 or Compile and install emacs 25.3 (Please google)
* rm -rf ~/.emacs.d
* git clone https://github.com/windtail/emacs-config.git ~/.emacs.d
* open emacs, packages will be automatically installed from melpa (China mirror)
* enjoy it or not ^_^

Recommended system packages
---------------------------

GNU global
  for counsel-gtags

cscope
  for xcscope

ag (silversearcher-ag)
  for counsel-projectile-ag

manpages-dev
  for doc of standard c library functions

libstdc++-6-doc
  for doc of standard c++ library functions

All this can be done on debian::

  sudo apt install global cscope silversearcher-ag manpages-dev libstdc++-6-doc
