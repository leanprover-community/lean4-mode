Installation
============

Before using this major mode, you need to [install Lean 4](https://leanprover.github.io/lean4/doc/setup.html#basic-setup).

To use `lean4-mode` in Emacs, add the following to your `init.el`:
```
;; You need to modify the following line
(setq load-path (cons "/path/to/lean4-mode" load-path))

(setq lean4-mode-required-packages '(dash f flycheck lsp-mode magit-section s))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(let ((need-to-refresh t))
  (dolist (p lean4-mode-required-packages)
    (when (not (package-installed-p p))
      (when need-to-refresh
        (package-refresh-contents)
        (setq need-to-refresh nil))
      (package-install p))))

(require 'lean4-mode)
```
Alternatively if you are a fan of `use-package` and `straight.el` you
can use:
```
(use-package lean4-mode
  :straight (lean4-mode :type git :host github :repo "leanprover/lean4-mode")
  ;; to defer loading the package until required
  :commands (lean4-mode))
```
If you are a doom-emacs user, adding the following to `packages.el` should work:
```
(package! lean4-mode :recipe
  (:host github
   :repo "leanprover/lean4-mode"))
```
If you are using nix to manage doom-emacs, you will need to uncomment the `lean` doom module in `init.el`, add `(package! lean4-mode)` to `packages.el`, and package `lean4-mode` using [`emacsPackagesOverlay`](https://github.com/nix-community/nix-doom-emacs#Installing-emacs-packages). 
For example, with nix-doom-emacs managed via home-manager, `home.nix` could look like:
```
{ pkgs, ... }:

let
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/nix-community/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
                                # and packages.el files
    
    emacsPackagesOverlay = self: super: {
      lean4-mode = self.melpaBuild rec {
        pname = "lean4-mode";
        version = "1";
        commit = "37d5c99b7b29c80ab78321edd6773200deb0bca6";
        src = pkgs.fetchFromGitHub {
          owner = "leanprover";
          repo = "lean4-mode";
          rev = commit;
          sha256 = "sha256-+dRaXB7uvN/weSZiKcfSKWhcdJVNg9Vg8k0pJkDNjpc=";
        };
        packageRequires = with self.melpaPackages;
          [ dash f flycheck magit-section lsp-mode s ];
        recipe = pkgs.writeText "recipe" ''
                  (lean4-mode :repo "leanprover/lean4-mode" :fetcher github)
                '';
      };
    };
  };
in
{
  home-manager.users.your-user-name.home.packages = [ doom-emacs ];
}
```

Trying It Out
=============

If things are working correctly, you should see the word ``Lean 4`` in the
Emacs mode line when you open a file with extension `.lean`. Emacs will ask you
to identify the "project" this file belongs to. If you then type
```lean
#check id
```
the word ``#check`` will be underlined, and hovering over it will show
you the type of ``id``. The mode line will show ``FlyC:0/1``, indicating
that there are no errors and one piece of information displayed.

Settings
========

Set these with e.g. `M-x customize-variable`.

* `lsp-headerline-breadcrumb-enable`: show a "breadcrumb bar" of namespaces and sections surrounding the current location (default: off)

Key Bindings and Commands
=========================

| Key                | Function                                                                        |
|--------------------|---------------------------------------------------------------------------------|
| <kbd>C-c C-k</kbd> | show the keystroke needed to input the symbol under the cursor                  |
| <kbd>C-c C-d</kbd> | recompile & reload imports (`lean4-refresh-file-dependencies`)                  |
| <kbd>C-c C-x</kbd> | execute Lean in stand-alone mode (`lean4-std-exe`)                              |
| <kbd>C-c C-p C-l</kbd> | builds package with lake (`lean4-lake-build`)                                   |
| <kbd>C-c C-i</kbd> | toggle info view showing goals and errors at point (`lean4-toggle-info-buffer`) |
| <kbd>C-c ! n</kbd> | flycheck: go to next error                                                      |
| <kbd>C-c ! p</kbd> | flycheck: go to previous error                                                  |

For `lsp-mode` bindings, see https://emacs-lsp.github.io/lsp-mode/page/keybindings/ (not all capabilities are supported currently).

In the default configuration, the Flycheck annotation `FlyC:n/n` indicates the
number of errors / responses from Lean; clicking on `FlyC` opens the Flycheck menu.
