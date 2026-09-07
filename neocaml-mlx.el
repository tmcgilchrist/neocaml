;;; neocaml-mlx.el --- Major mode for OCaml/JSX (.mlx) files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov <bozhidar@batsov.dev>
;;
;; Author:  Akira Komamura <akira.komamura@gmail.com>
;;          Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml ml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing OCaml files that embed
;; JSX syntax (Melange/React and similar PPX-driven OCaml-JSX setups),
;; conventionally given the `.mlx' extension.

;; The host grammar is the ordinary `ocaml' grammar, so every feature
;; of `neocaml-mode' (font-lock, indentation, navigation, imenu,
;; compilation, ...) is available unchanged.  On top of that, when the
;; `tsx' tree-sitter grammar is installed, the JSX subtrees are
;; highlighted via language injection: Emacs detects `let' bindings
;; carrying a configurable JSX-transform attribute (by default
;; `[@react.component]'), finds the JSX element they contain, and feeds
;; just that region to a `tsx' parser, reusing the built-in
;; `typescript-ts-mode' font-lock rules for the embedded JSX.  See
;; `neocaml-mlx-jsx-attribute-regexp' to teach it about other PPXes.
;;
;; For the host grammar, see
;; https://github.com/tree-sitter/tree-sitter-ocaml.
;; For the embedded JSX grammar, see
;; https://github.com/tree-sitter/tree-sitter-typescript (the `tsx'
;; language).

;; Limitations:
;;
;; The OCaml grammar itself has no notion of JSX, so it parses `<div>
;; ... </div>' as a tangle of infix expressions and `ERROR' nodes.
;; JSX-aware indentation is provided by appending tsx indent rules
;; for the injected JSX regions, but the boundaries between OCaml and
;; JSX may still produce imprecise results in some cases.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'treesit)
(require 'neocaml)
(require 'typescript-ts-mode)

(declare-function neocaml--setup-mode "neocaml")
(declare-function typescript-ts-mode--font-lock-settings
                  "typescript-ts-mode" (language))
(declare-function tsx-ts-mode--font-lock-compatibility-bb1f97b
                  "typescript-ts-mode" (language))
(defvar neocaml--imenu-settings)

(defgroup neocaml-mlx nil
  "Major mode for editing OCaml/JSX (.mlx) files with tree-sitter."
  :prefix "neocaml-mlx-"
  :group 'neocaml
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

;;; Grammar installation

(defconst neocaml-mlx-grammar-recipes
  '((tsx "https://github.com/tree-sitter/tree-sitter-typescript"
         "v0.23.2"
         "tsx/src"))
  "Tree-sitter grammar recipe for the `tsx' (JSX) language.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.

The host `ocaml' grammar is installed by `neocaml-install-grammars'.")

(defun neocaml-mlx-install-grammar (&optional force)
  "Install the `tsx' tree-sitter grammar if not already available.
The host `ocaml' grammar is a prerequisite and is installed separately
via `neocaml-install-grammars'.  With prefix argument FORCE, reinstall
even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'tsx nil)))
    (message "Installing tsx (JSX) tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-mlx-grammar-recipes))
      (treesit-install-language-grammar 'tsx))))

;;; JSX injection

(defcustom neocaml-mlx-jsx-attribute-regexp "react\\.component"
  "Regexp matched against an OCaml attribute id to detect JSX bindings.
When a `let' binding carries an attribute whose id matches this regexp
\(the `react.component' part of `[@react.component]'), neocaml-mlx
injects the `tsx' grammar into its body so the embedded JSX is
highlighted.  Extend this to support other JSX-transform PPXes, e.g.
\"\\\\(react\\\\.component\\\\|jsx\\\\)\".

The match is performed against the text of the `attribute_id' node,
which excludes the surrounding `[@' and `]'."
  :type 'regexp
  :group 'neocaml-mlx
  :package-version '(neocaml . "0.11.0"))

(defun neocaml-mlx--injection-available-p ()
  "Non-nil if `tsx' language injection is available.
Requires Emacs 30+ and the `tsx' tree-sitter grammar."
  (and (>= emacs-major-version 30)
       (treesit-language-available-p 'tsx)))

(defconst neocaml-mlx--non-code-node-types
  '("comment" "string" "string_content" "quoted_string"
    "quoted_string_content" "character" "character_content")
  "OCaml node types whose text must never be read as JSX.
A `<' inside a comment or a string literal does not open a tag.")

(defun neocaml-mlx--code-position-p (pos)
  "Non-nil when POS is ordinary OCaml code.
Return nil inside a comment, string or character literal, as classified
by the host `ocaml' parser.  The parse tree is consulted rather than
`syntax-ppss' so this stays safe to call from a range function, where
`syntax-propertize' may be mid-flight."
  (let ((node (treesit-node-at pos 'ocaml)))
    (not (and node
              (member (treesit-node-type node)
                      neocaml-mlx--non-code-node-types)))))

(defun neocaml-mlx--search-jsx-start (limit)
  "Search forward from point for the `<' that opens a JSX element.
Return its position, or nil when none is found before LIMIT.  Matches
inside comments and string literals are skipped, so `(* <a *)' and
`let s = \"<div>\"' do not start a JSX region."
  (let ((found nil))
    (while (and (null found)
                (re-search-forward (rx "<" (in "A-Za-z_")) limit t))
      (let ((start (match-beginning 0)))
        (when (neocaml-mlx--code-position-p start)
          (setq found start))))
    found))

(defconst neocaml-mlx--jsx-tag-regexp
  (rx (or (group "</" (* (not (any "<>"))) ">")
          (group "<" (in "A-Za-z_") (*? (not (any "<"))) "/>")
          (group "<" (in "A-Za-z_") (*? (not (any "<"))) ">")))
  "Regexp matching a single JSX tag.
Group 1 matches a closing tag, group 2 a self-closing tag, and group 3
an opening tag.")

(defun neocaml-mlx--jsx-end (start limit)
  "Return the position just past the JSX element that begins at START.
Tags are balanced textually instead of being taken from the host parse
tree: the OCaml grammar has no notion of JSX, and its error recovery
routinely ends a binding in the middle of a closing tag or before the
`/>' of a self-closing one.  Scan no further than LIMIT.  Return nil
when the element never closes."
  (save-excursion
    (goto-char start)
    (let ((depth 0)
          (end nil))
      (while (and (null end)
                  (re-search-forward neocaml-mlx--jsx-tag-regexp limit t))
        (cond
         ((match-beginning 1)
          (setq depth (1- depth))
          (when (<= depth 0)
            (setq end (match-end 0))))
         ((match-beginning 2)
          (when (zerop depth)
            (setq end (match-end 0))))
         (t
          (setq depth (1+ depth)))))
      end)))

(defun neocaml-mlx--jsx-range (node limit)
  "Return the JSX region belonging to NODE as a cons of (BEG . END).
NODE is the JSX-transform `attribute' of a component binding.  The
search starts at NODE and runs no further than LIMIT, which should be
the start of the next component's attribute, or `point-max' for the last
one.  Return nil when no complete JSX element is found.

Neither boundary comes from NODE.  The OCaml parser mis-parses JSX as a
tangle of infix expressions and `ERROR' nodes, so for a self-closing
element, or a component that binds its JSX to a local before returning
it, the JSX sits outside NODE's extent entirely."
  (save-restriction
    ;; Range functions can run while `syntax-propertize' has narrowed the
    ;; buffer.  Searching within that restriction would miss JSX.
    (widen)
    (save-excursion
      (goto-char (treesit-node-start node))
      (when-let* ((start (neocaml-mlx--search-jsx-start limit))
                  (end (neocaml-mlx--jsx-end start limit)))
        (cons start end)))))

(defvar neocaml-mlx--component-query-cache nil
  "Cons of (REGEXP . QUERY) caching the compiled component query.")

(defun neocaml-mlx--component-query ()
  "Return the compiled tree-sitter query matching JSX component bindings.
The query is compiled once and reused until
`neocaml-mlx-jsx-attribute-regexp' changes.  `treesit-update-ranges'
runs this on every jit-lock chunk and on every indent command, so
rebuilding and recompiling the query per call is a per-keystroke cost.

The `attribute' is captured rather than the `let_binding': after error
recovery the JSX is frequently not inside the binding at all, so the
binding's extent is no use.  The capture has to sit at the same paren
level as the predicate that filters it; capturing the enclosing
`value_definition' instead puts the two in separate patterns, which
Emacs 30 rejects at query time even though Emacs 31 accepts it."
  (let ((regexp neocaml-mlx-jsx-attribute-regexp))
    (unless (equal (car neocaml-mlx--component-query-cache) regexp)
      (setq neocaml-mlx--component-query-cache
            (cons regexp
                  (treesit-query-compile
                   'ocaml
                   `((value_definition
                      (attribute (attribute_id) @_jsx_attr
                                 (:match ,regexp @_jsx_attr)) @mlx))))))
    (cdr neocaml-mlx--component-query-cache)))

(defun neocaml-mlx--set-ranges (_start _end)
  "Set the `tsx' parser's included ranges for JSX component bindings.
START and END are described in `treesit-range-rules' and are ignored:
`treesit-parser-set-included-ranges' replaces the entire range list, so
every component in the buffer has to be recomputed together.

Each component's JSX region is bounded by the start of the next
component, so a region can extend past the host node's end."
  (save-restriction
    ;; Range functions can run while syntax-propertize has narrowed the
    ;; buffer.  Querying in that restriction both misses definitions and
    ;; can let query predicates invoke syntax-propertize out of order.
    (widen)
    (let* ((tsx-parser (treesit-parser-create 'tsx))
           (nodes (sort (treesit-query-capture
                         (treesit-buffer-root-node 'ocaml)
                         (neocaml-mlx--component-query)
                         nil nil t)
                        (lambda (a b)
                          (< (treesit-node-start a) (treesit-node-start b)))))
           (ranges nil))
      (while nodes
        (let* ((node (pop nodes))
               (limit (if nodes
                          (treesit-node-start (car nodes))
                        (point-max)))
               (range (neocaml-mlx--jsx-range node limit)))
          (when range (push range ranges))))
      (setq ranges (nreverse ranges))
      ;; An empty list makes a parser cover the whole buffer, so use a
      ;; degenerate range when there is no JSX to parse.
      (treesit-parser-set-included-ranges
       tsx-parser (or ranges `((,(point-min) . ,(point-min))))))))

(defun neocaml-mlx--range-settings ()
  "Return range settings for injecting `tsx' into OCaml JSX regions.
Returns nil when injection is not available.  The ranges share a single
`tsx' parser; each detected component binding contributes one span
covering its JSX element."
  (when (neocaml-mlx--injection-available-p)
    (treesit-range-rules #'neocaml-mlx--set-ranges)))

(defun neocaml-mlx--tsx-parser ()
  "Return this buffer's `tsx' parser, or nil.
`treesit-parser-list' only grew its LANGUAGE argument in Emacs 30, and
this file still has to byte-compile on 29."
  (let ((parsers (treesit-parser-list))
        (found nil))
    (while (and parsers (null found))
      (let ((parser (pop parsers)))
        (when (eq (treesit-parser-language parser) 'tsx)
          (setq found parser))))
    found))

(defun neocaml-mlx--jsx-region-p (pos)
  "Non-nil when POS falls inside an injected JSX range."
  (when-let* ((parser (neocaml-mlx--tsx-parser)))
    (let ((ranges (treesit-parser-included-ranges parser))
          (found nil))
      (while (and ranges (not found))
        (let ((range (pop ranges)))
          (when (and (>= pos (car range)) (< pos (cdr range)))
            (setq found t))))
      found)))

(defun neocaml-mlx--tsx-indent-context (bol)
  "Return (NODE . PARENT) at BOL resolved against the `tsx' parser.
Mirrors what `treesit--indent-largest-node-at' does for an ordinary
embedded parser.  Return nil when there is no usable `tsx' node."
  (when-let* ((parser (neocaml-mlx--tsx-parser))
              (root (treesit-parser-root-node parser))
              (smallest (treesit-node-at bol parser)))
    (let* ((node (treesit-parent-while
                  smallest
                  (lambda (n)
                    (and (eq bol (treesit-node-start n))
                         (not (treesit-node-eq n root))))))
           (parent (if node
                       (treesit-node-parent node)
                     (treesit-node-on bol bol parser))))
      (and parent (cons node parent)))))

(defun neocaml-mlx--indent (node parent bol)
  "Indent BOL, resolving JSX positions against the `tsx' parser.
NODE and PARENT are as for `treesit-simple-indent'.

`treesit--indent-largest-node-at' locates nodes through
`treesit-parsers-at', which only knows about parsers registered via
range overlays.  This mode sets the `tsx' parser's ranges directly, so
inside an injected region NODE and PARENT arrive from the host `ocaml'
tree - where the JSX is a tangle of `ERROR' nodes - and the `tsx' indent
rules can never match.  Re-derive both from the `tsx' tree there, and
defer to the OCaml rules everywhere else."
  (let ((context (and (neocaml-mlx--jsx-region-p bol)
                      (neocaml-mlx--tsx-indent-context bol))))
    (if context
        (treesit-simple-indent (car context) (cdr context) bol)
      (treesit-simple-indent node parent bol))))

;;; Font-lock

(defface neocaml-mlx-jsx-tag-delimiter-face
  '((t :inherit typescript-ts-jsx-tag-face))
  "Face used for JSX delimiters in `neocaml-mlx-mode'.
This complements `typescript-ts-jsx-tag-face' by colouring the `<', `</',
`>' and `/>' punctuation around JSX tags, which the default
`typescript-ts-mode' rules leave unfontified."
  :group 'neocaml-mlx
  :package-version '(neocaml . "0.11.0"))

(defun neocaml-mlx--tsx-font-lock-settings ()
  "Return `tsx' font-lock settings for embedded JSX in `neocaml-mlx-mode'.
Reuses Emacs' built-in `typescript-ts-mode' rules and adds an
overriding `jsx' rule so JSX tags, delimiters, and attributes win
out over the host `ocaml' grammar's faces on the same text, which
the OCaml parser mis-parses as ordinary identifiers/operators.
Returns nil when injection is not available."
  (when (neocaml-mlx--injection-available-p)
    (append
     (typescript-ts-mode--font-lock-settings 'tsx)
     (treesit-font-lock-rules
      :language 'tsx
      :feature 'jsx
      :override t
      (append
       ;; Tag names and attributes, adjusted to the installed tsx grammar
       ;; version.  These deliberately repeat the built-in `jsx' rules:
       ;; the copies here carry `:override t', which is what lets them
       ;; win over the OCaml faces already applied to the same text.
       (tsx-ts-mode--font-lock-compatibility-bb1f97b 'tsx)
       ;; Tag delimiters, which no built-in rule covers.
       '((jsx_opening_element ["<" ">"] @neocaml-mlx-jsx-tag-delimiter-face)
         (jsx_closing_element ["</" ">"] @neocaml-mlx-jsx-tag-delimiter-face)
         (jsx_self_closing_element ["<" "/>"] @neocaml-mlx-jsx-tag-delimiter-face)))))))

(defvar neocaml-mlx--tsx-feature-list nil
  "Cached `treesit-font-lock-feature-list' of `tsx-ts-mode'.")

(defun neocaml-mlx--tsx-feature-list ()
  "Return the `treesit-font-lock-feature-list' of `tsx-ts-mode'.
Computed once per session.  Obtaining it means activating the mode, and
doing that in a temporary buffer on every `.mlx' activation would run
`prog-mode-hook' and friends - lsp, flycheck, copilot - in a buffer that
is about to be killed."
  (or neocaml-mlx--tsx-feature-list
      (setq neocaml-mlx--tsx-feature-list
            (with-temp-buffer
              (delay-mode-hooks (tsx-ts-mode))
              treesit-font-lock-feature-list))))

(defun neocaml-mlx--merge-feature-lists (a b)
  "Return a fresh feature list merging the levels of A and B.
Every level is freshly consed, so the result shares no structure with
either argument.  That matters: the inputs are the quoted literals
inside `neocaml.el' and `typescript-ts-mode.el', and a caller that
modified the merged list in place would otherwise corrupt font-lock for
every OCaml and TSX buffer in the session."
  (let ((levels (max (length a) (length b)))
        (result nil))
    (dotimes (i levels)
      ;; The trailing nil makes `append' copy its last argument too.
      (push (delete-dups (append (nth i a) (nth i b) nil)) result))
    (nreverse result)))

(defun neocaml-mlx--font-lock-feature-list ()
  "Return the feature list merging `ocaml' and `tsx' feature levels.
The current buffer is expected to have been configured by
`neocaml--setup-mode'.  The TSX list is obtained from `tsx-ts-mode' so
changes to either mode's feature levels are reflected here
automatically."
  (neocaml-mlx--merge-feature-lists
   treesit-font-lock-feature-list
   (neocaml-mlx--tsx-feature-list)))

;;; Indentation

(defun neocaml-mlx--jsx-indent-rules ()
  "Return JSX-specific indentation rules for `neocaml-mlx-mode'.
These rules are based on the neovim MLX support queries (indents.scm),
translated to the tsx grammar node names (jsx_opening_element,
jsx_closing_element, jsx_self_closing_element, jsx_expression, etc.).

The translation from neovim indent queries to Emacs treesit rules:
- @indent.begin on jsx_element_opening/self_closing/expression:
  children of these nodes are indented by `neocaml-indent-offset'.
- @indent.end on \">\" in jsx_element_closing and \"/>\" in self-closing:
  these delimiters end an indent block.
- @indent.branch on jsx_element_closing and \">\":
  closing elements and their delimiters align with the opening element.
- @indent.branch on \"/>\" in self-closing:
  the self-closing delimiter aligns with the opening tag."
  `(;; JSX closing elements align with parent jsx_element (branch)
    ((node-is "jsx_closing_element") parent-bol 0)

    ;; > delimiter aligns with parent opening element (branch/end)
    ((node-is ">") parent-bol 0)

    ;; /> delimiter aligns with parent self-closing element (branch/end)
    ((node-is "/>") parent-bol 0)

    ;; Content inside JSX elements is indented (begin)
    ((parent-is "jsx_element") parent-bol neocaml-indent-offset)

    ;; Content inside JSX fragments is indented (begin)
    ((parent-is "jsx_fragment") parent-bol neocaml-indent-offset)

    ;; Attributes on new lines in opening elements are indented (begin)
    ((parent-is "jsx_opening_element") parent-bol neocaml-indent-offset)

    ;; Attributes on new lines in self-closing elements are indented (begin)
    ((parent-is "jsx_self_closing_element") parent-bol neocaml-indent-offset)

    ;; Content inside JSX expressions { } is indented (begin)
    ((parent-is "jsx_expression") parent-bol neocaml-indent-offset)))

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-mlx-mode neocaml-base-mode "OCaml[MLX]"
  "Major mode for editing OCaml files with embedded JSX (.mlx).

`neocaml-mlx-mode' is `neocaml-mode' plus highlighting and indentation
for embedded JSX.  When the `tsx' tree-sitter grammar is installed,
`let' bindings marked with a JSX-transform attribute (see
`neocaml-mlx-jsx-attribute-regexp', default `[@react.component]') have
their JSX bodies highlighted via language injection using the built-in
`typescript-ts-mode' font-lock rules, with JSX-aware indentation rules
appended for the injected regions.  If the `tsx' grammar is absent,
the mode degrades gracefully to plain `neocaml-mode' behaviour.

\\{neocaml-base-mode-map}"
  ;; Offer to install the JSX grammar first: every step below is skipped
  ;; when the grammar is missing, so prompting at the end of the mode
  ;; body would install it and still leave this buffer without JSX
  ;; support until the mode was re-run.
  (when (and (>= emacs-major-version 30)
             (not (treesit-language-available-p 'tsx))
             (y-or-n-p "The tsx (JSX) tree-sitter grammar is not installed; \
JSX highlighting needs it.  Install it now?"))
    (neocaml-mlx-install-grammar))

  (setq-local treesit-simple-imenu-settings neocaml--imenu-settings)

  ;; Set up embedded-JSX injection before the OCaml setup creates the
  ;; OCaml parser and runs `treesit-major-mode-setup'; the range
  ;; settings reference the host `ocaml' grammar and are honoured once
  ;; that parser exists.
  (when (neocaml-mlx--injection-available-p)
    (setq-local treesit-range-settings (neocaml-mlx--range-settings)))

  ;; Full OCaml setup: ocaml parser, font-lock, indent, navigation, ...
  ;; This installs its own `treesit-font-lock-settings', so the tsx rules
  ;; have to be layered on afterwards.
  (neocaml--setup-mode 'ocaml)

  ;; Layer the tsx font-lock and indent rules on top of the OCaml ones.
  ;; The injected range is one contiguous span per component, from the
  ;; first tag to the end of the element, so the tsx parser covers the
  ;; text between tags too.  OCaml expressions in that text, such as
  ;; `(React.string "...")', keep their OCaml faces because the OCaml
  ;; rules run first and only the `jsx' rules below override.
  (when (neocaml-mlx--injection-available-p)
    (let ((tsx-settings (neocaml-mlx--tsx-font-lock-settings)))
      (when tsx-settings
        (setq-local treesit-font-lock-settings
                    (append treesit-font-lock-settings tsx-settings))
        (setq-local treesit-font-lock-feature-list
                    (neocaml-mlx--font-lock-feature-list))
        (treesit-font-lock-recompute-features)
        (font-lock-flush)))

    ;; Append JSX-specific indent rules for the tsx parser.
    ;; The OCaml indent rules (set up by `neocaml--setup-mode') handle
    ;; OCaml regions; these rules handle JSX regions parsed by tsx.
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        `((tsx ,@(neocaml-mlx--jsx-indent-rules)))))
    ;; Route indentation inside those regions to them.
    (setq-local treesit-indent-function #'neocaml-mlx--indent)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlx\\'" . neocaml-mlx-mode))

;; Eglot integration: `.mlx' files are OCaml as far as ocamllsp is
;; concerned, but eglot matches on `provided-mode-derived-p' and this
;; mode derives from `neocaml-base-mode', not `neocaml-mode'.
(put 'neocaml-mlx-mode 'eglot-language-id "ocaml")

(provide 'neocaml-mlx)

;;; neocaml-mlx.el ends here
