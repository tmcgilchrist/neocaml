;;; neocaml-mlx-test.el --- Tests for neocaml-mlx-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-mlx-mode': auto-mode association, mode
;; derivation, and `tsx' language injection for embedded JSX.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'neocaml)
(require 'neocaml-mlx)
(require 'neocaml-test-helpers)

(defconst neocaml-mlx-test--react-component
  "\
module App = struct

  let[@react.component] make () =
    <div>
      <h1> (React.string \"Hello, React.ml!\") </h1>
    </div>
  ;;
end"
  "A small OCaml/JSX component used across the specs.")

(defun neocaml-mlx-test--tsx-parser ()
  "Return the `tsx' parser active in the current buffer, or nil."
  (car (cl-remove-if-not
        (lambda (p) (eq (treesit-parser-language p) 'tsx))
        (treesit-parser-list))))

(defun neocaml-mlx-test--real-ranges ()
  "Return the non-degenerate included ranges of the current tsx parser."
  (when-let* ((parser (neocaml-mlx-test--tsx-parser))
              (ranges (treesit-parser-included-ranges parser)))
    (cl-remove-if (lambda (r) (= (car r) (cdr r))) ranges)))

(defun neocaml-mlx-test--injected-texts (source)
  "Return the text of each injected `tsx' range for SOURCE.
SOURCE is inserted into a `neocaml-mlx-mode' buffer and the ranges are
recomputed from scratch."
  (with-temp-buffer
    (insert source)
    (neocaml-mlx-mode)
    (treesit-update-ranges)
    (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cdr r)))
            (neocaml-mlx-test--real-ranges))))

(describe "neocaml-mlx-mode"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not installed")))

  (it "is associated with .mlx files"
    (expect (assoc "\\.mlx\\'" auto-mode-alist)
            :to-equal '("\\.mlx\\'" . neocaml-mlx-mode)))

  (it "derives from `neocaml-base-mode'"
    (with-temp-buffer
      (neocaml-mlx-mode)
      (expect (provided-mode-derived-p major-mode 'neocaml-base-mode)
              :to-be-truthy)
      (expect (provided-mode-derived-p major-mode 'prog-mode)
              :to-be-truthy)))


  (describe "when the tsx grammar is available"
    (before-all
      (unless (neocaml-mlx--injection-available-p)
        (signal 'buttercup-pending
                "tsx tree-sitter grammar or Emacs 30+ not available")))

    (it "configures `treesit-range-settings'"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (expect treesit-range-settings :to-be-truthy)))

    (it "enables the `jsx' font-lock feature"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (expect (memq 'jsx (apply #'append treesit-font-lock-feature-list))
                :to-be-truthy)))

    (it "preserves OCaml levels and enables JSX at level 3"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (expect (memq 'escape-sequence
                      (nth 2 treesit-font-lock-feature-list))
                :to-be-truthy)
        (expect (memq 'property
                      (nth 3 treesit-font-lock-feature-list))
                :to-be-truthy)
        (expect (memq 'jsx
                      (nth 2 treesit-font-lock-feature-list))
                :to-be-truthy)))

    (it "injects a tsx range covering the JSX element"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (treesit-update-ranges)
        (let ((ranges (neocaml-mlx-test--real-ranges)))
          (expect ranges :to-be-truthy)
          (expect (length ranges) :to-equal 1)
          (let* ((r (car ranges))
                 (text (buffer-substring (car r) (cdr r))))
            (expect (string-prefix-p "<div>" text) :to-be t)
            (expect (string-suffix-p "</div>" text) :to-be t)
            (expect (string-search "<h1>" text) :to-be-truthy)
            (expect (string-search "</h1>" text) :to-be-truthy)))))

    (it "computes injection ranges from the widened buffer"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (goto-char (point-min))
        (search-forward "<h1>")
        (let ((narrow-start (match-beginning 0))
              (narrow-end (match-end 0)))
          (narrow-to-region narrow-start narrow-end)
          (treesit-update-ranges)
          (expect (cons (point-min) (point-max))
                  :to-equal (cons narrow-start narrow-end))
          (widen)
          (let* ((range (car (neocaml-mlx-test--real-ranges)))
                 (text (buffer-substring (car range) (cdr range))))
            (expect text :to-equal
                    (concat "<div>\n"
                            "      <h1> (React.string \"Hello, React.ml!\") "
                            "</h1>\n"
                            "    </div>"))))))

    (it "does not inject into bindings without JSX"
      (with-neocaml-test-buffer neocaml-mlx-mode
          "let[@react.component] make () = print_endline \"plain\""
        (treesit-update-ranges)
        (expect (neocaml-mlx-test--real-ranges) :to-equal nil)))

    (it "fontifies JSX tag names with `typescript-ts-jsx-tag-face'"
      (with-temp-buffer
        (insert neocaml-mlx-test--react-component)
        (let ((treesit-font-lock-level 3))
          (neocaml-mlx-mode))
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "<div>")
        ;; The tag name sits between the delimiters.
        (expect (1+ (match-beginning 0))
                :to-have-face 'typescript-ts-jsx-tag-face)))))

(describe "neocaml-mlx JSX region detection"
  (before-all
    (unless (neocaml-mlx--injection-available-p)
      (signal 'buttercup-pending
              "tsx tree-sitter grammar or Emacs 30+ not available")))

  ;; The OCaml grammar has no notion of JSX, so the extent of the node the
  ;; query matches says very little about where the JSX actually is.  Each
  ;; of these shapes ends the host node in a different wrong place.

  (it "injects into a self-closing element"
    ;; Error recovery ends the binding before the ` />', so a backward
    ;; scan for `>' from the node end finds nothing at all.
    (expect (neocaml-mlx-test--injected-texts
             "let[@react.component] make () = <Foo bar=\"1\" />\n;;")
            :to-equal '("<Foo bar=\"1\" />")))

  (it "injects into an element whose closing tag ends the binding"
    (expect (neocaml-mlx-test--injected-texts
             "let[@react.component] make () = <div><h1>hi</h1></div>\n;;")
            :to-equal '("<div><h1>hi</h1></div>")))

  (it "injects when the JSX is bound to a local before being returned"
    ;; Recovers as (value_definition (attribute) (let_binding pattern:...))
    ;; plus a sibling ERROR node holding the JSX.
    (expect (neocaml-mlx-test--injected-texts
             (concat "let[@react.component] make () =\n"
                     "  let el = <div></div> in\n"
                     "  if a > b then el else el\n"
                     ";;"))
            :to-equal '("<div></div>")))

  (it "ignores a tag inside an OCaml comment"
    (expect (neocaml-mlx-test--injected-texts
             "let[@react.component] make () = (* <a *) <div></div>\n;;")
            :to-equal '("<div></div>")))

  (it "ignores a tag inside an OCaml string"
    (expect (neocaml-mlx-test--injected-texts
             "let[@react.component] make () = ignore \"<a>\"; <div></div>\n;;")
            :to-equal '("<div></div>")))

  (it "bounds each component region by the next component"
    (expect (neocaml-mlx-test--injected-texts
             (concat "let[@react.component] a () = <Foo />\n;;\n"
                     "let[@react.component] b () = <Bar></Bar>\n;;"))
            :to-equal '("<Foo />" "<Bar></Bar>")))

  (it "does not inject into a binding without JSX"
    (expect (neocaml-mlx-test--injected-texts
             "let[@react.component] make () = print_endline \"plain\"")
            :to-equal nil)))

(describe "neocaml-mlx JSX font-lock"
  (before-all
    (unless (neocaml-mlx--injection-available-p)
      (signal 'buttercup-pending
              "tsx tree-sitter grammar or Emacs 30+ not available")))

  (it "fontifies a self-closing tag, its delimiters and its attributes"
    (with-temp-buffer
      (insert "let[@react.component] make () = <div class=\"x\" />\n;;")
      (let ((treesit-font-lock-level 3))
        (neocaml-mlx-mode))
      (font-lock-ensure)
      (goto-char (point-min))
      (expect "div" :to-have-face 'typescript-ts-jsx-tag-face)
      (goto-char (point-min))
      (expect "class" :to-have-face 'typescript-ts-jsx-attribute-face)
      (goto-char (point-min))
      (expect "/>" :to-have-face 'neocaml-mlx-jsx-tag-delimiter-face))))

(describe "neocaml-mlx indentation"
  (before-all
    (unless (neocaml-mlx--injection-available-p)
      (signal 'buttercup-pending
              "tsx tree-sitter grammar or Emacs 30+ not available")))

  (it "resolves a position inside a JSX region to the tsx language"
    (with-temp-buffer
      (insert neocaml-mlx-test--react-component)
      (neocaml-mlx-mode)
      (treesit-update-ranges)
      (goto-char (point-min))
      (search-forward "<h1>")
      (expect (treesit-language-at (match-beginning 0)) :to-equal 'tsx)))

  (it "indents a nested JSX element under its parent"
    (with-temp-buffer
      (insert "let[@react.component] make () =\n  <div>\n<h1>hi</h1>\n  </div>\n;;")
      (neocaml-mlx-mode)
      (treesit-update-ranges)
      (goto-char (point-min))
      (search-forward "<h1>")
      (beginning-of-line)
      (indent-for-tab-command)
      (expect (current-indentation) :to-equal 4)))

  (it "aligns a closing tag with its opening element"
    (with-temp-buffer
      (insert "let[@react.component] make () =\n  <div>\n    <h1>hi</h1>\n</div>\n;;")
      (neocaml-mlx-mode)
      (treesit-update-ranges)
      (goto-char (point-min))
      (search-forward "</div>")
      (beginning-of-line)
      (indent-for-tab-command)
      (expect (current-indentation) :to-equal 2))))

(describe "neocaml-mlx font-lock feature list"
  (it "shares no structure with the lists it merges"
    ;; The inputs are the quoted literals inside neocaml.el and
    ;; typescript-ts-mode.el.  `cl-union' returns one of its arguments
    ;; unchanged when the other adds nothing, which would alias them.
    (let* ((a '((comment definition) (keyword string type)))
           (b '((comment declaration) (keyword string)))
           (merged (neocaml-mlx--merge-feature-lists a b)))
      (expect merged :to-equal '((comment definition declaration)
                                 (keyword string type)))
      (dolist (level (append a b))
        (dolist (merged-level merged)
          (expect (eq level merged-level) :to-be nil)))))

  (it "survives a caller modifying the merged list in place"
    ;; A destructive caller must not be able to rewrite the constants
    ;; that every neocaml-mode and tsx-ts-mode buffer shares.
    (let* ((a (list (list 'comment 'definition) (list 'keyword)))
           (b (list (list 'jsx) (list 'string)))
           (a-copy (copy-tree a))
           (b-copy (copy-tree b))
           (merged (neocaml-mlx--merge-feature-lists a b)))
      (apply #'nconc merged)
      (expect a :to-equal a-copy)
      (expect b :to-equal b-copy)))

  (it "does not call `treesit-merge-font-lock-feature-list'"
    ;; That function is new in Emacs 31, but injection is advertised for
    ;; Emacs 30+.  Make it explode to prove nothing reaches it.
    (cl-letf (((symbol-function 'treesit-merge-font-lock-feature-list)
               (lambda (&rest _)
                 (error "treesit-merge-font-lock-feature-list is Emacs 31+"))))
      (expect (neocaml-mlx--merge-feature-lists '((a)) '((b)))
              :to-equal '((a b)))))

  (it "computes the tsx feature list without running mode hooks"
    ;; Activating tsx-ts-mode in a temp buffer would otherwise start lsp,
    ;; flycheck and friends in a buffer that is about to be killed.
    (let ((neocaml-mlx--tsx-feature-list nil)
          (ran nil))
      (let ((hook (lambda () (setq ran t))))
        (add-hook 'prog-mode-hook hook)
        (unwind-protect
            (progn
              (neocaml-mlx--tsx-feature-list)
              (expect ran :to-be nil))
          (remove-hook 'prog-mode-hook hook)))))

  (it "computes the tsx feature list only once"
    (let ((neocaml-mlx--tsx-feature-list nil))
      (expect (eq (neocaml-mlx--tsx-feature-list)
                  (neocaml-mlx--tsx-feature-list))
              :to-be t))))

(describe "neocaml-mlx component query"
  (it "compiles the query once and reuses it"
    ;; treesit-update-ranges runs per jit-lock chunk and per indent, so a
    ;; query rebuilt each call is a per-keystroke cost.
    (let ((neocaml-mlx--component-query-cache nil))
      (expect (eq (neocaml-mlx--component-query)
                  (neocaml-mlx--component-query))
              :to-be t)))

  (it "recompiles when the attribute regexp changes"
    (let* ((neocaml-mlx--component-query-cache nil)
           (first (let ((neocaml-mlx-jsx-attribute-regexp "react\\.component"))
                    (neocaml-mlx--component-query)))
           (second (let ((neocaml-mlx-jsx-attribute-regexp "jsx"))
                     (neocaml-mlx--component-query))))
      (expect (eq first second) :to-be nil))))

(describe "neocaml-mlx grammar installation"
  (before-all
    (unless (neocaml-mlx--injection-available-p)
      (signal 'buttercup-pending
              "tsx tree-sitter grammar or Emacs 30+ not available")))

  (it "sets up injection in the activation that installs the grammar"
    ;; Prompting at the end of the mode body installed the grammar but
    ;; left the buffer without ranges until the mode was re-run.
    (let* ((installed nil)
           (real (symbol-function 'treesit-language-available-p)))
      (cl-letf (((symbol-function 'treesit-language-available-p)
                 (lambda (lang &optional detail)
                   (if (eq lang 'tsx)
                       installed
                     (funcall real lang detail))))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'neocaml-mlx-install-grammar)
                 (lambda (&rest _) (setq installed t))))
        (with-temp-buffer
          (insert neocaml-mlx-test--react-component)
          (neocaml-mlx-mode)
          (expect treesit-range-settings :to-be-truthy)))))

  (it "does not prompt when the grammar is already installed"
    (let ((prompted nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq prompted t) nil)))
        (with-temp-buffer
          (insert neocaml-mlx-test--react-component)
          (neocaml-mlx-mode))
        (expect prompted :to-be nil)))))

(describe "neocaml-mlx editor integration"
  (it "uses the ocaml eglot language id"
    (expect (get 'neocaml-mlx-mode 'eglot-language-id) :to-equal "ocaml"))

  (it "registers .mlx buffers with eglot"
    ;; eglot matches on `provided-mode-derived-p', and this mode derives
    ;; from neocaml-base-mode rather than neocaml-mode, so it has to be
    ;; named explicitly or .mlx files get no ocamllsp.
    (unless (require 'eglot nil t)
      (signal 'buttercup-pending "eglot not available"))
    (with-temp-buffer (neocaml-mlx-mode))
    (expect (cl-find-if
             (lambda (entry)
               (and (listp (car entry))
                    (cl-find-if (lambda (m)
                                  (and (consp m) (eq (car m) 'neocaml-mlx-mode)))
                                (car entry))))
             eglot-server-programs)
            :to-be-truthy)))

(provide 'neocaml-mlx-test)

;;; neocaml-mlx-test.el ends here
