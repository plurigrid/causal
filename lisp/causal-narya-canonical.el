;;; causal-narya-canonical.el --- Canonical Lisp proof schemas for N-awareness -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Codex
;; Keywords: languages, lisp, tools, proof
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This module packages a small library of canonical Lisp claims as
;; N-awareness proof schemas.
;;
;; The organizing idea is:
;;
;; - Topos-style structure differences give the ladder of awareness.
;; - Narya gives the proof vocabulary and proof bundle shape.
;; - Lisp supplies the canonical semantic claims that remain stable across
;;   concrete dialects: code/data correspondence, structural recursion,
;;   lexical scope, macro non-leakage, replay determinism, and
;;   local-to-global compositionality.
;;
;; The exported proof bundles use the same core fields that the local Narya
;; proof tooling already expects: `before', `after', `delta', `birth', and
;; `impact'.  This makes it straightforward to attach these canonical Lisp
;; schemas to event logs, proof sessions, or CatColab-style ologs.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function causal-proofreader--backend "causal-proofreader-utils")

(defgroup causal-narya-canonical nil
  "Canonical Lisp proof schemas for Narya-oriented workflows."
  :group 'applications
  :prefix "causal-narya-canonical-")

(defcustom causal-narya-canonical-schemata
  '((:id homoiconic-syntax
     :awareness 0
     :title "Syntax is inspectable data"
     :claim "Well-formed Lisp programs admit a canonical tree presentation."
     :canonical-lisp-fact
     "Programs can be inspected and transformed as the same class of structured objects they compute over."
     :proof-kind queue_consistency
     :invariants (queue_consistency gf3_conservation)
     :before ((surface . "reader forms")
              (structure . "token stream")
              (observation . "local syntax fragments"))
     :after ((surface . "sexp tree")
             (structure . "composable syntax object")
             (observation . "globally inspectable form"))
     :delta "Reader-level fragments become a stable structural object."
     :birth "Awareness begins when syntax is lifted into a shared tree world."
     :impact "Parsing, navigation, and proof state inspection become first-class."
     :topos-note "Local token differences glue into a point-free syntax object.")
    (:id structural-recursion
     :awareness 1
     :title "Finite cons structure supports induction"
     :claim "Functions defined by nil/cons descent admit structural induction on finite lists."
     :canonical-lisp-fact
     "Recursive programs over finite lists can be justified by a canonical induction principle."
     :proof-kind queue_consistency
     :invariants (queue_consistency replay_determinism)
     :before ((surface . "head-tail decomposition")
              (structure . "local recursive step")
              (observation . "one constructor at a time"))
     :after ((surface . "induction principle")
             (structure . "global fold or recursion law")
             (observation . "all finite branches covered"))
     :delta "Local recursive clauses are glued into a total induction schema."
     :birth "Awareness climbs when local recursion is seen as a global proof law."
     :impact "Termination arguments and fold laws become reusable proof assets."
     :topos-note "The list object is known through its local constructors and their glue."
     :lisp-principles (fold map structural-induction))
    (:id lexical-scope
     :awareness 2
     :title "Bindings form a stable local world"
     :claim "Alpha-renaming preserves meaning when capture is avoided."
     :canonical-lisp-fact
     "Lexically scoped bindings depend on structural position, not accidental variable names."
     :proof-kind non_leakage
     :invariants (non_leakage queue_consistency)
     :before ((surface . "binder names")
              (structure . "local environments")
              (observation . "free and bound occurrences"))
     :after ((surface . "alpha-equivalence class")
             (structure . "capture-avoiding substitution law")
             (observation . "binding topology preserved"))
     :delta "Names are quotient out in favor of binding structure."
     :birth "Awareness becomes relational when scope is understood as a local world."
     :impact "Refactoring, macro hygiene, and proof reuse stop depending on spelling accidents."
     :topos-note "Bindings are local patches whose overlaps are governed by substitution."
     :lisp-principles (lexical-scope alpha-equivalence substitution))
    (:id macro-non-leakage
     :awareness 3
     :title "Expansion should not leak authority across scopes"
     :claim "A hygienic or gensym-disciplined macro expansion preserves user bindings and avoids accidental capture."
     :canonical-lisp-fact
     "Macro systems are sound when generated names do not steal or leak lexical authority."
     :proof-kind non_leakage
     :invariants (non_leakage gf3_conservation)
     :before ((surface . "macro call site")
              (structure . "user bindings plus expansion template")
              (observation . "local syntax transformation"))
     :after ((surface . "expanded program")
             (structure . "separated user and generated scopes")
             (observation . "authority boundaries preserved"))
     :delta "Expansion becomes a controlled morphism instead of a name collision hazard."
     :birth "Awareness becomes social when generated code must respect neighboring scopes."
     :impact "Macros can be reasoned about as lawful world morphisms instead of ad hoc rewrites."
     :topos-note "Expansion is a gluing map that must preserve open neighborhoods of scope."
     :lisp-principles (macroexpansion hygiene gensym))
    (:id replay-determinism
     :awareness 4
     :title "Pure evaluation replays"
     :claim "For pure forms and a fixed lexical environment, repeated evaluation yields the same observable result."
     :canonical-lisp-fact
     "A pure Lisp fragment is replay-deterministic under a fixed environment and evaluation strategy."
     :proof-kind replay_determinism
     :invariants (replay_determinism queue_consistency)
     :before ((surface . "closed or environment-fixed form")
              (structure . "evaluation trace")
              (observation . "single execution"))
     :after ((surface . "stable value")
             (structure . "repeatable trace class")
             (observation . "same output on replay"))
     :delta "Single execution is lifted into a replay-invariant trace class."
     :birth "Awareness becomes temporal when one execution is compared with another."
     :impact "Proof traces, evaluator tests, and proof assistant sessions gain reproducible semantics."
     :topos-note "Time-indexed observations are glued into one stable section of behavior."
     :lisp-principles (purity replay evaluation))
    (:id proof-state-olog
     :awareness 5
     :title "Proof state exports to compositional world structure"
     :claim "Hypotheses and goals can be functorially exported into an olog-like object/aspect presentation."
     :canonical-lisp-fact
     "A proof state is not only a local tactic problem; it can be reflected into a compositional semantic object."
     :proof-kind gf3_conservation
     :invariants (gf3_conservation queue_consistency non_leakage)
     :before ((surface . "local hypotheses and current goal")
              (structure . "proof assistant buffer")
              (observation . "tactic-facing context"))
     :after ((surface . "olog objects and aspects")
             (structure . "collaborative categorical model")
             (observation . "local-to-global semantic map"))
     :delta "Proof context becomes a portable semantic object for collaborative reasoning."
     :birth "Awareness becomes world-level when proof state is exported beyond the tactic loop."
     :impact "Narya, Proof General, and CatColab can share one semantic presentation layer."
     :topos-note "This is the step from local proof obligations to a world of composable relations."
     :lisp-principles (proof-state olog compositionality)))
  "Canonical Lisp proof schemas organized by N-awareness.

Each schema is a plist with Narya-compatible proof bundle fields and a
Topos-style description of the structural jump it captures."
  :type '(repeat plist))

(defun causal-narya-canonical--sort (schemata)
  "Return SCHEMATA sorted by ascending awareness level."
  (sort (copy-sequence schemata)
        (lambda (left right)
          (< (plist-get left :awareness)
             (plist-get right :awareness)))))

(defun causal-narya-canonical-all ()
  "Return all canonical Lisp proof schemata in awareness order."
  (causal-narya-canonical--sort causal-narya-canonical-schemata))

(defun causal-narya-canonical-find (schema-id)
  "Return the schema identified by SCHEMA-ID, or nil if it is unknown."
  (seq-find (lambda (schema)
              (eq (plist-get schema :id) schema-id))
            (causal-narya-canonical-all)))

(defun causal-narya-canonical-for-awareness (awareness)
  "Return all schemata whose awareness is less than or equal to AWARENESS."
  (seq-filter (lambda (schema)
                (<= (plist-get schema :awareness) awareness))
              (causal-narya-canonical-all)))

(defun causal-narya-canonical-proof-bundle (schema-id)
  "Translate SCHEMA-ID into a compact Narya-style proof bundle.

The result is an alist so it can be serialized or attached to event logs
without pulling in extra dependencies."
  (when-let ((schema (causal-narya-canonical-find schema-id)))
    `((id . ,(symbol-name (plist-get schema :id)))
      (title . ,(plist-get schema :title))
      (awareness . ,(plist-get schema :awareness))
      (claim . ,(plist-get schema :claim))
      (canonical_lisp_fact . ,(plist-get schema :canonical-lisp-fact))
      (proof_kind . ,(symbol-name (plist-get schema :proof-kind)))
      (invariants . ,(mapcar #'symbol-name (plist-get schema :invariants)))
      (before . ,(plist-get schema :before))
      (after . ,(plist-get schema :after))
      (delta . ,(plist-get schema :delta))
      (birth . ,(plist-get schema :birth))
      (impact . ,(plist-get schema :impact))
      (topos_note . ,(plist-get schema :topos-note))
      (lisp_principles . ,(mapcar #'symbol-name (plist-get schema :lisp-principles))))))

(defun causal-narya-canonical-olog (schema-id)
  "Return a simple olog-style projection for SCHEMA-ID.

This is deliberately lightweight so it can be handed to CatColab-oriented
code or rendered in buffers without extra translation steps."
  (when-let ((schema (causal-narya-canonical-find schema-id)))
    `((objects
       . ((source . ,(format "a local proof situation about %s"
                             (plist-get schema :title)))
          (target . ,(format "a global Lisp semantic world where %s"
                             (downcase (plist-get schema :title))))
          (witness . ,(format "a proof bundle witnessing %s"
                              (plist-get schema :claim)))))
      (aspects
       . ((local_to_global . ,(plist-get schema :delta))
          (birth . ,(plist-get schema :birth))
          (impact . ,(plist-get schema :impact)))))))

(defun causal-narya-canonical-current-backend ()
  "Return the current proof backend when the proofreader machinery is available."
  (when (fboundp 'causal-proofreader--backend)
    (causal-proofreader--backend)))

(defun causal-narya-canonical-current-world (schema-id)
  "Return SCHEMA-ID annotated with the active proof backend.

This is the smallest useful bridge between the Narya-aware proofreader
layer and the canonical Lisp proof schemata defined here."
  (when-let ((bundle (causal-narya-canonical-proof-bundle schema-id)))
    (cons `(backend . ,(or (and (causal-narya-canonical-current-backend)
                                (symbol-name (causal-narya-canonical-current-backend)))
                           "unknown"))
          bundle)))

(defun causal-narya-canonical--format-schema (schema)
  "Return a human-readable rendering of SCHEMA."
  (mapconcat
   #'identity
   (delq nil
         (list (format "%s (N-%s)"
                       (plist-get schema :title)
                       (plist-get schema :awareness))
               ""
               (format "Claim: %s" (plist-get schema :claim))
               (format "Canonical Lisp fact: %s"
                       (plist-get schema :canonical-lisp-fact))
               (format "Proof kind: %s" (plist-get schema :proof-kind))
               (format "Invariants: %s"
                       (string-join
                        (mapcar #'symbol-name (plist-get schema :invariants))
                        ", "))
               (format "Birth: %s" (plist-get schema :birth))
               (format "Delta: %s" (plist-get schema :delta))
               (format "Impact: %s" (plist-get schema :impact))
               (format "Topos note: %s" (plist-get schema :topos-note))
               (when-let ((principles (plist-get schema :lisp-principles)))
                 (format "Lisp principles: %s"
                         (string-join (mapcar #'symbol-name principles) ", ")))))
   "\n"))

(defun causal-narya-canonical-read-schema ()
  "Prompt for a canonical schema id and return it as a symbol."
  (intern
   (completing-read
    "Canonical schema: "
    (mapcar (lambda (schema)
              (symbol-name (plist-get schema :id)))
            (causal-narya-canonical-all))
    nil t)))

(defun causal-narya-canonical-show (schema-id)
  "Display the canonical schema identified by SCHEMA-ID."
  (interactive (list (causal-narya-canonical-read-schema)))
  (if-let ((schema (causal-narya-canonical-find schema-id)))
      (with-help-window (help-buffer)
        (princ (causal-narya-canonical--format-schema schema))
        (princ "\n\nProof bundle:\n")
        (pp (causal-narya-canonical-proof-bundle schema-id))
        (princ "\nOlog:\n")
        (pp (causal-narya-canonical-olog schema-id)))
    (user-error "Unknown canonical schema: %s" schema-id)))

(defun causal-narya-canonical-show-awareness (awareness)
  "Display all schemata up to AWARENESS."
  (interactive "nMaximum awareness level: ")
  (with-help-window (help-buffer)
    (dolist (schema (causal-narya-canonical-for-awareness awareness))
      (princ (causal-narya-canonical--format-schema schema))
      (princ "\n\n"))))

(provide 'causal-narya-canonical)

;;; causal-narya-canonical.el ends here
