;;; META.scm — pharos-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — pharos-ssg

(define-module (pharos-ssg meta)
  #:export (architecture-decisions development-practices design-rationale))

(define architecture-decisions
  '((adr-001
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Pharos-SSG satellite in hyperpolymath ecosystem")
     (decision . "Follow Rhodium Standard Repository guidelines")
     (consequences . ("RSR Gold target" "SHA-pinned actions" "SPDX headers" "Multi-platform CI")))
    (adr-002
     (title . "MCP Adapter Architecture")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Need unified interface for 28 SSG tools")
     (decision . "Use Deno-based adapters synced from poly-ssg-mcp hub")
     (consequences . ("Consistent MCP interface" "Deno runtime dependency" "Adapter synchronization required")))))

(define development-practices
  '((code-style (languages . ("javascript" "typescript")) (formatter . "deno fmt") (linter . "deno lint"))
    (security (sast . "CodeQL") (credentials . "env vars only") (no-eval . #t) (no-shell-injection . #t))
    (testing (coverage-minimum . 70) (framework . "deno test"))
    (versioning (scheme . "SemVer 2.0.0"))))

(define design-rationale
  '((why-mill-synthesis . "Deterministic, auditable, SPARK-verifiable builds")
    (why-ada-spark . "Formal verification for safety-critical code")
    (why-rescript . "Strong typing with JavaScript interop")
    (why-accessibility-first . "No user should be excluded from content")
    (why-rsr . "Consistency, security, and maintainability")))

(define component-matrix
  '((total-components . 44)
    (complete . 44)
    (categories
     (("Core Engine" . 4) ("Build System" . 4) ("Site Generation" . 4)
      ("Adapters" . 3) ("Accessibility" . 5) ("Testing" . 4)
      ("Documentation" . 8) ("Configuration" . 3) ("Language Tooling" . 6)
      ("Examples" . 3)))))
