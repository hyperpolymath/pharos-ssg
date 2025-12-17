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
  '((why-rsr "RSR ensures consistency, security, and maintainability.")))
