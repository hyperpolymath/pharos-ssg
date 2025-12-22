;;; META.scm — pharos-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;;
;; NoteG SSG - Architecture Decisions and Development Practices

(define-module (pharos-ssg meta)
  #:export (architecture-decisions development-practices design-rationale component-matrix))

(define architecture-decisions
  '((adr-001
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Establish consistent project standards within hyperpolymath ecosystem")
     (decision . "Follow Rhodium Standard Repository guidelines at Gold level")
     (consequences . ("SHA-pinned GitHub Actions" "SPDX license headers" "Multi-platform CI/CD" "CodeQL scanning")))

    (adr-002
     (title . "Mill-Based Synthesis Architecture")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Need deterministic, verifiable build process for SSG")
     (decision . "Implement Babbage-inspired mill architecture with operation cards")
     (consequences . ("Deterministic builds" "Auditable traces" "SPARK-verifiable" "Functional state")))

    (adr-003
     (title . "Multi-Language Implementation")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Balance safety, expressiveness, and ecosystem integration")
     (decision . "Ada/SPARK for core engine, ReScript for SSG and language tooling")
     (consequences . ("Formal verification for safety-critical code" "Strong typing" "JavaScript interop")))

    (adr-004
     (title . "Accessibility-First Design")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Static sites should be accessible to all users")
     (decision . "Build-in support for sign languages (BSL, GSL, ASL) and Makaton")
     (consequences . ("Extended frontmatter schema" "Sign language video integration" "WCAG 2.1 AA default")))

    (adr-005
     (title . "NoteG Domain-Specific Language")
     (status . "accepted")
     (date . "2025-12-22")
     (context . "Need expressive yet safe templating language")
     (decision . "Create NoteG DSL with lexer, parser, compiler, interpreter, and LSP")
     (consequences . ("Rich editor support" "Safe template execution" "Compile-to-JS" "Pipe operator")))))

(define development-practices
  '((code-style
     (languages . (("ada" . "gnatpp") ("rescript" . "rescript-format") ("javascript" . "deno-fmt")))
     (line-length . 100)
     (indentation . 2))

    (security
     (sast . "CodeQL")
     (credentials . "environment-variables-only")
     (no-eval . #t)
     (no-shell-injection . #t)
     (input-sanitization . "adapters/_security.js"))

    (testing
     (coverage-minimum . 70)
     (frameworks . (("rescript" . "rescript-test") ("deno" . "deno-test") ("ada" . "aunit")))
     (types . ("unit" "integration" "e2e" "bernoulli")))

    (versioning
     (scheme . "SemVer 2.0.0"))))

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
