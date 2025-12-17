;;; STATE.scm — pharos-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "0.2.0") (updated . "2025-12-17") (project . "pharos-ssg")))

(define current-position
  '((phase . "v0.2 - Core Implementation")
    (overall-completion . 40)
    (components
      ((rsr-compliance ((status . "complete") (completion . 100)))
       (scm-configuration ((status . "complete") (completion . 100)))
       (security-policy ((status . "complete") (completion . 100)))
       (adapters ((status . "complete") (completion . 100) (count . 28)))
       (testing ((status . "pending") (completion . 0)))
       (documentation ((status . "in-progress") (completion . 30)))))))

(define blockers-and-issues '((critical ()) (high-priority ())))

(define critical-next-actions
  '((immediate
      (("Add adapter tests" . high)
       ("Create integration tests" . high)))
    (this-week
      (("Add CI/CD pipeline for tests" . medium)
       ("Expand documentation" . medium)))))

(define roadmap
  '((v0.1 (title . "Initial Setup")
          (status . "complete")
          (items . ("RSR compliance" "SCM files" "Security policy" "Adapter sync")))
    (v0.2 (title . "Core Implementation")
          (status . "in-progress")
          (items . ("Security hardening" "Adapter validation" "Basic documentation")))
    (v0.3 (title . "Testing & Quality")
          (status . "pending")
          (items . ("Unit tests" "Integration tests" "70% coverage" "CI pipeline")))
    (v0.4 (title . "Documentation & Polish")
          (status . "pending")
          (items . ("API documentation" "Usage examples" "Contributing guide update")))
    (v1.0 (title . "Stable Release")
          (status . "pending")
          (items . ("Full test coverage" "Performance optimization" "Release automation")))))

(define session-history
  '((snapshots
      ((date . "2025-12-15") (session . "initial") (notes . "SCM files added"))
      ((date . "2025-12-16") (session . "integration") (notes . "poly-ssg-mcp integration"))
      ((date . "2025-12-17") (session . "security-review") (notes . "Security audit, SCM fixes, roadmap update")))))

(define state-summary
  '((project . "pharos-ssg")
    (completion . 40)
    (blockers . 0)
    (updated . "2025-12-17")
    (next-milestone . "v0.3 - Testing & Quality")))
