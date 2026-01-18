;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;; META.scm - Meta-level information for pharos-ssg

(define project-meta
  `((version . "1.0.0")

    (architecture-decisions
      ((adr-001
         (title . "Pharo Smalltalk as Implementation Language")
         (status . "accepted")
         (date . "2024-12-29")
         (context . "Need a fully reflective environment for interactive site development")
         (decision . "Use Pharo 12+ for live image-based development")
         (consequences . ("Everything is an object, including the generator"
                          "Changes reflected instantly without rebuild"
                          "Requires Pharo VM installation"
                          "State persists across sessions")))
       (adr-002
         (title . "Site as Object Graph")
         (status . "accepted")
         (date . "2024-12-29")
         (context . "File-based content is disconnected from the generator")
         (decision . "Represent site structure as a live object graph in the image")
         (consequences . ("Navigate site with Pharo inspector"
                          "Refactor templates as regular objects"
                          "Broken links detected immediately"
                          "Memory footprint for large sites")))
       (adr-003
         (title . "Headless Build for CI/CD")
         (status . "accepted")
         (date . "2024-12-29")
         (context . "Production builds should not require GUI")
         (decision . "Support headless Pharo execution for CI/CD pipelines")
         (consequences . ("Same image used interactively and in CI"
                          "Build reproducibility via image snapshots"
                          "Pipeline integration via command-line interface")))))

    (development-practices
      ((code-style . "pharo-standard")
       (security . "openssf-scorecard")
       (testing . "sunit-tests")
       (versioning . "semver")
       (documentation . "asciidoc")
       (branching . "trunk-based")))

    (design-rationale
      ((why-pharo . "Live programming environment enables interactive site development")
       (why-pharos . "A lighthouse guiding through the sea of static files")
       (why-object-graph . "Sites are naturally hierarchical objects, not flat files")))))
