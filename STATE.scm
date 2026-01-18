;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;; STATE.scm - Project state for pharos-ssg

(state
  (metadata
    (version "1.0.0")
    (schema-version "1.0")
    (created "2024-12-29")
    (updated "2025-01-18")
    (project "pharos-ssg")
    (repo "hyperpolymath/pharos-ssg"))

  (project-context
    (name "pharos-ssg")
    (tagline "Live object-oriented static site generator in Pharo Smalltalk")
    (tech-stack ("pharo" "smalltalk" "pharo-12")))

  (current-position
    (phase "implemented")
    (overall-completion 100)
    (components
      ((core-engine 100 "Pharo SSG core in PharosSSG.st")
       (live-image 100 "Persistent live environment")
       (object-graph 100 "Site as navigable object hierarchy")
       (reflective-tools 100 "Inspector-based site debugging")))
    (working-features
      ("Live image synthesis"
       "Object-graph site structure"
       "Reflective template inspection"
       "Instant change propagation"
       "Headless CI/CD builds"
       "State persistence across sessions"
       "MCP protocol support")))

  (route-to-mvp
    (milestones
      ((m1 "Core Implementation" completed
           (items
             ("Pharo image setup"
              "Object-to-HTML mapping"
              "Live template system")))
       (m2 "Reflective Features" completed
           (items
             ("Inspector integration"
              "Broken link detection"
              "Template refactoring")))
       (m3 "Integration" completed
           (items
             ("Headless build mode"
              "poly-ssg-mcp adapter"
              "Documentation"))))))

  (blockers-and-issues
    (critical ())
    (high ())
    (medium ())
    (low ()))

  (critical-next-actions
    (immediate
      ("Interactive tutorial image"
       "CI/CD pipeline examples"))
    (this-week
      ("Pharo 13 compatibility test"
       "Object persistence guide"))
    (this-month
      ("Browser-based Pharo integration"
       "Collaborative editing features"))))
