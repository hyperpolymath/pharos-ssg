;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;; ECOSYSTEM.scm - Ecosystem positioning for pharos-ssg

(ecosystem
  (version "1.0.0")
  (name "pharos-ssg")
  (type "ssg-engine")
  (purpose "Interactive live-image static site generator using Pharo Smalltalk's reflective environment")

  (position-in-ecosystem
    (role "satellite")
    (hub "poly-ssg-mcp")
    (domain "interactive-ssg")
    (uniqueness "Live image environment where generator and content share the same memory space"))

  (related-projects
    ((poly-ssg-mcp
       (relationship "hub")
       (description "Central MCP adapter hub for all poly-ssg engines"))
     (obli-ssg
       (relationship "sibling")
       (description "Privacy-focused Oberon SSG"))
     (terrapin-ssg
       (relationship "sibling")
       (description "Educational Logo-based SSG"))
     (iota-ssg
       (relationship "sibling")
       (description "Array-oriented APL-based SSG"))
     (befunge-ssg
       (relationship "sibling")
       (description "2D esoteric Befunge-based SSG"))
     (qed-ssg
       (relationship "sibling")
       (description "Multi-SSG adapter hub in Lean 4"))
     (parallax-ssg
       (relationship "sibling")
       (description "Parallel-first Chapel SSG"))))

  (what-this-is
    ("Static site generator in Pharo Smalltalk"
     "Live image with persistent state"
     "Site structure as navigable object graph"
     "Reflective debugging and inspection"
     "Instant template change propagation"
     "Part of poly-ssg-mcp family"))

  (what-this-is-not
    ("A traditional edit-compile-run SSG"
     "A lightweight command-line tool"
     "A file-based content system"
     "A dynamic web application framework")))
