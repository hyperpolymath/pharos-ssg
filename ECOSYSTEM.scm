;;; ECOSYSTEM.scm — pharos-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;;
;; NoteG SSG - Ecosystem Position and Relationships

(define-module (pharos-ssg ecosystem)
  #:export (ecosystem-info related-projects integration-points))

(define ecosystem-info
  '((version . "1.0.0")
    (name . "pharos-ssg")
    (codename . "NoteG")
    (type . "static-site-generator")
    (purpose . "Mill-based SSG with accessibility-first design and formal verification")
    (language . "NoteG DSL")

    (position-in-ecosystem
     "NoteG SSG (pharos-ssg) is a satellite project in the hyperpolymath ecosystem.
      It implements a novel mill-based synthesis approach for static site generation,
      combining Ada/SPARK formal verification with ReScript's type safety.
      Primary focus on accessibility: BSL, GSL, ASL, Makaton support.")

    (distinguishing-features
     ((mill-synthesis . "Deterministic build via operation cards")
      (spark-verified . "Core engine formally verified in SPARK")
      (accessibility . "First-class BSL, GSL, ASL, Makaton support")
      (noteg-language . "Domain-specific language for templates and content")
      (mcp-integration . "Compatible with 28 SSG adapters via poly-ssg-mcp")))))

(define related-projects
  '((poly-ssg-mcp
     (url . "https://github.com/hyperpolymath/poly-ssg-mcp")
     (relationship . "hub")
     (description . "Unified MCP server for 28 static site generators")
     (integration . "Adapters sync from hub, MCP protocol for tooling"))

    (rhodium-standard-repositories
     (url . "https://github.com/hyperpolymath/rhodium-standard-repositories")
     (relationship . "standard")
     (description . "RSR compliance framework")
     (integration . "Gold-level compliance target"))))

(define integration-points
  '((mcp-protocol
     (description . "Model Context Protocol for AI/LLM tool integration")
     (adapters . 28)
     (sync-source . "poly-ssg-mcp"))

    (ci-cd
     (platform . "github-actions")
     (features . ("sha-pinned-actions" "codeql" "dependabot")))

    (container
     (runtime . "podman")
     (format . "OCI"))

    (editor-integration
     (lsp . "noteg-lang/src/lsp/Server.res")
     (editors . ("vscode" "neovim" "emacs")))))
