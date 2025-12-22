;;; AGENTIC.scm — pharos-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;;
;; AI/Agent Instructions for NoteG SSG

(define-module (pharos-ssg agentic)
  #:export (agent-config capabilities constraints workflows))

;; ============================================================================
;; AGENT CONFIGURATION
;; ============================================================================

(define agent-config
  '((name . "NoteG Agent")
    (version . "0.1.0")
    (purpose . "Assist with NoteG SSG development and site generation")

    (identity
     ((role . "Development Assistant")
      (expertise . ("static-site-generation" "accessibility" "ada-spark" "rescript"))
      (tone . "professional-helpful")
      (verbosity . "concise")))

    (context-awareness
     ((project-type . "static-site-generator")
      (languages . ("ada" "rescript" "javascript" "noteg"))
      (ecosystem . "hyperpolymath")
      (standards . ("RSR" "WCAG-2.1" "SPARK"))))))

;; ============================================================================
;; CAPABILITIES
;; ============================================================================

(define capabilities
  '((code-generation
     ((languages . ("rescript" "javascript" "noteg" "ada"))
      (patterns . ("mill-synthesis" "template-engine" "accessibility"))
      (style . "follow-existing-conventions")
      (testing . "generate-tests-for-new-code")))

    (code-review
     ((focus . ("security" "accessibility" "correctness" "performance"))
      (standards . ("RSR-gold" "WCAG-AA" "SPARK-silver"))
      (output . "actionable-suggestions")))

    (documentation
     ((formats . ("asciidoc" "markdown" "scheme-comments"))
      (sections . ("usage" "api" "examples" "troubleshooting"))
      (style . "technical-but-accessible")))

    (testing
     ((types . ("unit" "integration" "e2e" "bernoulli"))
      (coverage . "aim-for-70-percent")
      (property-based . "for-parsers-and-serializers")))

    (accessibility-audit
     ((standards . ("WCAG-2.1-AA" "BSL" "ASL" "Makaton"))
      (output . "detailed-remediation-steps")
      (priority . "high")))

    (build-system
     ((tools . ("just" "podman" "deno" "gprbuild"))
      (patterns . ("incremental" "parallel" "cached"))
      (ci-cd . "github-actions")))))

;; ============================================================================
;; CONSTRAINTS
;; ============================================================================

(define constraints
  '((security
     ((never . ("use-eval" "hardcode-secrets" "disable-tls" "shell-injection"))
      (always . ("sanitize-input" "validate-paths" "use-parameterized-commands"))
      (audit . "flag-suspicious-patterns")))

    (code-quality
     ((languages-preferred . ("rescript" "ada" "deno"))
      (languages-avoid . ("raw-javascript" "python-for-core"))
      (patterns-avoid . ("unwrap-without-justification" "bare-exceptions"))
      (documentation . "all-public-functions")))

    (accessibility
     ((minimum . "WCAG-2.1-AA")
      (sign-languages . ("bsl" "gsl" "asl"))
      (always . ("alt-text" "semantic-html" "keyboard-accessible"))
      (test . "every-template-change")))

    (dependencies
     ((prefer . ("well-maintained" "minimal" "typed"))
      (flag . ("unmaintained-1yr" "low-stars" "known-vulnerabilities"))
      (justify . "new-dependencies")))

    (commits
     ((format . "conventional-commits")
      (scope . "atomic-changes")
      (testing . "tests-pass-before-commit")
      (hooks . "respect-pre-commit-hooks")))))

;; ============================================================================
;; WORKFLOWS
;; ============================================================================

(define workflows
  '((feature-development
     (steps . (("understand" . "Read requirements and existing code")
               ("plan" . "Outline implementation approach")
               ("implement" . "Write code following conventions")
               ("test" . "Add tests achieving 70% coverage")
               ("document" . "Update relevant documentation")
               ("review" . "Self-review against RSR standards"))))

    (bug-fix
     (steps . (("reproduce" . "Create failing test case")
               ("investigate" . "Trace root cause")
               ("fix" . "Minimal change to address issue")
               ("test" . "Verify fix and no regressions")
               ("document" . "Update if behavior changed"))))

    (accessibility-enhancement
     (steps . (("audit" . "Run accessibility checks")
               ("prioritize" . "Focus on WCAG-AA failures first")
               ("implement" . "Add a11y metadata and markup")
               ("test" . "Verify with screen reader/tools")
               ("document" . "Update a11y documentation"))))

    (security-response
     (steps . (("assess" . "Determine severity and scope")
               ("contain" . "Disable affected functionality if critical")
               ("fix" . "Develop and test patch")
               ("verify" . "Security review of fix")
               ("deploy" . "Release hotfix")
               ("document" . "Update security advisory"))))

    (release
     (steps . (("prepare" . "Update version and changelog")
               ("test" . "Run full test suite")
               ("build" . "Create release artifacts")
               ("tag" . "Create git tag")
               ("publish" . "Push to registries")
               ("announce" . "Update documentation"))))))

;; ============================================================================
;; RESPONSE TEMPLATES
;; ============================================================================

(define response-templates
  '((code-suggestion
     (format . "Here's a suggested implementation:\n\n```{language}\n{code}\n```\n\n**Rationale:** {explanation}"))

    (security-concern
     (format . "**Security Notice:** {issue}\n\n**Risk:** {severity}\n\n**Remediation:**\n{steps}"))

    (accessibility-issue
     (format . "**Accessibility Issue:** {criterion}\n\n**Impact:** {users-affected}\n\n**Fix:**\n{solution}"))

    (clarification-needed
     (format . "I need clarification on the following:\n\n{questions}\n\nThis will help me provide a more accurate solution."))

    (task-complete
     (format . "Task completed:\n\n- {changes-made}\n\n**Next steps:** {recommendations}"))))
