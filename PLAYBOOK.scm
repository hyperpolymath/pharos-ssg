;;; PLAYBOOK.scm — pharos-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;;
;; Operations Playbook for NoteG SSG

(define-module (pharos-ssg playbook)
  #:export (operations incidents runbooks alerts))

;; ============================================================================
;; OPERATIONS
;; ============================================================================

(define operations
  '((build
     (name . "Build Pipeline")
     (commands . ("just build" "just test" "just audit"))
     (timeout . 600)
     (retry . 3)
     (on-failure . "notify-maintainers"))

    (deploy
     (name . "Deployment")
     (commands . ("just container-build" "just container-push"))
     (requires . ("build" "test-all"))
     (timeout . 1200)
     (rollback . "deploy-previous"))

    (release
     (name . "Release Process")
     (steps . (("tag-version" . "git tag -a v$VERSION -m 'Release $VERSION'")
               ("build-release" . "MODE=release just build")
               ("run-tests" . "just test-all")
               ("create-release" . "gh release create v$VERSION")
               ("publish-container" . "just container-push")))
     (approval-required . #t))

    (hotfix
     (name . "Hotfix Process")
     (branch-pattern . "hotfix/*")
     (steps . (("create-branch" . "git checkout -b hotfix/$ISSUE")
               ("apply-fix" . "# implement fix")
               ("test" . "just test-all")
               ("merge-main" . "git checkout main && git merge hotfix/$ISSUE")
               ("merge-develop" . "git checkout develop && git merge hotfix/$ISSUE")
               ("tag" . "git tag -a v$VERSION -m 'Hotfix $VERSION'")))
     (expedited . #t))))

;; ============================================================================
;; INCIDENTS
;; ============================================================================

(define incidents
  '((severity-levels
     ((critical . ((response-time . 15)
                   (escalation . "immediate")
                   (notify . ("maintainers" "on-call"))))
      (high . ((response-time . 60)
               (escalation . "30-minutes")
               (notify . ("maintainers"))))
      (medium . ((response-time . 240)
                 (escalation . "4-hours")
                 (notify . ("team"))))
      (low . ((response-time . 1440)
              (escalation . "next-business-day")
              (notify . ("assignee"))))))

    (response-template
     ((acknowledge . "Incident acknowledged: $TITLE")
      (investigating . "Investigating root cause")
      (identified . "Root cause identified: $CAUSE")
      (fix-deployed . "Fix deployed, monitoring")
      (resolved . "Incident resolved, post-mortem scheduled")))

    (post-mortem-template
     ((sections . ("Summary" "Timeline" "Root Cause" "Impact" "Resolution" "Prevention"))))))

;; ============================================================================
;; RUNBOOKS
;; ============================================================================

(define runbooks
  '((build-failure
     (name . "Build Failure Recovery")
     (symptoms . ("CI fails" "Build timeout" "Dependency error"))
     (steps . (("check-logs" . "Review CI logs for error")
               ("identify-cause" . "Determine if code, deps, or infra")
               ("fix-code" . "If code issue, create fix PR")
               ("update-deps" . "If deps issue, update and test")
               ("retry" . "Rerun CI pipeline")))
     (escalation . "If unresolved after 30 min, escalate"))

    (spark-verification-failure
     (name . "SPARK Verification Failure")
     (symptoms . ("gnatprove fails" "Proof timeout" "Unsatisfied precondition"))
     (steps . (("review-proof" . "Check gnatprove output")
               ("analyze-precondition" . "Verify input constraints")
               ("add-assertions" . "Add loop invariants if needed")
               ("increase-timeout" . "Try --timeout=120")
               ("simplify-code" . "Refactor complex proofs")))
     (expertise . "SPARK/formal-methods"))

    (accessibility-regression
     (name . "Accessibility Regression")
     (symptoms . ("WCAG check fails" "A11y tests fail" "User report"))
     (steps . (("run-audit" . "pa11y site-url")
               ("identify-issue" . "Check which criterion fails")
               ("review-changes" . "git diff on templates")
               ("apply-fix" . "Update affected templates")
               ("verify" . "Rerun a11y tests")))
     (priority . high))

    (container-startup-failure
     (name . "Container Startup Failure")
     (symptoms . ("Container exits" "Health check fails" "Port not listening"))
     (steps . (("check-logs" . "podman logs container-id")
               ("verify-config" . "Check environment variables")
               ("test-locally" . "Run container with -it for debug")
               ("check-deps" . "Verify all dependencies available")
               ("rebuild" . "just container-build"))))))

;; ============================================================================
;; ALERTS
;; ============================================================================

(define alerts
  '((build-duration
     (metric . "build_duration_seconds")
     (threshold . 300)
     (operator . ">")
     (severity . medium)
     (message . "Build taking longer than 5 minutes"))

    (test-failure-rate
     (metric . "test_failure_rate")
     (threshold . 0.1)
     (operator . ">")
     (severity . high)
     (message . "Test failure rate above 10%"))

    (dependency-vulnerability
     (metric . "vuln_count_high")
     (threshold . 0)
     (operator . ">")
     (severity . critical)
     (message . "High severity vulnerability detected"))

    (accessibility-score
     (metric . "a11y_score")
     (threshold . 90)
     (operator . "<")
     (severity . high)
     (message . "Accessibility score below 90%"))))
