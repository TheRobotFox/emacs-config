;;; my-haskell-workflow.el --- Recover Haskell sessions when loading -*- lexical-binding: t; -*-

;;; Commentary:
;; A retained session can have missing process state or an exited GHCi child.
;; Repair it on explicit load/reload before Haskell mode queues the command.

;;; Code:
(defun my/haskell-ensure-process-for-load (&rest _)
  "Ensure the current Haskell session has a running process before loading.
Keep healthy sessions intact.  Rebuild broken state with an empty queue so
commands left over from the previous process are not replayed."
  (let* ((session (haskell-session))
         (state (haskell-session-process session))
         (process (and state (haskell-process-process state))))
    (unless (and (processp process) (process-live-p process))
      ;; Initialize state before starting: the upstream restart path writes
      ;; to it if an orphaned process still exists under the session name.
      (haskell-session-set-process session
                                   (haskell-process-make (haskell-session-name session)))
      (haskell-process-start session))))

(with-eval-after-load 'haskell
  (advice-add 'haskell-process-file-loadish :before #'my/haskell-ensure-process-for-load))

(provide 'my-haskell-workflow)
;;; my-haskell-workflow.el ends here
