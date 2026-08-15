;;; magpi-core.el --- Lean Magpi domain model -*- lexical-binding: t; -*-

;; This file deliberately has no Pimacs or Magit dependency.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct magpi-attempt
  id name intent root profile model thinking authority context launch-spec
  backend-handle status activity observed-files last-response started-at)

(defun magpi-attempt-add-observed-file (attempt path)
  "Record PATH once on ATTEMPT and return ATTEMPT."
  (when (and (stringp path) (not (string-empty-p path)))
    (setf (magpi-attempt-observed-files attempt)
          (seq-uniq (append (magpi-attempt-observed-files attempt)
                            (list path))
                    #'string=)))
  attempt)

(defun magpi--content-text (content)
  "Extract textual blocks from Pi CONTENT."
  (cond
   ((stringp content) content)
   ((vectorp content) (magpi--content-text (append content nil)))
   ((listp content)
    (mapconcat
     (lambda (item)
       (if (and (listp item) (equal (plist-get item :type) "text"))
           (or (plist-get item :text) "")
         ""))
     content ""))
   (t "")))

(defun magpi--one-line (text &optional width)
  "Collapse TEXT to one line, limited to WIDTH columns."
  (let ((line (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " (or text "")))))
    (truncate-string-to-width line (or width 100) nil nil "…")))

(defun magpi-attempt-adopt-title (attempt title)
  "Use TITLE only when ATTEMPT deliberately has no authored intention."
  (when (and (stringp title)
             (not (string-empty-p title))
             (string-empty-p (or (magpi-attempt-intent attempt) "")))
    (setf (magpi-attempt-intent attempt) title
          (magpi-attempt-name attempt)
          (format "%s · %s" (magpi--one-line title 42)
                  (substring (magpi-attempt-id attempt) 0
                             (min 4 (length (magpi-attempt-id attempt)))))))
  attempt)

(defun magpi-attempt-apply-event (attempt event)
  "Reduce one Pi RPC EVENT into ATTEMPT and return ATTEMPT."
  (let ((type (plist-get event :type)))
    (pcase type
      ((or "agent_start" "turn_start")
       (setf (magpi-attempt-status attempt) 'running
             (magpi-attempt-activity attempt) "thinking"))
      ("agent_settled"
       (setf (magpi-attempt-status attempt) 'idle
             (magpi-attempt-activity attempt) nil))
      ("tool_execution_start"
       (let* ((tool (plist-get event :toolName))
              (args (plist-get event :args))
              (path (plist-get args :path)))
         (setf (magpi-attempt-status attempt) 'running
               (magpi-attempt-activity attempt)
               (if tool (format "%s" tool) "tool"))
         (when (member tool '("edit" "write"))
           (magpi-attempt-add-observed-file attempt path))))
      ("tool_execution_end"
       (setf (magpi-attempt-activity attempt) "thinking"))
      ("message_end"
       (let ((message (plist-get event :message)))
         (when (equal (plist-get message :role) "assistant")
           (setf (magpi-attempt-last-response attempt)
                 (magpi--one-line
                  (magpi--content-text (plist-get message :content)))))))
      ("backend_title"
       (magpi-attempt-adopt-title attempt (plist-get event :title)))
      ("extension_ui_request"
       (when (equal (plist-get event :method) "setTitle")
         (magpi-attempt-adopt-title attempt (plist-get event :title))))
      ("backend_disconnected"
       (magpi-attempt-mark-disconnected attempt))
      ((or "extension_error" "auto_retry_start")
       (setf (magpi-attempt-status attempt) 'attention
             (magpi-attempt-activity attempt)
             (if (equal type "auto_retry_start") "retrying" "extension error")))))
  attempt)

(defun magpi-attempt-mark-disconnected (attempt)
  "Mark ATTEMPT disconnected."
  (setf (magpi-attempt-status attempt) 'disconnected
        (magpi-attempt-activity attempt) nil)
  attempt)

(provide 'magpi-core)
;;; magpi-core.el ends here
