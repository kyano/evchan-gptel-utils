;;; evchan-gptel-utils --- Custom tools collection for my opinionated setting -*- lexical-binding: t -*-

;; Copyright (C) 2025 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/evchan-gptel-utils
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (gptel "0.9.8.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom tools collection for my opinionated setting

;;; Code:

(require 'gptel)

(defun evchan-gptel-utils/read-url (callback url)
  "Read the content of a URL and pass it to CALLBACK.

The content of a URL is converted to plain-text by `shr'.  Before
converting, the HTML is parsed by libxml2 so Emacs must be compiled with
it.

If the mime-type of URL is `application/pdf', it will converted by the
external program `pdftotext'.  If `pdftotext' is not found, an error
message is passed to CALLBACK."

  (let ((url-user-agent (format "Emacs/%s (%s) gptel"
                                emacs-version
                                system-configuration))
        (url-request-method "GET"))
    (defvar url-http-end-of-headers)
    (condition-case error
        (url-retrieve url (lambda (status)
                            (if (plist-member status 'error)
                                (funcall callback (format "An error occurred: %s"
                                                          (plist-get status 'error)))
                              (let ((headers (buffer-substring-no-properties
                                              (point-min)
                                              (marker-position url-http-end-of-headers)))
                                    (found-content-type nil)
                                    (mime-type nil)
                                    dom
                                    plain-text)
                                (catch 'break-loop
                                  (dolist
                                      (elem (split-string headers))
                                    (when found-content-type
                                      (setq mime-type elem)
                                      (throw 'break-loop t))
                                    (when (eq (compare-strings
                                               elem nil nil
                                               "content-type:" nil nil
                                               t) t)
                                      (setq found-content-type t))))
                                (if (string= mime-type "application/pdf")
                                    (let ((temp-file (make-temp-file "gptel-tool-read-url"))
                                          text-content)
                                      (write-region (marker-position url-http-end-of-headers)
                                                    (point-max)
                                                    temp-file)
                                      (with-temp-buffer
                                        (call-process "pdftotext" nil t nil "-q" temp-file "-")
                                        (setq text-content (buffer-string)))
                                      (funcall callback text-content))
                                  (progn
                                    (setq dom (libxml-parse-html-region
                                               (marker-position url-http-end-of-headers)
                                               (point-max)))
                                    (with-temp-buffer
                                      (shr-insert-document dom)
                                      (setq plain-text (buffer-substring-no-properties
                                                        (point-min)
                                                        (point-max))))
                                    (funcall callback plain-text)))))) nil t nil)
      (t (funcall callback (format "An error occurred: %s"
                                   (error-message-string error)))))))

(defun evchan-gptel-utils/run-python (callback code)
  "Run a Python code block CODE and pass it to CALLBACK.

A valid executable `python3' must be found in the directories set in
`PATH'."

  (let ((temp-file (make-temp-file "gptel-tool-run-python")))
    (with-temp-buffer
      (goto-char (point-min))
      (insert code)
      (write-file temp-file))
    (let ((output-buffer (generate-new-buffer "gptel-tool-run-python/output")))
      (set-process-sentinel
       (start-process "gptel-tool-run-python/python" output-buffer "python3" "-q" temp-file)
       (lambda (process signal)
         (let ((status (process-status process))
               output)
           (when (memq status '(exit signal))
             (cond
              ((string= (substring signal 0 -1) "finished")
               (with-current-buffer output-buffer
                 (setq output (buffer-string)))
               (kill-buffer output-buffer)
               (funcall callback output))
              (t
               (with-current-buffer output-buffer
                 (setq output (buffer-string)))
               (kill-buffer output-buffer)
               (funcall callback (format "An error occurred: %s\n\n%s"
                                         (substring signal 0 -1)
                                         output)))))))))))

(defun evchan-gptel-utils/read-file (callback path)
  "Read a file PATH and pass its content to CALLBACK."

  (let ((full-path (expand-file-name
                    path
                    default-directory))
        file-content)
    (if (file-exists-p full-path)
        (condition-case error
            (with-temp-buffer
              (progn
                (goto-char (point-min))
                (insert-file-contents full-path)
                (setq file-content (buffer-substring-no-properties
                                    (point-min)
                                    (point-max)))
                (funcall callback file-content)))
          (t (funcall callback (format "Error loading file %s: %s"
                                       path
                                       (error-message-string error)))))
      (funcall callback (format "File not found: %s"
                                path)))))

(defun evchan-gptel-utils/read-buffer (callback buffer-name)
  "Read the content of a buffer named BUFFER-NAME and pass it to CALLBACK."

  (let ((buffer (get-buffer buffer-name))
        buffer-content)
    (if (buffer-live-p buffer)
        (condition-case error
            (with-current-buffer buffer
              (setq buffer-content (buffer-substring-no-properties
                                    (point-min)
                                    (point-max)))
              (funcall callback buffer-content))
          (t (funcall callback (format "An error occurred: %s"
                                       error))))
      (funcall callback (format "%s is not live anymore"
                                buffer-name)))))

(defun evchan-gptel-utils/now (callback)
  "Return the date and time of now to CALLBACK."

  (let ((timestring (format-time-string "%F(%a) %T%:z"
                                        (current-time))))
    (funcall callback timestring)))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "read_url"
              :function #'evchan-gptel-utils/read-url
              :async t
              :description "Get the content of a URL in plain-text"
              :args (list '( :name "url"
                             :type string
                             :description "The URL to read"))
              :category "web"))
(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "run_python"
              :function #'evchan-gptel-utils/run-python
              :async t
              :description "Run the python code"
              :args (list '( :name "code"
                             :type string
                             :description "The code string to run"))
              :category "programming"))
(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "read_file"
              :function #'evchan-gptel-utils/read-file
              :async t
              :description "Get the content of a file"
              :args (list '( :name "path"
                             :type string
                             :description "The filepath to read"))
              :category "file"))
(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "read_buffer"
              :function #'evchan-gptel-utils/read-buffer
              :async t
              :description "Get the content of a buffer"
              :args (list '( :name "buffer_name"
                             :type string
                             :description "The buffer name to read"))
              :category "emacs"))
(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "now"
              :function #'evchan-gptel-utils/now
              :async t
              :description
              (concat "Get the current date and time."
                      " The format is `%Y-%m-%dT%H:%M:%S%:z`."
                      " ex) \"2025-09-05T14:07:37+09:00\"")
              :args '()
              :category "time"))

(defun evchan-gptel-utils/hunyuan-mt-prompt ()
  "Generate a prompt template for `Hunyuan-MT'."

  (interactive)
  (let* ((lang-candidates '("Chinese" "English" "French" "Portuguese" "Spanish"
                            "Japanese" "Turkish" "Russian" "Arabic" "Korean" "Thai"
                            "Italian" "German" "Vietnamese" "Malay" "Indonesian"
                            "Filipino" "Hindi" "Traditional Chinese" "Polish" "Czech"
                            "Dutch" "Khmer" "Burmese" "Persian" "Gujarati" "Urdu"
                            "Telugu" "Marathi" "Hebrew" "Bengali" "Tamil" "Ukrainian"
                            "Tibetan" "Kazakh" "Mongolian" "Uyghur" "Cantonese"))
         (target-lang (completing-read "Choose a target language: " lang-candidates nil t))
         (prompt-string (format "Translate the following segment into %s, without additional explanation.\n\n"
                                target-lang)))
    (insert prompt-string)))

(define-key gptel-mode-map
            (kbd "C-c <f12> t")
            #'evchan-gptel-utils/hunyuan-mt-prompt)

(provide 'evchan-gptel-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; evchan-gptel-utils.el ends here
