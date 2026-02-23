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
(require 'gptel-gemini)

(defconst evchan-gptel-utils/user-agent
  (format "Emacs/%s (%s) gptel"
          emacs-version
          system-configuration))

(defun evchan-gptel-utils/read-url (callback url)
  "Read the content of a URL and pass it to CALLBACK.

The content of a URL is converted to plain-text by `shr'.  Before
converting, the HTML is parsed by libxml2 so Emacs must be compiled with
it.

If the mime-type of URL is `application/pdf', it will converted by the
external program `pdftotext'.  If `pdftotext' is not found, an error
message is passed to CALLBACK."

  (let ((url-user-agent evchan-gptel-utils/user-agent)
        (url-request-method "GET"))
    (defvar url-http-end-of-headers)
    (condition-case error
        (url-retrieve
         url
         (lambda (status)
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
                   (funcall callback plain-text))))))
         nil t nil)
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

(defvar evchan-gptel-utils/wikipedia-token-expires-at 0)
(defvar evchan-gptel-utils/wikipedia-access-token)

(defun evchan-gptel-utils/wikipedia-refresh-token (callback)
  "Refresh the access token for Wikipedia and proceed CALLBACK."

  (let ((now (string-to-number (format-time-string "%s" (current-time)))))
    (if (> now evchan-gptel-utils/wikipedia-token-expires-at)
        (let* ((url-user-agent evchan-gptel-utils/user-agent)
               (token-url "https://meta.wikimedia.org/w/rest.php/oauth2/access_token")
               (client-id (plist-get (car (auth-source-search
                                           :host "wikimedia.org"))
                                     :user))
               (client-secret (auth-info-password
                               (car (auth-source-search
                                     :host "wikimedia.org"))))
               (url-request-method "POST")
               (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
               (url-request-data (concat "grant_type=client_credentials"
                                         (format "&client_id=%s"
                                                 (url-hexify-string client-id))
                                         (format "&client_secret=%s"
                                                 (url-hexify-string client-secret)))))
          (defvar url-http-end-of-headers)
          (condition-case error
              (url-retrieve
               token-url
               (lambda (status)
                 (if (plist-member status 'error)
                     (funcall callback (format "An error occurred: %s"
                                               (plist-get status 'error)))
                   (let* ((response (json-read-from-string
                                     (buffer-substring-no-properties (marker-position url-http-end-of-headers)
                                                                     (point-max))))
                          (token-type (cdr (assoc 'token_type response)))
                          (expires-in (cdr (assoc 'expires_in response)))
                          (access-token (cdr (assoc 'access_token response))))
                     (setq evchan-gptel-utils/wikipedia-token-expires-at
                           (+ now (truncate (* expires-in 0.75))))
                     (setq evchan-gptel-utils/wikipedia-access-token
                           (concat token-type " " access-token))
                     (funcall callback nil))))
               nil t nil)
            (t (funcall callback (format "An error occurred: %s"
                                         (error-message-string error))))))
      (funcall callback nil))))

(defun evchan-gptel-utils/search-wikipedia (callback keyword)
  "Search Wikipedia documents with titles containing KEYWORD.

After searching, pass them to CALLBACK in JSON format."

  (evchan-gptel-utils/wikipedia-refresh-token
   #'(lambda (err)
       (if err
           (funcall callback err)
         (let* ((url-user-agent evchan-gptel-utils/user-agent)
                (search-url "https://api.wikimedia.org/core/v1/wikipedia/en/search/title")
                (url-request-method "GET")
                (search-term (format "q=%s" (url-hexify-string keyword)))
                (limit "limit=15"))
           (defvar url-http-end-of-headers)
           (condition-case error
               (url-retrieve
                (format "%s?%s&%s" search-url search-term limit)
                (lambda (status)
                  (if (plist-member status 'error)
                      (funcall callback (format "An error occurred: %s"
                                                (plist-get status 'error)))
                    (let* ((response (json-read-from-string
                                      (buffer-substring-no-properties (marker-position url-http-end-of-headers)
                                                                      (point-max))))
                           (pages (cdr (assoc 'pages response)))
                           page-list)
                      (setq page-list
                            (mapcar
                             (lambda (page)
                               (let ((key (cdr (assoc 'key page)))
                                     (title (cdr (assoc 'title page)))
                                     (description (cdr (assoc 'description page))))
                                 `(,title . ((title . ,key)
                                             (description . ,description)))))
                             pages))
                      (funcall callback (json-encode page-list))))) nil t nil)
             (t (funcall callback (format "An error occurred: %s"
                                          (error-message-string error))))))))))

(defun evchan-gptel-utils/fetch-wikipedia (callback key)
  "Fetch a document titled KEY from Wikipedia and pass it to CALLBACK."

  (evchan-gptel-utils/wikipedia-refresh-token
   #'(lambda (err)
       (if err
           (funcall callback err)
         (let* ((url-user-agent evchan-gptel-utils/user-agent)
                (fetch-url (format "https://api.wikimedia.org/core/v1/wikipedia/en/page/%s"
                                   key))
                (url-request-method "GET"))
           (defvar url-http-end-of-headers)
           (condition-case error
               (url-retrieve
                fetch-url
                (lambda (status)
                  (if (plist-member status 'error)
                      (funcall callback (format "An error occurred: %s"
                                                (plist-get status 'error)))
                    (let* ((response (json-read-from-string
                                      (buffer-substring-no-properties (marker-position url-http-end-of-headers)
                                                                      (point-max))))
                           (wiki-source (cdr (assoc 'source response))))
                      (funcall callback (json-encode
                                         `((title . ,key)
                                           (document . ,wiki-source)))))))
                nil t nil)
             (t (funcall callback (format "An error occurred: %s"
                                          (error-message-string error))))))))))

(defvar evchan-gptel-utils/gcloud-token-expires-at 0)
(defvar evchan-gptel-utils/gcloud-access-token)

(defun evchan-gptel-utils/gcloud-refresh-token (callback)
  "Refresh the access token for GCloud API and proceed CALLBACK."

  (let ((now (string-to-number (format-time-string "%s" (current-time)))))
    (if (> now evchan-gptel-utils/gcloud-token-expires-at)
        (progn
          (with-temp-buffer
            (when (/= 0 (call-process "gcloud" nil t nil "auth" "application-default" "print-access-token"))
              (funcall callback "Failed to call gcloud CLI"))
            (setq evchan-gptel-utils/gcloud-token-expires-at
                  (+ now 3000))
            (setq evchan-gptel-utils/gcloud-access-token
                  (string-trim (buffer-string))))
          (funcall callback nil))
      (funcall callback nil))))

(defun evchan-gptel-utils/get-gcloud-token ()
  "Return the access token for GCloud API."

  (evchan-gptel-utils/gcloud-refresh-token
   #'(lambda (err)
       (if err
           (error err)
         evchan-gptel-utils/gcloud-access-token))))

(defun evchan-gptel-utils/gptel-gemini-for-vertexai (backend-name)
  "Modify the Gemini backend BACKEND-NAME to work properly with Vertex AI.

This modifies the value of the `:url' slot and adds an advice to
`gptel--request-data'.  The advice adds two built-in tools
`google_search' and `url_context' if no tools are provided."

  (let* ((backend (alist-get backend-name
                             gptel--known-backends
                             nil nil
                             #'string=))
         (protocol (slot-value backend 'protocol))
         (host (slot-value backend 'host))
         (endpoint (slot-value backend 'endpoint))
         (stream (slot-value backend 'stream)))
    (setf (slot-value backend 'url)
          #'(lambda ()
              (concat protocol "://" host
                      endpoint
                      "/" (symbol-name gptel-model)
                      ":" (if (and stream gptel-use-curl gptel-stream)
                              "streamGenerateContent"
                            "generateContent"))))
    (advice-add #'gptel--request-data
                :around
                #'(lambda (fn backend prompts)
                    (let ((request-data (funcall fn backend prompts)))
                      (when (and (gptel-gemini-p backend)
                                 gptel-use-tools
                                 (not gptel-tools))
                        (plist-put request-data
                                   :tools
                                   [(:google_search ())
                                    (:url_context ())])
                        request-data))))))

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
              :description "Get the content of an Emacs buffer"
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
                      " The format is `YEAR-MONTH-DAY(WEEKDAY) HOUR:MINUTE:SECOND+TIMEZONE'."
                      " ex) \"2025-09-05(금) 14:07:37+09:00\"")
              :args '()
              :category "time"))
(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "search_wikipedia"
              :function #'evchan-gptel-utils/search-wikipedia
              :async t
              :description
              (concat "Search Wikipedia documents which a title contains `keyword`."
                      " To fetch the actual article, use `fetch_wikipedia`.")
              :args (list '( :name "keyword"
                             :type string
                             :description "A keyword to search"))
              :category "wikipedia"))
(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "fetch_wikipedia"
              :function #'evchan-gptel-utils/fetch-wikipedia
              :async t
              :description
              (concat "Fetch a Wikipedia document with the `title`, in MediaWiki format."
                      " To get the title of an article, use `search_wikipedia`.")
              :args (list `( :name "title"
                             :type string
                             :description
                             ,(concat "A title of document to fetch."
                                      " The title can be retrieved from `search_wikipedia`")))
              :category "wikipedia"))

(provide 'evchan-gptel-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; evchan-gptel-utils.el ends here
