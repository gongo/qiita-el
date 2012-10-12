;;; qiita.el -- Qiita API Library for emacs

;; Author: Wataru MIYAGUNI (gonngo _at_ gmail.com)
;; Keywords: qiita

;; Copyright (c) 2012 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This program is a tool for Qiita <http://qiita.com> .

;;; Usage:

;;
;; (require 'qiita-el)
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'markdown-mode)
(require 'helm)

(defconst qiita->api-endpoint "https://qiita.com/api/v1"
  "The base URI on Qiita API. see <http://qiita.com/docs>")
(defvar qiita->token nil)

(defun qiita:response-status (response)
  (plist-get response :status))

(defun qiita:response-body (response)
  (plist-get response :json))

(defun qiita:api-exec (method path &optional args)
  (with-temp-buffer
    (when qiita->token
      (add-to-list 'args `("token" . ,qiita->token)))
    (let* ((uri (concat qiita->api-endpoint path))
           (query (json-encode-alist args))
           (ret (call-process "curl" nil (current-buffer) nil
                              "-H" "Content-type: application/json"
                              "-s"
                              "-X" method
                              "-w" "\nhttp_code=%{http_code}"
                              "--data-binary" query uri))
           res body)
      (goto-char (point-min))

      (unless (zerop ret)
        (error (format "Error: Not retrieved %s" uri)))
      (unless (re-search-forward "^http_code=\\(.*\\)$" nil t)
        (error "Error: Can't find status code"))

      ;; ectract status code
      (setq res
            (plist-put res :status
                       (string-to-number (match-string-no-properties 1))))
      (delete-region (match-beginning 0) (match-end 0))

      ;; convert string to json object
      (setq body (replace-regexp-in-string
                  "\n+$" ""
                  (buffer-substring-no-properties (point-min) (point-max))))
      (setq res (plist-put res :json
                           (unless (eq 0 (length body))
                             (let ((json-object-type 'plist)
                                   (json-array-type 'list))
                               (json-read-from-string body)))))
      res
      )))

(defun qiita:api-rate-limit ()
  (let ((response (qiita:api-exec "GET" "/rate_limit")))
    (qiita:response-body response)))

(defun qiita:api-auth (username password)
  (let ((response (qiita:api-exec "POST" "/auth" `(("url_name" . ,username)
                                                   ("password" . ,password)))))
    (plist-get (qiita:response-body response) :token)))

(defun qiita:api-user-items (username)
  "指定したユーザーの投稿を取得します。"
  ;; pending
  )

(defun qiita:api-user-stocks (username)
  "指定したユーザーのストックした投稿を取得します。"
  ;; pending
  )

(defun qiita:api-tag-items (tag)
  "指定したタグの投稿を取得します。"
  ;; pending
  )

(defun qiita:api-tag ()
  "タグ一覧を取得します。"
  ;;(qiita:api-exec "GET" "/tags"))
  ;;pending
  )

(defun qiita:api-search (q &optional stocked)
  "指定したキーワードの検索結果を取得します。 TODO"
  (let ((args `(("q" . ,q)))
        response)
    (when qiita->token
      (add-to-list 'args `("stocked" . ,(if stocked "true" "false"))))
    (setq response (qiita:api-exec "GET" "/search" args))
    (qiita:response-body response)))

(defun qiita:api-items ()
  (let ((response (qiita:api-exec "GET" "/items")))
    (qiita:response-body response)))

(defun qiita:api-stocks ()
  "自分のストックした投稿を取得します。(要認証)"
  ;;(qiita:api-exec "GET" "/stocks"))
  ;;pending
  )

(defun qiita:api-create-item (title body tags private &optional gist? tweet?)
  (let ((args `(("title"   . ,title)
                ("body"    . ,body)
                ("tags"    . ,tags)
                ("private" . ,private))))
    (when gist?    (add-to-list 'args '("gist"    . "true")))
    (when tweet?   (add-to-list 'args '("tweet"   . "true")))

    (let ((response (qiita:api-exec "POST" "/items" args)))
      (if (eq 201 (qiita:response-status response))
          (message "success")
        (error "Error: Can't create item because %s"
               (plist-get (qiita:response-body response) :error))))))

(defun qiita:api-update-item ()
  ;; pending
  )

(defun qiita:api-delete-item (uuid)
  (let ((response (qiita:api-exec "DELETE" (format "/items/%s" uuid))))
    (if (eq 204 (qiita:response-status response))
        (message "success")
      (error "Error: Can't Delete item (%s). because %s"
             uuid (plist-get (qiita:response-body response) :error)))))

(defun qiita:api-get-item (uuid)
  ;;(qiita:api-exec "GET" (format "/items/%s" uuid)))
  ;; pending
  )

(defun qiita:api-stock-item (uuid)
  "指定した投稿 UUID をストックします。"
  ;; pending
  )

(defun qiita:api-unstock-item (uuid)
  "指定した投稿 UUID をストック解除します。"
  ;; pending
  )

(defun qiita:api-user-info (username)
  "指定したユーザーの情報を取得します。"
  ;; pending
  )

(defun qiita:body-cut-title (&optional buffer)
  (when (null buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (title)
      (setq title (cond
                   ((re-search-forward markdown-regex-header-1-atx nil t)
                    (match-string-no-properties 2))
                   ((re-search-forward markdown-regex-header-1-setext nil t)
                    (match-string-no-properties 1))
                   (t (error "Can't find header-1"))))
      (delete-region (match-beginning 0) (match-end 0))
      title)))

(defun qiita:body-cut-tags (&optional buffer)
  (when (null buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (tags)
      (unless (re-search-forward "^==tags== \\(.*\\)$" nil t)
        (error "Can't find tags"))
      (setq tags (match-string-no-properties 1))
      (delete-region (match-beginning 0) (match-end 0))
      (vconcat (mapcar (lambda (x) `((:name . ,x))) (split-string tags ","))))))

(defun qiita:post (&optional private?)
  (interactive "P")
  (let ((mkdn (with-current-buffer (current-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert mkdn)
      (let ((title (qiita:body-cut-title))
            (tags  (qiita:body-cut-tags))
            (body (buffer-substring-no-properties (point-min) (point-max)))
            (private (if (null private?) "true" "false")))
        (qiita:api-create-item title body tags private)))))

(defun qiita:browse (uuid)
  (browse-url (concat "http://qiita.com/items/" uuid)))

(defun qiita:items-candidates ()
  (mapcar (lambda (item)
            (let ((title (plist-get item :title))
                  (uuid  (plist-get item :uuid))
                  (user  (plist-get (plist-get item :user) :name))
                  (tags  (mapconcat (lambda (tag)
                                      (concat "[" (plist-get tag :name) "]"))
                                    (plist-get item :tags) "")))
              (cons (concat tags  "\n"
                            title "\n"
                            "  by " user)
                    uuid)))
          (qiita:api-items)))


(defun qiita:items (&optional my)
  (interactive "P")
  (let ((qiita->token (when my qiita->token)))
    (helm :sources '(helm-c-qiita-items-source))))

(defun qiita:delete (uuid)
  (when (yes-or-no-p "Delete this item?")
    (qiita:api-delete-item uuid)))

(defvar helm-c-qiita-items-source
  '((name . "Qiita new activities")
    (candidates . qiita:items-candidates)
    (candidate-number-limit . 100)
    (type . qiita-my-items)
    (multiline)))


;; (define-helm-type-attribute 'qiita-my-items
;;   `((action ("Open Browser" . qiita:browse))))

(define-helm-type-attribute 'qiita-my-items
  `((action ("Open Browser" . qiita:browse)
            ("Delete" . qiita:delete))))
