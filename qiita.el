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

(defun qiita:api-exec (method uri &optional args)
  (with-temp-buffer
    (when qiita->token
      (add-to-list 'args `("token" . ,qiita->token)))
    (let* ((query (json-encode-alist args))
           (ret (call-process "curl" nil (current-buffer) nil
                              "-H" "Content-type: application/json"
                              "-s"
                              "-X" method
                              "--data-binary" query uri)))
      (condition-case err
          (progn
            (unless (zerop ret)
              (error (format "Error: Not retrieved %s" uri)))
            (let ((json-object-type 'plist)
                  (json-array-type 'list))
              (json-read-from-string (buffer-substring-no-properties
                                      (point-min) (point-max)))))
        (error (message (error-message-string err)))))))

(defun qiita:api-rate-limit ()
  (qiita:api-exec "GET" (concat qiita->api-endpoint "/rate_limit")))

(defun qiita:api-auth (username password)
  (plist-get (qiita:api-exec "POST" (concat qiita->api-endpoint "/auth")
                             `(("url_name" . ,username)
                               ("password" . ,password)))
             :token))

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
  (qiita:api-exec "GET" (concat qiita->api-endpoint "/tags")))

(defun qiita:api-search (q &optional stocked)
  "指定したキーワードの検索結果を取得します。 TODO"
  (let ((args `(("q" . ,q))))
    (when qiita->token
      (add-to-list 'args `("stocked" . ,(if stocked "true" "false"))))
    (qiita:api-exec "GET" (concat qiita->api-endpoint "/search") args)))

(defun qiita:api-items ()
  "新着投稿を取得します。
もし認証を行う場合は、自身の新着投稿を取得します。"
  (qiita:api-exec "GET" (concat qiita->api-endpoint "/items")))

(defun qiita:api-stocks ()
  "自分のストックした投稿を取得します。(要認証)"
  (qiita:api-exec "GET" (concat qiita->api-endpoint "/stocks")))

(defun qiita:api-create-item (title body tags private? &optional gist? tweet?)
  (let ((args `(("title" . ,title)
                ("body"  . ,body)
                ("tags"  . ,tags))))
    (if private? (add-to-list 'args '("private" . "true"))
      (add-to-list 'args '("private" . "false")))
    (when gist?    (add-to-list 'args '("gist"    . "true")))
    (when tweet?   (add-to-list 'args '("tweet"   . "true")))
    (qiita:api-exec "POST" (concat qiita->api-endpoint "/items") args)))

(defun qiita:api-update-item ()
  ;; pending
  )

(defun qiita:api-delete-item ()
  ;; pending
  )

(defun qiita:api-get-item (uuid)
  "指定した投稿 UUID を取得します。"
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
  "指定したバッファ内の markdown 文章からタイトル (header-1) の部分を切り取る。

対象は

  # hogehoge

もしくは

  hogehoge
  ========="
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
  "指定したバッファ内の markdown 文章からをタグ表記を切り取り、array に変換する。

  ==tags== ruby,emacs,c
    ;; => [((:name . \"emacs\")) ((:name . \"ruby\")) ((:name . \"c\"))]"
  (when (null buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (tags)
      (unless (re-search-forward "^=tags= \\(.*\\)$" nil t)
        (error "Can't find tags"))
      (setq tags (match-string-no-properties 1))
      (delete-region (match-beginning 0) (match-end 0))
      (vconcat (mapcar (lambda (x) `((:name . ,x)))
                       (split-string tags ",")))
      )))

(defun qiita:post (&optional private?)
  (interactive "P")
  (let ((mkdn (with-current-buffer (current-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert mkdn)
      (let ((title (qiita:body-cut-title))
            (tags  (qiita:body-cut-tags))
            (body (buffer-substring-no-properties (point-min) (point-max))))
        (qiita:api-create-item title body tags (null private?)))
      )))
