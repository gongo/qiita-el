(require 'json)
(require 'ert-expectations)
(require 'el-mock)

(defun qiita-test:json-read (filename)
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read-file (concat "fixtures/" filename ".json"))))

(defun qiita-test:response (status body)
  `(:status ,status :json ,(when body (qiita-test:json-read body))))

(expectations
  (desc "qiita:api-rate-limit")
  (expect 140
    (stub qiita:api-exec => (qiita-test:response 200 "get_rate_limit"))
    (plist-get (qiita:api-rate-limit) :remaining))





  )
