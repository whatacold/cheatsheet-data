(defun gen-data (el-fname json-fname)
  (let (json-data)
    (with-current-buffer (find-file-noselect el-fname)
      (goto-char (point-min))
      (let (category)
        (ignore-errors
          (setq category (read (current-buffer)))

          (while category
            ;; (message "json data: %s" json-data)
            ;; (message "category %s" category)
            (let (category-name items)
              (setq category-name (car category))
              (setq items (mapcar (lambda (fn)
                                    (let ((docstr (documentation fn)))
                                      ;; (setq docstr "mock")
                                      `(:name ,(symbol-name fn) :title ,docstr)))
                                  (nth 1 category)))
              ;; (message "category name: %s, items: %s" category-name items)
              (setq json-data (cons `(:name ,category-name :apis ,(vconcat items))
                                    json-data)))

            (setq category (read (current-buffer)))))))

    (when json-data
      ;; (message "json data: %s" json-data)

      (with-current-buffer (find-file-noselect json-fname)
        (erase-buffer)
        (insert "export default function data() { return ")
        (insert (json-serialize (vconcat (reverse json-data))))
        (insert "}")
        (save-some-buffers 'dont-ask)))))

(gen-data "./elisp-api.el" "./data/elisp-api-data.json")
