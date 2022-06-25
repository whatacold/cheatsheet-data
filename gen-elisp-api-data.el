(require 'ring)
(require 'seq)

(defun gen-data (el-fname js-fname)
  "Generate a .js file `JS-FNAME' based on data in `EL-FNAME'."
  (let (json-data)
    (with-current-buffer (find-file-noselect el-fname)
      (goto-char (point-min))
      (let (category)
        (ignore-error end-of-file
          (setq category (read (current-buffer)))

          (while category
            ;; (message "json data: %s" json-data)
            ;; (message "category %s" category)
            (let (category-name items)
              (setq category-name (plist-get category :name))
              (setq items (mapcar (lambda (fn)
                                    (let ((docstr (documentation fn)))
                                      ;; (setq docstr "mock")
                                      `(:name ,(symbol-name fn) :title ,docstr)))
                                  (plist-get category :functions)))
              ;; (message "category name: %s, items: %s" category-name items)
              (setq json-data (cons `(:name ,category-name
                                            :apis ,(vconcat items)
                                            :description ,(plist-get category :description))
                                    json-data)))

            (setq category (read (current-buffer)))))))

    (when json-data
      ;; (message "json data: %s" json-data)

      (with-current-buffer (find-file-noselect js-fname)
        (erase-buffer)
        (insert "export default function data() { return ")
        (insert (json-serialize (vconcat (reverse json-data))))
        (insert "}")
        (save-some-buffers 'dont-ask)))))

(gen-data "./elisp-api.el" "./data/elisp-api-data.js")
