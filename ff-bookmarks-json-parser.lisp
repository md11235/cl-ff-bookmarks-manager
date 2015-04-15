(defvar bookmarks-object (with-open-file (input #P"d:/zhang/Downloads/bookmark-merger0.1/working_copy/bookmarks-2015-04-14.json"
                                                :direction :input
                                                :external-format :utf-8)
                           (cl-json:decode-json input)))

(defclass ff-bookmark-item ()
  "guid"
  "title"
  "index"
  "dateAdded"
  "lastModified"
  "id"
  "annos"
  "flags"
  "expires"
  "value"
  "type"
  
  "root"
  "children"

  "guid"
  "title"
  "index"
  "dateAdded"
  "lastModified"
  "id"
  "annos"
  "flags"
  "expires"
  "value"
  "type"
  
  "uri"
  )

;;;; merge children carrying the same name of `place-container-object` 
;;;; 
(defun merge-children (place-container-object)
  (let ((original-children (assoc :children place-container-object))
        (new-children '()))
    (loop for child-object in original-children
       do (if (x-moz-place-container-p child-object)
              (let ((maybe-existing-child (get-the-exisitng-child)))
                (if maybe-existing-child
                    ;; move the children of the current child into the existing one
                    (push (assoc :children ))
                    ))))
    (setf (assoc :children place-container-object) new-children)))