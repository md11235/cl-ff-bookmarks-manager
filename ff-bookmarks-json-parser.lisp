(require 'cl-json)

(defclass ff-bookmark-record ()
  ((guid :initarg :guid
         :reader ff-bookmark-record-guid)
   (title :initarg :title
          :reader ff-bookmark-record-title)
   (index :initarg :index
          :reader ff-bookmark-record-index)
   (date-added :initarg :date-added
               :reader ff-bookmark-record-date-added)
   (last-modified :initarg :last-modified
                  :reader ff-bookmark-record-last-modified)
   (id :initarg :id
       :reader ff-bookmark-record-id)
   (annos :initarg :annos
          :reader ff-bookmark-record-annos)
   (flags :initarg :flags
          :reader ff-bookmark-record-flags)
   (expires :initarg :expires
            :reader ff-bookmark-record-expires)
   (value :initarg :value
          :reader ff-bookmark-record-value)
   (type :initarg :type
         :reader ff-bookmark-record-type)
   (root :initarg :root
         :reader ff-bookmark-record-root)
   (children :initarg :children
             :accessor ff-bookmark-record-children)
   (uri :initarg :uri
        :accessor ff-bookmark-record-uri)))

(defun parse-root-bookmark-record-from-ff-exported-json (filepath-of-ff-exported-json)
  (let ((root-bookmark-alist
         (with-open-file (input filepath-of-ff-exported-json
                                :direction :input
                                :external-format :utf-8)
           (cl-json:decode-json input))))
    (parse-ff-bookmark-record root-bookmark-alist)))

(defun x-moz-place-container-p (bookmark-record)
  (string-equal (ff-bookmark-record-type bookmark-record)
                "text/x-moz-place-container"))

(defun parse-ff-bookmark-record (record-alist)
  (let ((bookmark-record
         (make-instance 'ff-bookmark-record
                        :guid (cdr (assoc :guid record-alist))
                        :title (cdr (assoc :title record-alist))
                        :index (cdr (assoc :index record-alist))
                        :date-added (cdr (assoc :date-added record-alist))
                        :last-modified (cdr (assoc :last-modified record-alist))
                        :id (cdr (assoc :id record-alist))
                        :type (cdr (assoc :type record-alist))
                        :root (cdr (assoc :root record-alist))
                        )))
    (if (x-moz-place-container-p bookmark-record)
        (setf (ff-bookmark-record-children bookmark-record)
              (loop for child-record-alist in (cdr (assoc :children record-alist))
                 collect (parse-ff-bookmark-record child-record-alist)))
        (setf (ff-bookmark-record-uri bookmark-record)
              (cdr (assoc :uri record-alist))))
    bookmark-record))


;;;; merge children with the same name
;;;; 
(defun merge-bookmark-record-children (bookmark-record)
  (let ((original-children (ff-bookmark-record-children bookmark-record))
        (new-children '()))
    (flet ((get-maybe-existing-child (children title)
             (car (member title children
                          :test #'string-equal
                          :key #'ff-bookmark-record-title ))))
      (loop for child-bookmark-record in original-children
         do (if (x-moz-place-container-p child-bookmark-record)
                (let ((maybe-existing-child (get-maybe-existing-child
                                             (remove-if-not #'x-moz-place-container-p
                                                            new-children)
                                             (ff-bookmark-record-title child-bookmark-record))))
                  (if maybe-existing-child
                      ;; move the children of the current child into the existing one
                      (append (ff-bookmark-record-children child-bookmark-record)
                              (ff-bookmark-record-children maybe-existing-child))
                      (push child-bookmark-record new-children)))
                (push child-bookmark-record new-children))))
    (setf (ff-bookmark-record-children bookmark-record)
          (sort new-children
                #'(lambda (title-1 title-2)
                    (string< (string-downcase title-1)
                             (string-downcase title-2)))
                :key #'ff-bookmark-record-title))
    bookmark-record))

(defun merge-bookmark-record-recursively (bookmark-record)
  (merge-bookmark-record-children bookmark-record)
  (loop for child-bookmark-record in (remove-if-not #'x-moz-place-container-p
                                                    (ff-bookmark-record-children bookmark-record))
     do (merge-bookmark-record-children child-bookmark-record))
  bookmark-record)

(defun merge-from-root-bookmark-record (root-bookmark-record)
  (merge-bookmark-record-recursively root-bookmark-record))

