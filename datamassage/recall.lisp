;;; ???

(defun read-votes (file)
  (with-open-file (f file)
    (loop with luvl = (make-hash-table)
          for l = (read-line f nil)
          for lf = (when l (cl-ppcre:split #\Tab l))
          while l
          do (destructuring-bind (us ls vs)
                 lf
               (let ((user-id (parse-integer us))
                     (link-id (parse-integer ls))
                     (vote (parse-integer vs)))
                 (push (cons link-id vote)
                       (gethash user-id luvl))))
          finally (return luvl))))

(defun compute-recall (test-votes predicted-votes)
  (let ((luvl (read-votes test-votes)))
    (with-open-file (f predicted-votes)
      (loop for l = (read-line f nil nil)
            for v = (when l 
                      (mapcar #'parse-integer
                              (cl-ppcre:split " " l)))
            for ul = (when v (gethash (first v) luvl))
            for recall = (when (and v (rest v)) (/ (length (intersection (mapcar #'car ul)
                                                          (rest v)))
                                    (length ul)))
            for precision = (when (and v (rest v)) (/ (length (intersection (mapcar #'car ul)
                                                             (rest v)))
                                       (length (rest v))))
            while l
            when recall
            collect (coerce recall 'float) into rs
            when precision
            collect (coerce precision 'float) into ps
            finally (return (values (sapa:sample-mean rs)
                                    (sapa:sample-mean ps)))))))