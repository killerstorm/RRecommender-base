(require 'cl-ppcre)
(require 'alexandria)

(defmacro get-or-init (place init-expr)
  "get or initialize idiom"
  `(or ,place (setf ,place ,init-expr)))

(defun format-tab-delimited-list (s list)
  (loop for (i next) on list
        do (princ i s)
        if next 
        do (princ #\Tab s)
        else do (princ #\Newline s)))

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
                 (push (cons user-id vote)
                       (gethash link-id luvl))))
          finally (return luvl))))

(defun import-votes (luvl luv min-link-votes)
  (loop for l being each hash-key of luvl
        using (hash-value uvl)
        when (>= (length uvl) min-link-votes)
        do (loop with ht = (get-or-init (gethash l luv)
                                        (make-hash-table))
                 for (u . v) in uvl
                 do (setf (gethash u ht) v))))

(defparameter *prune-log* t)

(defun prune-luv (luv min-link-votes min-user-votes)
  (let (pruned-users)

    (when *prune-log* (format t "starting with ~a links~%" (hash-table-count luv)))

    (loop for link being each hash-key of luv
          using (hash-value uv)
          when (< (hash-table-count uv) min-link-votes)
          do (remhash link luv))

    (when *prune-log* (format t "~a links left~%" (hash-table-count luv)))

    (loop with user-votes = (make-hash-table)
	  for link being each hash-key of luv
	  using (hash-value uv)
	  do (loop for user being each hash-key of uv
		   do (incf (gethash user user-votes 0)))
	  finally (progn
		    (setf pruned-users
			  (loop for user being each hash-key of user-votes
				using (hash-value c)
				when (< c min-user-votes)
				collect user))
		    (when *prune-log*
		      (format t "~a users, ~a pruned, ~a left~%"
			      (hash-table-count user-votes)
			      (length pruned-users)
			      (- (hash-table-count user-votes)
				 (length pruned-users))))))

    (when pruned-users
      (loop for link being each hash-key of luv
	    using (hash-value uv)
	    do (loop for pt in pruned-users
                     do (remhash pt uv)))
      (prune-luv luv min-link-votes min-user-votes))))

(defun split-links (luv min-info-votes perc-test-links perc-test-votes)
  (let ((test-links (butlast (alexandria:shuffle (alexandria:hash-table-keys luv))
                             (round (* (hash-table-count luv) (- 1 perc-test-links)))))
        (train-luv (make-hash-table))
        (info-luv (make-hash-table))
        (test-luv (make-hash-table)))
    (loop for link being each hash-key of luv
          using (hash-value uv)
          for is-test-link = (and (member link test-links)
                                  (> (hash-table-count uv) min-info-votes))
          if is-test-link 
          do (let* ((users (alexandria:hash-table-keys uv))
                    (n-info-votes (min (max min-info-votes
                                            (round (* (- 1 perc-test-votes)
                                                      (length users))))
                                       (- (length users) 1)))
                    (test-users (butlast users n-info-votes))
                    (info-votes (make-hash-table))
                    (test-votes (make-hash-table)))
               (loop for u being each hash-key of uv
                     using (hash-value v)
                     do (setf (gethash u (if (member u test-users)
                                             test-votes
                                             info-votes))
                              v))
               (setf (gethash link info-luv) info-votes
                     (gethash link test-luv) test-votes))
          else 
          do (setf (gethash link train-luv) uv))
        (values train-luv info-luv test-luv)))

(defun store-luv (file luv)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (loop for link being each hash-key of luv
          using (hash-value uv)
          do (loop for u being each hash-key of uv
                   using (hash-value v)
                   do (format-tab-delimited-list f (list link u v))))))

(defun create-split-data-set (input output-dir 
                              min-link-votes min-user-votes
                              min-info-votes perc-test-links perc-test-votes)
  (let ((luv (make-hash-table)))
    (import-votes (read-votes input) luv min-link-votes)
    #+sbcl
    (sb-ext:gc :full t)
    (prune-luv luv min-link-votes min-user-votes)
    (multiple-value-bind (train info test)
        (split-links luv min-info-votes perc-test-links perc-test-votes)
      (store-luv (merge-pathnames #p"train-votes.txt" output-dir) train)
      (store-luv (merge-pathnames #p"info-votes.txt" output-dir) info)
      (store-luv (merge-pathnames #p"test-votes.txt" output-dir) test))
    ()))