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

(defun user-vote-counts (luv)
  (loop with user-votes = (make-hash-table)
        for link being each hash-key of luv
        using (hash-value uv)
        do (loop for user being each hash-key of uv
                 do (incf (gethash user user-votes 0)))
        finally (return user-votes)))


(defparameter *prune-log* t)

(defun prune-luv (luv min-link-votes min-user-votes)
  (let (pruned-users)

    (when *prune-log* (format t "starting with ~a links~%" (hash-table-count luv)))

    (loop for link being each hash-key of luv
          using (hash-value uv)
          when (< (hash-table-count uv) min-link-votes)
          do (remhash link luv))

    (when *prune-log* (format t "~a links left~%" (hash-table-count luv)))

    (let ((uvc (user-vote-counts luv)))
      (setf pruned-users (loop for user being each hash-key of uvc
                               using (hash-value c)
                               when (< c min-user-votes)
                               collect user))
      (when *prune-log*
        (format t "~a users, ~a pruned, ~a left~%"
                (hash-table-count uvc)
                (length pruned-users)
                (- (hash-table-count uvc)
                   (length pruned-users)))))
    
    (when pruned-users
      (loop for link being each hash-key of luv
	    using (hash-value uv)
	    do (loop for pt in pruned-users
                     do (remhash pt uv)))
      (prune-luv luv min-link-votes min-user-votes))))

(defun split-links (luv min-link-votes min-user-votes
                    min-info-votes perc-test-links perc-test-users)
  (let ((test-links (butlast (alexandria:shuffle (alexandria:hash-table-keys luv))
                             (round (* (hash-table-count luv) (- 1 perc-test-links)))))
        (test-users-ht (make-hash-table))
        (train-users-ht)
        (train-luv (make-hash-table))
        (info-luv (make-hash-table))
        (test-luv (make-hash-table)))

    (loop for link being each hash-key of luv
          using (hash-value uv)
          unless (member link test-links)
          do (setf (gethash link train-luv) (alexandria:copy-hash-table uv)))

    (prune-luv train-luv min-link-votes min-user-votes)

    (setf train-users-ht (user-vote-counts train-luv))

    (loop for u in (butlast (alexandria:shuffle 
                             (alexandria:hash-table-keys train-users-ht))
                            (round (* (hash-table-count train-users-ht) (- 1 perc-test-users))))
          do (setf (gethash u test-users-ht) t))

    (loop for link being each hash-key of luv
          using (hash-value uv)
          when (and (member link test-links)
                    (> (hash-table-count uv) min-info-votes))
          do (let* ((link-users (alexandria:hash-table-keys uv))
                    (users (remove-if-not (lambda (u) (gethash u train-users-ht))
                                          link-users))
                    (info-users (remove-if (lambda (u) (gethash u test-users-ht))
                                           users))
                    (info-votes (make-hash-table))
                    (test-votes (make-hash-table)))
               (when (>= (length info-users min-info-users))
                 (loop for u being each hash-key of uv
                     using (hash-value v)
                     when (gethash u train-users-ht)
                     do (setf (gethash u (if (gethash u test-users)
                                             test-votes
                                             info-votes))
                              v))
                 (setf (gethash link info-luv) info-votes
                       (gethash link test-luv) test-votes))))
    (values train-luv info-luv test-luv)))

(defun store-luv (file luv)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (loop for link being each hash-key of luv
          using (hash-value uv)
          do (loop for u being each hash-key of uv
                   using (hash-value v)
                   do (format-tab-delimited-list f (list u link v))))))

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
      (store-luv (merge-pathnames #p"test-votes.txt" output-dir) test)
      (with-open-file (s (merge-pathnames #p"test-users.txt" output-dir) :direction :output)
        (loop for u being each hash-key of (user-vote-counts test)
              do (print u s))))))