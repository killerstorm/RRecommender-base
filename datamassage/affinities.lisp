(require 'cl-ppcre)


(defun read-link-subreddits (link-file fn)
  (with-open-file (f link-file)
    (loop for l = (read-line f nil)
          for lf = (when l (cl-ppcre:split #\Tab l))
          while l
          do (destructuring-bind (link-id linkname subreddit &rest rest)
                 lf
               (declare (ignore linkname rest))
               (funcall fn (parse-integer link-id) subreddit)))))

(defun read-votes/f (file fn)
  (with-open-file (f file)
    (loop for l = (read-line f nil)
          for lf = (when l (cl-ppcre:split #\Tab l))
          while l
          do (destructuring-bind (us ls vs)
                 lf
               (let ((user-id (parse-integer us))
                     (link-id (parse-integer ls))
                     (vote (parse-integer vs)))
                 (funcall fn user-id link-id vote))))))

(defun user-subdreddit-affinities (link-file vote-file out-dir  min-user-votes min-user-subreddits)
  (let ((link->subreddit (make-hash-table))
        (user->subreddit->votes (make-hash-table))
        (subreddit->id (make-hash-table :test 'equal)))
    (read-link-subreddits link-file 
                          (lambda (link-id subdreddit)
                            (let ((subreddit-id 
                                   (get-or-init (gethash subdreddit subreddit->id)
                                                (hash-table-count subreddit->id))))
                              (setf (gethash link-id link->subreddit) subreddit-id))))
    (read-votes/f vote-file
                  (lambda (user-id link-id vote)
                    (when (plusp vote)
                      (let ((subreddit->votes 
                             (get-or-init (gethash user-id user->subreddit->votes)
                                          (make-hash-table)))
                            (sr-id (gethash link-id link->subreddit)))
                        (incf (gethash sr-id subreddit->votes 0) 1)))))
    (with-open-file (s (merge-pathnames #p"user-subreddits.txt" out-dir)
                       :direction :output :if-exists :supersede)
      (loop for user-id being each hash-key of user->subreddit->votes
            using (hash-value subreddit->votes)
            for total-votes = (loop for vc being each hash-value of subreddit->votes
                                    summing vc)
            when (and (>= total-votes min-user-votes)
                      (>= (hash-table-count subreddit->votes) min-user-subreddits))
            do (loop for sr-id being each hash-key of subreddit->votes
                     using (hash-value votec)
                     do (format-tab-delimited-list s (list user-id sr-id votec)))))
    (with-open-file (s (merge-pathnames #p"subreddits.txt" out-dir)
                       :direction :output :if-exists :supersede)
      (loop for subreddit being each hash-key of subreddit->id
            using (hash-value id)
            do (format-tab-delimited-list s (list id subreddit))))))

    
                                        