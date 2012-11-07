(defun read-link-subreddits (link-file fn)
  (with-open-file (f link-file)
    (loop with l = (read-line f nil)
          for lf = (when l (cl-ppcre:split #\Tab l))
          while l
          do (destructuring-bind (link-id linkname subreddit &rest rest)
                 lf
               (declare (ignore linkname rest))
               (funcall fn link-id subreddit)))))

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
                   