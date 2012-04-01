(require 'alexandria)
(require 'cl-fad)
(require 'cl-ppcre)

(defstruct dump-parse-ctx
  link-ids user-ids
  out-votes
  out-users
  out-links)

(defun format-tab-delimited-list (s list)
  (loop for (i next) on list
        do (princ i s)
        if next 
        do (princ #\Tab s)
        else do (princ #\Newline s)))

(defun resolve-user (user ctx)
  (let* ((ht (dump-parse-ctx-user-ids ctx))
         (user-id (gethash user ht)))
    (or user-id
        (progn 
          (setf user-id (hash-table-count ht))
          (setf (gethash user ht) user-id)
          (format-tab-delimited-list (dump-parse-ctx-out-users ctx)
                                     (list user-id user))
          user-id))))

(defun resolve-link (linkid ctx subreddit url creator time title thumbnail)
  (let* ((ht (dump-parse-ctx-link-ids ctx))
         (link-id (gethash linkid ht)))
    (or link-id
        (progn 
          (setf link-id (hash-table-count ht))
          (setf (gethash linkid ht) link-id)
          (format-tab-delimited-list (dump-parse-ctx-out-links ctx)
                                     (list link-id linkid subreddit 
                                           url creator time title thumbnail))
          link-id))))



(defun parse-dumper-stream (s ctx)
  (loop for l = (read-line s nil nil)
        for fields = (when l (cl-ppcre:split #\Tab l))
        while l
        do (destructuring-bind (user vote linkid subreddit url creator time title thumbnail)
               fields
             (let ((user-id (resolve-user user ctx))
                   (link-id (resolve-link linkid ctx
                                          subreddit url creator time title thumbnail)))
               (format-tab-delimited-list (dump-parse-ctx-out-votes ctx)
                                          (list user-id link-id vote))))))

(defun import-many-files (in-dir out-dir &optional limit)
  (with-open-file (votes (merge-pathnames "votes.txt" out-dir)
                         :direction :output :if-exists :supersede)
    (with-open-file (users (merge-pathnames "users.txt" out-dir)
                           :direction :output :if-exists :supersede)
      (with-open-file (links (merge-pathnames "links.txt" out-dir)
                       :direction :output :if-exists :supersede)
        (with-open-file (log (merge-pathnames "log.txt" out-dir)
                             :direction :output :if-exists :supersede)
          (let ((ctx (make-dump-parse-ctx :link-ids (make-hash-table :test 'equal)
                                          :user-ids (make-hash-table :test 'equal)
                                          :out-votes votes
                                          :out-links links
                                          :out-users users)))
          (loop repeat (or limit most-positive-fixnum)
                for file in (cl-fad:list-directory in-dir)
                do (format log "begin ~a, ~a~%" (pathname-name file) (get-universal-time))
                do (handler-case (with-open-file (s file)
                                   (parse-dumper-stream s ctx))
                     (error (e) (format log "error: ~a (~s)~%" e e)))
                do (format log "end ~a, ~a~%" (pathname-name file) (get-universal-time)))))))))
