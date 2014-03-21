(in-package :fern)

(defun make-queue ()
  "Makes a thread-safe FIFO queue (unbounded)."
  (make-instance 'jpl-queues:synchronized-queue
                 :queue (make-instance 'jpl-queues:unbounded-fifo-queue)))

(defun cons-map (fn list)
  "A looper that can handle cons cells:

     (cons-map (lambda (x) (+ x 4)) '(1 2 . 3))
       => (5 6 . 7)"
  (let* ((cur (car list))
         (next (cdr list))
         (res nil))
    (loop while cur do
      (cond ((eq (type-of next) 'cons)
             (setf res (append res (list (funcall fn cur)))
                   cur (car next)
                   next (cdr next)))
            (t
             (setf res (append res (cons (funcall fn cur)
                                         (when next (funcall fn next))))
                   cur nil))))
    res))

(defun recurse-tree (tree fn &key replace)
  "Recursively call a function for each leaf in a tree, collecting the results
   as we go along. Works on lists, arrays, and hash tables (calls the given
   function for the keys *and* values of the hash).

   Allows specifying a replace function which is called for each value passed
   through (atoms, lists, arrays, hash tables) and when it returns a value, will
   return that value for the form, completely ignoring the value of the actual
   list/array/hash. This allows very specific value replacement. For instance,
   if you want to recurse over a tree, but every time you see the list '(1 2 3)
   return the symbol 'X, this lets you do that:

     (recurse-tree '(1 2 (1 2 3) 4) '1+
                   :replace (lambda (x)
                              (when (equalp x '(1 2 3))
                                'harl)))
       => (2 3 HARL 5)"
  (let* ((replace-val (and replace (funcall replace tree))))
    (when replace-val (return-from recurse-tree replace-val))
    (cond ((stringp tree)
           (funcall fn tree))
          ((arrayp tree)
           (loop for x across tree collect (recurse-tree x fn :replace replace)))
          ((eq (type-of tree) 'cons)
           (cons-map (lambda (x) (recurse-tree x fn :replace replace)) tree))
          ((hash-table-p tree)
           (let ((hash (make-hash-table :test (hash-table-test tree))))
             (loop for key being the hash-keys of tree
                   for val being the hash-values of tree do
               (setf (gethash (recurse-tree key fn :replace replace) hash) (recurse-tree val fn :replace replace)))
             hash))
          (t
           (funcall fn tree)))))

