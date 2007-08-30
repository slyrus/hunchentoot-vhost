;;; file: hunchentoot-vhost.lisp
;;;
;;; Copyright (c) 2007 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; This file contains code that is a derivative of work that is:
;;; Copyright (c) 2004-2007, Dr. Edmund Weitz.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:hunchentoot-vhost)

(defclass virtual-host ()
  ((name :accessor name
         :initarg :name
         :documentation "The name of the virtual host")
   (handled-host-list :accessor handled-host-list
                      :initarg :handled-host-list
                      :documentation "A list of the hostnames which
  for which this virtual host handles the requests if a suffix of the
  host of the request matches one of the names in this list.")
   (dispatch-table :accessor dispatch-table
               :initarg :dispatch-table
               :initform (list #'dispatch-easy-virtual-handlers)
               :documentation "A list of dispatch functions to be
  called for this virtual host.")
   (easy-handler-alist :accessor easy-handler-alist
               :initarg :easy-handler-alist
               :initform nil
               :documentation "A list of virtual-easy-handler
  functions to be called for this virtual host.")
   (server :accessor server
               :initarg :server
               :initform nil
               :documentation "The hunchentoot::server for which this
               virtual-host should be used. If nil, use this virtual
               host for all servers."))
  (:documentation "An object of this class contains information about
  a virtual host to be handled by the hunchentoot-vhost machinery."))

(defun host-name (request)
  "Returns just the host portion of the 'Host' incoming http header
value, rather than either host or host:port if the port is specified."
  (let ((host-and-port (hunchentoot:host request)))
    (let ((colon-pos (position #\: host-and-port)))
      (if colon-pos
          (subseq host-and-port 0 colon-pos)
          host-and-port))))

(defvar *server-vhost-list-hash-table* (make-hash-table))

(setf hunchentoot:*meta-dispatcher*
      (lambda (server)
        (let ((hash (gethash server *server-vhost-list-hash-table*)))
          (if hash
              (progn
                (print (cons 'moose server))
                (cons #'dispatch-virtual-host-handlers
                      hunchentoot:*dispatch-table*))
              hunchentoot:*dispatch-table*))))

(defun make-virtual-host (name
                          hosts
                          &key
                          server
                          (class 'virtual-host))
  "Creates a virtual host of the specified name that handles requests
whose host suffixes match one of the specified hosts, are the single
specified host if it is an string rather than a list of strings."
  (when (atom hosts)
    (setf hosts (list hosts)))
  
  (setf (gethash server *server-vhost-list-hash-table*)
        (delete name (gethash server *server-vhost-list-hash-table*)
                :key #'name
                :test #'equal))
  (let ((vhost (make-instance class
                              :name name
                              :handled-host-list hosts
                              :server server)))
    (push vhost (gethash server *server-vhost-list-hash-table*))
    vhost))

(defun virtual-host-handles (vhost host-name)
  "Returns the name of the host handled by this virtual host whose
suffix is host-name if it exists, otherwise returns NIL."
  (find-if (lambda (x)
             (let ((pos (search x host-name)))
               (when pos
                 (= pos (- (length host-name)
                           (length x))))))
           (handled-host-list vhost)))

(defun dispatch-virtual-host-handlers (request)
  "The dispatch function for the vhost handlers. This should be added
to the hunchentoot:*dispatch-table*."
  (let ((vhost
         (loop for vhost in (gethash hunchentoot::*server* *server-vhost-list-hash-table*)
            do (when (virtual-host-handles vhost (host-name request))
                 (return vhost)))))
    (when vhost
      (loop for dispatch-fn in (dispatch-table vhost)
         for action = (funcall dispatch-fn request vhost)
         do (print action)
         when action return action))))

(defun dispatch-easy-virtual-handlers (request vhost)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-EASY-VIRTUAL-HANDLER, if there is one."
  (loop for (uri server-names easy-handler) in (easy-handler-alist vhost)
     when (and (or (eq server-names t)
                   (find (hunchentoot::server-name hunchentoot::*server*) server-names :test #'eq))
               (cond ((stringp uri)
                      (string= (hunchentoot::script-name request) uri))
                     (t (funcall uri request))))
     do (return easy-handler)))

(defmacro define-easy-virtual-handler (virtual-host description lambda-list &body body)
  "Defines an easy-virtual-handler for use with a given
virtual-host. See hunchentoot:define-easy-handler for documentation of
the description and lambda-list arguments."
  (destructuring-bind (name &key uri (server-names t)
                       (default-parameter-type ''string)
                       (default-request-type :both))
      description
    `(progn
       ,@(when uri
               (list
                (hunchentoot::with-rebinding (uri)
                  `(progn
                     (setf (easy-handler-alist ,virtual-host)
                           (delete-if (lambda (list)
                                        (or (equal ,uri (first list))
                                            (eq ',name (third list))))
                                      (easy-handler-alist ,virtual-host)))
                     (push (list ,uri ,server-names ',name) (easy-handler-alist ,virtual-host))))))
       (defun ,name (&key ,@(loop for part in lambda-list
                               collect
                                 (hunchentoot::make-defun-parameter part
                                                                    default-parameter-type
                                                                    default-request-type)))
         ,@body))))

(defun create-virtual-host-prefix-dispatcher (prefix page-function)
  "Creates a dispatch function which will dispatch to the
function denoted by PAGE-FUNCTION if the file name of the current
request starts with the string PREFIX."
  (lambda (request vhost)
    (when (virtual-host-handles vhost (host-name request))
      (let ((mismatch (mismatch (hunchentoot::script-name request) prefix
                                :test #'char=)))
        (and (or (null mismatch)
                 (>= mismatch (length prefix)))
             page-function)))))

(defun create-virtual-host-folder-dispatcher-and-handler (uri-prefix base-path &optional content-type)
  "Creates and returns a dispatch function which will dispatch to a
handler function which emits the file relative to BASE-PATH that is
denoted by the URI of the request relative to URI-PREFIX.  URI-PREFIX
must be a string ending with a slash, BASE-PATH must be a pathname
designator for an existing directory.  If CONTENT-TYPE is not NIL,
it'll be the content type used for all files in the folder."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (error "~S must be string ending with a slash." uri-prefix))
  (when (or (pathname-name base-path)
            (pathname-type base-path))
    (error "~S is supposed to denote a directory." base-path))
  (flet ((handler ()
           (let* ((script-name (hunchentoot::url-decode (hunchentoot::script-name)))
                  (script-path (enough-namestring (ppcre::regex-replace-all "\\\\" script-name "/")
                                                  uri-prefix))
                  (script-path-directory (pathname-directory script-path)))
             (unless (or (stringp script-path-directory)
                         (null script-path-directory)
                         (and (listp script-path-directory)
                              (eq (first script-path-directory) :relative)
                              (loop for component in (rest script-path-directory)
                                    always (stringp component))))
               (setf (hunchentoot::return-code) hunchentoot::+http-forbidden+)
               (throw 'handler-done nil))
             (hunchentoot::handle-static-file (merge-pathnames script-path base-path) content-type))))
    (create-virtual-host-prefix-dispatcher uri-prefix #'handler)))
