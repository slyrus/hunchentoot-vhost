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
           virtual-host should be used. If nil, use this virtual host
           for all servers."))
  (:documentation "An object of this class contains information about
  a virtual host to be handled by the hunchentoot-vhost machinery."))

(defun host-name (&optional request)
  "Returns just the host portion of the 'Host' incoming http header
value, rather than either host or host:port if the port is specified."
  (let ((host-and-port (apply #'hunchentoot:host 
                              (when request (list request)))))
    (let ((colon-pos (position #\: host-and-port)))
      (if colon-pos
          (subseq host-and-port 0 colon-pos)
          host-and-port))))

(defun host-name-and-port (&optional request)
  "Returns the multiple values host and port (or nil if no port
  is specified) of the 'Host' incoming http header value, rather than
  either host or host:port if the port is specified."
  (let ((host-and-port (apply #'hunchentoot:host 
                              (when request (list request)))))
    (let ((colon-pos (position #\: host-and-port)))
      (if colon-pos
          (values (subseq host-and-port 0 colon-pos)
                  (subseq host-and-port (1+ colon-pos)))
          host-and-port))))

(defvar *server-vhost-list-hash-table* (make-hash-table)
  "A hash-table that stores whose keys are hunchentoot:server objects
  and whose values are a list of virtual hosts associated with the
  given hunchentoot:server object. To associate a hunchentoot-vhost
  object with a given server one either provides the server as a
  &key :server argument to make-virtual-host or calls add-virtual-host
  to add the hunchentoot-vhost to the given server.")

(defun add-virtual-host (vhost server)
  (pushnew #'dispatch-virtual-host-handlers hunchentoot::*dispatch-table*)
  (setf (gethash server *server-vhost-list-hash-table*)
        (delete (name vhost) (gethash server *server-vhost-list-hash-table*)
                :key #'name
                :test #'equal))
  (push vhost (gethash server *server-vhost-list-hash-table*)))

(defun make-virtual-host (name
                          hosts
                          &key
                          server)
  "Creates a virtual host of the specified name that handles requests
whose host suffixes match one of the specified hosts, are the single
specified host if it is an string rather than a list of strings."
  (when (atom hosts)
    (setf hosts (list hosts)))
  (let ((vhost (make-instance 'virtual-host
                              :name name
                              :handled-host-list hosts
                              :server server)))
    (when server (add-virtual-host vhost server))
    vhost))

(defun virtual-host-handles (vhost request &key exact)
  "Returns the name of the host handled by this virtual host whose
suffix is host-name if it exists, otherwise returns NIL."
  (multiple-value-bind (host-name host-port)
      (host-name-and-port request)
    (declare (ignore host-port))
    (if exact
        (find host-name (handled-host-list vhost) :test #'equal)
        (find-if (lambda (candidate-host)
                   ;; check to see if the candidate-host is an atom, if so
                   ;; it's the name of the virtual host and we should handle
                   ;; requests for this host on any port.
                   ;;
                   ;; TODO: we could add support for supporting
                   ;; '(host [port|(list of ports)]) style host designators.
                   (cond ((atom candidate-host)
                          (let ((pos (search candidate-host host-name)))
                            (when pos
                              (= pos (- (length host-name)
                                        (length candidate-host))))))))
                 (handled-host-list vhost)))))

(defparameter *virtual-host* nil)

(defun dispatch-virtual-host-handlers (request)
  "The dispatch function for the vhost handlers."
  (let ((vhost
         (or (loop for vhost in (gethash hunchentoot::*server*
                                         *server-vhost-list-hash-table*)
                do (when (virtual-host-handles vhost request :exact t)
                     (return vhost)))
             (loop for vhost in (gethash hunchentoot::*server*
                                         *server-vhost-list-hash-table*)
                do (when (virtual-host-handles vhost request)
                     (return vhost))))))
    (when vhost
      (loop for dispatch-fn in (dispatch-table vhost)
         for action = (let ((*virtual-host* vhost))
                        (funcall dispatch-fn request))
         when action return action))))

(defun dispatch-easy-virtual-handlers (request)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-EASY-VIRTUAL-HANDLER, if there is one."
  (loop for (uri server-names easy-handler) in (easy-handler-alist *virtual-host*)
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
    (declare (ignore name))
    (hunchentoot::with-unique-names (fn)
      `(let ((,fn (lambda (&key ,@(loop for part in lambda-list
                                    collect
                                    (hunchentoot::make-defun-parameter part
                                                                       default-parameter-type
                                                                       default-request-type)))
                   ,@body)))
         ,@(when uri
                 (list
                  (hunchentoot::with-rebinding (uri)
                    `(progn
                       (setf (easy-handler-alist ,virtual-host)
                             (delete-if (lambda (list)
                                          (equal ,uri (first list)))
                                        (easy-handler-alist ,virtual-host)))
                       (push (list ,uri ,server-names ,fn) (easy-handler-alist ,virtual-host))))))))))

