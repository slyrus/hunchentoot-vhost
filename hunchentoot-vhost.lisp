;;; file: hunchentoot-vhost.lisp
;;;
;;; Copyright (c) 2007 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
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
   (dispatcher :accessor dispatcher
               :initarg :dispatcher
               :initform nil
               :documentation "A list of dispatch functions to be
  called for this virtual host."))
  (:documentation "An object of this class contains information about
  a virtual host to be handled by the hunchentoot-vhost machinery."))

(defvar *virtual-host-dispatch-list* nil "A list of virtual-host instances to
be handled by dispatch-virtual-host-handlers")

(defun host-name (request)
  "Returns just the host portion of the 'Host' incoming http header
value, rather than either host or host:port if the port is specified."
  (let ((host-and-port (hunchentoot:host request)))
    (let ((colon-pos (position #\: host-and-port)))
      (if colon-pos
          (subseq host-and-port 0 colon-pos)
          host-and-port))))

(defun make-virtual-host (name hosts)
  "Creates a virtual host of the specified name that handles requests
whose host suffixes match one of the specified hosts, are the single
specified host if it is an string rather than a list of strings."
  (when (atom hosts)
    (setf hosts (list hosts)))
  (setf *virtual-host-dispatch-list*
        (delete-if (lambda (vhost)
                    (equal (name vhost) name))
                   *virtual-host-dispatch-list*))
  (car (push (make-instance 'virtual-host
                            :name name
                            :handled-host-list hosts)
             *virtual-host-dispatch-list*)))

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
         (loop for vhost in *virtual-host-dispatch-list*
            do (when (virtual-host-handles vhost (host-name request))
                 (return vhost)))))
    (when vhost
      (print vhost)
      (dispatch-easy-virtual-handlers request vhost))))

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
                     (setf (dispatcher ,virtual-host)
                           (delete-if (lambda (list)
                                        (or (equal ,uri (first list))
                                            (eq ',name (third list))))
                                      (dispatcher ,virtual-host)))
                     (push (list ,uri ,server-names ',name) (dispatcher ,virtual-host))))))
       (defun ,name (&key ,@(loop for part in lambda-list
                               collect
                                 (hunchentoot::make-defun-parameter part
                                                                    default-parameter-type
                                                                    default-request-type)))
         ,@body))))

(defun dispatch-easy-virtual-handlers (request vhost)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-EASY-VIRTUAL-HANDLER, if there is one."
  (loop for (uri server-names easy-handler) in (dispatcher vhost)
     when (and (or (eq server-names t)
                   (find (hunchentoot::server-name hunchentoot::*server*) server-names :test #'eq))
               (cond ((stringp uri)
                      (string= (hunchentoot::script-name request) uri))
                     (t (funcall uri request))))
     do (return easy-handler)))
