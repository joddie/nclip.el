;;; nclip-server.el --- Pure-Elisp server for nclip.el

;; Copyright (C) 2013 Jon Oddie <jonxfield@gmail.com>
;;
;; Author: Jon Oddie <jonxfield@gmail.com>
;; URL: http://www.github.com/maio/nclip.el
;; Version: 0.1
;; Keywords: nclip, clipboard, network
;; Package-Requires: ((nclip "2") (web-server "0.1.1") (emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;;; Commentary:

;; A pure-Elisp server for the `nclip' networked clipboard server,
;; which can be used in place of "nclip.py" or "nclip.rb".  

;; Quick summary:

;; 1. On your local workstation, run `nclip-server-start'.

;; 2. On the remote Emacs, run `turn-on-nclip'.

;; The kill-rings of the local and remote Emacs should then be
;; automatically synchronised.  If your local Emacs is built with a
;; GUI toolkit (X/Windows/Mac OS X), it should also synchronise the
;; system clipboard with the kill-ring.  (Unlike "nclip.py" and
;; "nclip.rb", the server started by `nclip-server-start' does not
;; interact with the system clipboard directly.)

;; See "nclip.el" for details on how to set up SSH port forwarding and
;; token authentication to allow the nclip server and client to
;; communicate.  In particular, the value of `nclip-auth-token' must
;; be the same in the local and remote Emacs.

;;;; Code:

(require 'web-server)
(require 'nclip)
(require 'macroexp)

(defvar nclip-server-port 2547)
(defvar nclip-server-ws-server nil)

;;;###autoload
(defun nclip-server-start ()
  "Start the nclip networked clipboard server.

Normally, you should run this command (or \"nclip.py\" or
\"nclip.rb\") on your local workstation, and run `turn-on-nclip'
in Emacs instances on remote machines that you connect to via
SSH.  Killing and yanking in the remote Emacs will then affect
the kill-ring (and/or system clipboard) on your local computer.

See the documentation in nclip.el for details on how to set up
SSH forwarding and token authorization.  The value of
`nclip-auth-token' in the local and remote Emacs must match."
  (interactive)
  (setq nclip-server-ws-server
        (ws-start
         `(((:GET . "") . ,#'nclip-server-get)
           ((:POST . "") . ,#'nclip-server-post))
         nclip-server-port
         (get-buffer-create "*nclip server log*")
         :host 'local))
  (message "NClip server started on localhost:%d" nclip-server-port))

;;;###autoload
(defun nclip-server-stop ()
  "Stop the nclip networked clipboard server."
  (interactive)
  (if (ws-server-p nclip-server-ws-server)
      (progn
        (ws-stop nclip-server-ws-server)
        (message "NClip server stopped"))
    (message "NClip server not running")))

;;;###autoload
(defun nclip-server-restart ()
  "Stop and restart the nclip clipboard server."
  (interactive)
  (nclip-server-stop)
  (nclip-server-start))

(defmacro nclip-with-authorisation (request &rest body)
  (declare (indent 1))
  (let ((request-local (make-symbol "request")))
    `(let ((,request-local ,request))
       (if (nclip-server-authorised ,request-local)
           ,(macroexp-progn body)
         (with-slots (process) ,request-local
           (ws-response-header process 500))))))

(defun nclip-server-get (request)
  (nclip-with-authorisation request 
    (with-slots (process) request
      (ws-response-header process 200 '("Content-type" . "text/plain"))
      (process-send-string process (current-kill 0)))))

(defun nclip-server-post (request)
  (nclip-with-authorisation request
    (with-slots (process pending) request
      (if (string-match "\r\n\r\n" pending)
          (let ((paste (substring pending (match-end 0))))
            (ws-response-header process 200 '("Content-type" . "text/plain"))
            (kill-new paste))
        (error "No request body")))))

(defun nclip-server-authorised (request)
  (with-slots (headers) request
    (assoc nclip-auth-token headers)))

(provide 'nclip-server)

;;; nclip-server.el ends here
