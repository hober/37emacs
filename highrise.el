;;; highrise.el --- Client for 37Signals' Highrise API

;; Copyright (C) 2007  Edward O'Connor

;; EUDC support derived from `eudc-mab-query-internal' in eudcb-mab.el, which is
;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: comm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides a programmatic client for the Highrise API, and provides an
;; EUDC backend for the same.


;;; History:

;; 2007-08-03: Initial version. Probably not quite for general
;;             consumption (yet).

;;; Code:

(require 'cl) ; Only for `remove-if'
(require 'eudc)
(require 'url)
(require 'xml)

(defgroup highrise nil
  "Emacs interface to 37signals' Highrise."
  :group 'processes
  :prefix "highrise-"
  :link '(url-link :tag "Highrise"
                   "http://www.highrisehq.com/")
  :link '(url-link :tag "Latest version of highrise.el"
                   "http://edward.oconnor.cx/elisp/highrise.el")
  :link '(url-link :tag "Highrise API documentation"
                   "http://developer.37signals.com/highrise/")
  :link '(url-link :tag "Highrise API discussion forum"
                   "http://forum.37signals.com/highrise/forums/15"))

(defcustom highrise-auth-token nil
  "Your Highrise authentication token.
Find it by clicking on 'My Info', then on \"Reveal authentication token
for feeds API\" link below your password or OpenID."
  :group 'highrise
  :type '(string))

(defcustom highrise-server nil
  "Your Highrise server name.
If you use http://foo.highrisehq.com, set `highrise-server' to \"foo\"."
  :group 'highrise
  :type '(string))



;;; Parsing

(eval-and-compile
  (defvar highrise-parse-table (make-hash-table :test 'eq)
    "Mapping of Highrise data types to lisp functions which parse them."))

;; Elementary data types

(defun highrise-parse-string (xml)
  "Return the textContent of XML."
  (mapconcat 'identity (xml-node-children xml) ""))
(puthash 'string 'highrise-parse-string highrise-parse-table)

(defun highrise-parse-integer (xml)
  "Extract an integer from XML's textContent."
  (string-to-number (highrise-parse-string xml)))
(puthash 'integer 'highrise-parse-integer highrise-parse-table)

(defun highrise-parse-datetime (xml)
  "Extract a datetime from XML's textContent."
  (date-to-time (highrise-parse-string xml)))
(puthash 'datetime 'highrise-parse-datetime highrise-parse-table)

;; The `highrise-define-data' macro.

(defun highrise-parse-child (xml field &optional child-parser)
  "Parse XML's child node named FIELD.
If non-nil, CHILD-PARSER is the function to use when parsing each child."
  (let ((parser (or child-parser 'highrise-parse))
        (child (car (xml-get-children xml field))))
    (when child
      (funcall parser child))))

(eval-and-compile
  (defun highrise-define-data-expand-field (spec)
    "Expand SPEC for `highrise-define-data', which see."
    (let* ((field (car spec))
           (type (cadr spec))
           (parser (gethash type highrise-parse-table 'highrise-parse))
           (spec-keyword (intern (format ":%s" field))))
      `(cons ,spec-keyword
             (highrise-parse-child xml ',field ',parser)))))

(defmacro highrise-define-data (spec &rest field-specs)
  "Define a Highrise data type according to SPEC.
If present, FIELD-SPECS should contain entries of the form (name type)."
  (let* ((name (car spec))
         (parser-name (intern (format "highrise-parse-%s" name)))
         (keyword (intern (format ":%s" name))))
    (when (memq :letter spec)
      (add-to-list 'spec :id)
      (add-to-list 'spec :editable)
      (add-to-list 'spec :owned)
      (add-to-list 'spec :hidable)
      (add-to-list 'spec :subjectable)
      (add-to-list 'field-specs '(body string))
      (add-to-list 'field-specs '(kase-id string))
      (add-to-list 'field-specs '(attachments array)))
    (when (memq :party spec)
      (add-to-list 'spec :id)
      (add-to-list 'spec :editable)
      (add-to-list 'spec :owned)
      (add-to-list 'spec :hidable)
      (add-to-list 'field-specs '(background string))
      (add-to-list 'field-specs '(contact-data contact-data)))
    (when (memq :id spec)
      (add-to-list 'field-specs '(id integer)))
    (when (memq :editable spec)
      (add-to-list 'field-specs '(created-at datetime))
      (add-to-list 'field-specs '(updated-at datetime)))
    (when (memq :owned spec)
      (add-to-list 'field-specs '(author-id integer))
      (add-to-list 'field-specs '(owner-id owner)))
    (when (memq :hidable spec)
      (add-to-list 'field-specs '(visible-to string))
      (add-to-list 'field-specs '(group-id integer)))
    (when (memq :subjectable spec)
      (add-to-list 'field-specs '(subject-id integer))
      (add-to-list 'field-specs '(subject-type string)))
    (when (memq :locatable spec)
      (add-to-list 'field-specs '(location string)))
    `(progn
       (puthash ',name ',parser-name highrise-parse-table)
       (defun ,parser-name (xml)
         "Transform Highrise XML into an idiomatic lisp data structure."
         (vector ,keyword
                 ,(if (memq :id spec)
                      '(highrise-parse-child xml 'id 'highrise-parse-integer)
                    nil)
                 (list
                  ,@(mapcar 'highrise-define-data-expand-field
                            field-specs)))))))
(put 'highrise-define-data 'lisp-indent-function 1)

;; Accessors for the fields of a parsed Highrise object.

(defsubst highrise-data-type (object)
  "Return OBJECT's Highrise data type."
  (aref object 0))

(defsubst highrise-data-id (object)
  "Return OBJECT's Highrise ID."
  (aref object 1))

(defsubst highrise-data-attrs (object)
  "Return OBJECT's Highrise attributes."
  (aref object 2))

(defun highrise-data-getattr (key object &optional default)
  "Fetch the value of KEY in OBJECT's attributes.
If OBJECT lacks KEY, DEFAULT is used."
  (let ((attrs (highrise-data-attrs object)))
    (assoc-default key attrs nil default)))

;; Highrise data type definitions.

(highrise-define-data (kase :id :editable :owned :hidable)
  (closed-at datetime)
  (name string))

(highrise-define-data (comment :id :editable :owned :hidable)
  ;; FIXME: no updated-at
  ;; FIXME: no owner-id
  (parent-id integer)
  (body string))

(highrise-define-data (company :party)
  (name string))

(highrise-define-data (email-address :id :locatable)
  (address string))

(highrise-define-data (phone-number :id :locatable)
  (number string))

(highrise-define-data (address :id :locatable)
  (street string)
  (city string)
  (state string)
  (zip string)
  (country string))

(highrise-define-data (instant-messenger :id :locatable)
  (protocol string)
  (address string))

(highrise-define-data (web-address :id :locatable)
  (url string))

(highrise-define-data (contact-data)
  (addresses array)
  (email-addresses array)
  (instant-messengers array)
  (phone-numbers array)
  (web-addresses array))

(highrise-define-data (attachment :id)
  (url string))

(highrise-define-data (email :letter)
  (title string))

(highrise-define-data (note :letter))

(highrise-define-data (person :party)
  (first-name string)
  (last-name string)
  (title string)
  (company-id integer))

(highrise-define-data (task :id :editable :owned :subjectable)
  (recording-id integer)
  (body string)
  (frame string)
  (alert-at datetime)
  (done-at datetime)
  (category-id integer))

(highrise-define-data (tag :id)
  (name string))

(highrise-define-data (user :id :editable)
  (name string)
  (person-id integer))

(highrise-define-data (group :id)
  (name string)
  (users array))

(highrise-define-data (membership :id :editable)
  (group-id integer)
  (user-id integer))

;; Custom data type definitions

(defun highrise-parse-record (xml)
  "Parse the Highrise <record/> element XML."
  (if (xml-get-children xml 'company-id)
      (highrise-parse-person xml)
    (highrise-parse-company xml)))
(puthash 'record 'highrise-parse-record highrise-parse-table)

(defun highrise-parse-array (xml &optional child-parser)
  "Parse and return the children of XML.
If non-nil, CHILD-PARSER is the function to use when parsing each child."
  (let ((children (xml-node-children xml))
        (parser (or child-parser 'highrise-parse)))
    (mapcar parser (remove-if 'stringp children))))
(puthash 'array 'highrise-parse-array highrise-parse-table)

;; The Highrise parser.

(defun highrise-parse (xml)
  "Transform Highrise xml XML into idiomatic lisp."
  (let* ((name (xml-node-name xml))
         (type (xml-get-attribute-or-nil xml 'type))
         (parser (gethash name highrise-parse-table)))
    (cond
     ((equal type "array") (highrise-parse-array xml))
     ((equal type "integer") (highrise-parse-integer xml))
     ((equal type "datetime") (highrise-parse-datetime xml))

     (parser (funcall parser xml))
     (t
      (warn "Unknown highrise object, %s" name)
      (vector :unkown nil nil xml)))))



;;; Network operations.

;; Placate the byte compiler.
(defvar url-http-end-of-headers)
(defvar highrise-debug nil)

(defun highrise-response (buffer)
  "Process the XML response from Highrise which resides in BUFFER."
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (highrise-parse (car (xml-parse-region (point) (point-max))))))
    (unless highrise-debug
      (kill-buffer buffer))))

(defun highrise-url (&optional relative)
  "Return the URL of your Highrise server."
  (let* ((url (format "http://%s.highrisehq.com/%s"
                      highrise-server (or relative "")))
         (parsed (url-generic-parse-url url)))
    (url-set-user parsed highrise-auth-token)
    (url-set-password parsed "")
    parsed))

;;; API call definition

;; TODO: HTTP verbs other than GET
;;    - requires ability to generate Highrise XML, not just consume it
(defmacro highrise-define-api (name &optional singular &rest things)
  "Define a Highrise API call named NAME.
If non-nil, SINGULAR specifies what individual objects are called;
NAME is typically plural. If given, THINGS name other objects which NAME
can own, e.g. people can own notes."
  (let ((fname (intern (format "highrise-api/%s" name)))
        (singular-fname (intern (format "highrise-api/%s" singular))))
    `(progn
       ,(when singular
          `(defun ,singular-fname (id)
             (highrise-response
              (let ((url-package-name "highrise.el"))
                (url-retrieve-synchronously
                 (highrise-url (format "%s/%d.xml" ,(symbol-name name) id)))))))
       (defun ,fname ()
         (highrise-response
          (let ((url-package-name "highrise.el"))
            (url-retrieve-synchronously
             (highrise-url (format "%s.xml" ,(symbol-name name)))))))
       ,@(mapcar
          (lambda (thing)
            (let ((thing-fname (intern (format "%s/%s" singular-fname thing))))
              `(defun ,thing-fname (id)
                 (highrise-response
                  (let ((url-package-name "highrise.el"))
                    (url-retrieve-synchronously
                     (highrise-url
                      (format "%s/%d/%s"
                              ,(symbol-name name) id
                              ,(symbol-name thing)))))))))
          things))))
(put 'highrise-define-api 'lisp-indent-function 2)

;;; Highrise API calls

(highrise-define-api tags)

(highrise-define-api people person
  tasks notes emails tags)
;; FIXME: paginated, ?n={offset}
;; FIXME: title, ?title={title}
;; FIXME: tagged, ?tag_id={tag-id}
;; FIXME: search, /people/search.xml?term={term}
;; FIXME: since, ?since=YYYYMMDDHHMMSS

(highrise-define-api companies company
  people tasks notes emails tags)
;; FIXME: paginated, ?n={offset}
;; FIXME: tagged, ?tag_id={tag-id}
;; FIXME: search, /people/search.xml?term={term}
;; FIXME: since, ?since=YYYYMMDDHHMMSS

(highrise-define-api parties)
(highrise-define-api parties/recently_added)
(highrise-define-api parties/recently_viewed)

(highrise-define-api tasks task)
(highrise-define-api tasks/upcoming)
(highrise-define-api tasks/assigned)
(highrise-define-api tasks/completed)
;; FIXME: /tasks?collection=assigned
;; FIXME: /tasks?collection=completed

(highrise-define-api kases kase
  tasks notes emails)
(highrise-define-api kases/open)
(highrise-define-api kases/closed)
;; FIXME: highrise-kases is bogus

(highrise-define-api notes note
  comments)
;; FIXME: highrise-notes is bogus

(highrise-define-api emails email
  comments)
;; FIXME: highrise-emails is bogus

(highrise-define-api comments comment)
;; FIXME: highrise-comments is bogus

(highrise-define-api users user)
(highrise-define-api groups group)
(highrise-define-api memberships membership)



;;; EUDC support

(eudc-protocol-set 'eudc-query-function 'eudc-highrise-query 'highrise)
(eudc-protocol-set 'eudc-list-attributes-function nil 'highrise)
(defvar eudc-highrise-conversion-alist nil)
(eudc-protocol-set 'eudc-highrise-conversion-alist nil 'highrise)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes nil 'highrise)

(defun eudc-highrise-query (query &optional return-attrs)
  "Search Highrise for QUERY, an EUDC internal search object.
If non-nil, RETURN-ATTRS specifies which fields the caller is
interested in."
  (let (result)
    (dolist (party (highrise-api/parties))
      (let* ((matched t)
             (id (highrise-data-id party))
             (first-name (highrise-data-getattr :first-name party ""))
             (last-name (highrise-data-getattr :last-name party ""))
             (name (highrise-data-getattr :name party ""))
             (contact-data (highrise-data-getattr :contact-data party))
             (email
              (mapconcat (lambda (email)
                           (highrise-data-getattr :address email))
                         (highrise-data-getattr :email-addresses contact-data)
                         " "))
             (phone
              (mapconcat (lambda (number)
                           (highrise-data-getattr :number number))
                         (highrise-data-getattr :phone-numbers contact-data)
                         " ")))
        (dolist (term query)
          (cond ((eq (car term) 'name)
                 (unless (string-match
                          (cdr term)
                          (format "%s %s %s" first-name name last-name))
                   (setq matched nil)))
                ((eq (car term) 'email)
                 (unless (string= (cdr term) email)
                   (setq matched nil)))
                ((eq (car term) 'phone)
                 (unless (string= (cdr term) phone)
                   (setq matched nil)))))
        (when matched
          (setq result
                (cons `((firstname . ,first-name)
                        (lastname . ,last-name)
                        (name . ,(or name (concat first-name " " last-name)))
                        (phone . ,phone)
                        (email . ,email))
                      result)))))
    (if (null return-attrs)
	result
      (let (eudc-result)
	(dolist (entry result)
	  (let (entry-attrs abort)
	    (dolist (attr entry)
	      (when (memq (car attr) return-attrs)
		(if (= (length (cdr attr)) 0)
		    (setq abort t)
		  (setq entry-attrs
			(cons attr entry-attrs)))))
	    (if (and entry-attrs (not abort))
		(setq eudc-result
		      (cons entry-attrs eudc-result)))))
	eudc-result))))

(eudc-register-protocol 'highrise)

(provide 'highrise)
;;; highrise.el ends here
