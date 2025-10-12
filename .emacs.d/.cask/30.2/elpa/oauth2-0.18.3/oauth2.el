;;; oauth2.el --- OAuth 2.0 Authorization Protocol  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021 Free Software Foundation, Inc

;; Author: Julien Danjou <julien@danjou.info>
;; Maintainer: Xiyue Deng <manphiz@gmail.com>, emacs-devel@gnu.org
;; Version: 0.18.3
;; URL: https://elpa.gnu.org/packages/oauth2.html
;; Keywords: comm
;; Package-Requires: ((emacs "27.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of the OAuth 2.0 draft.
;;
;; The main entry point is `oauth2-auth-and-store' which will return a token
;; structure, which contains information needed for OAuth2 authentication,
;; e.g. access_token, refresh_token, etc.
;;
;; If the token needs to be refreshed, call `oauth2-refresh-access' on the token
;; and it will be refreshed with a new access_token.  The code will also store
;; the new value of the access token for reuse.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'plstore)
(require 'json)
(require 'url-http)

(defvar url-http-data)
(defvar url-http-method)
(defvar url-http-extra-headers)
(defvar url-callback-arguments)
(defvar url-callback-function)

(defgroup oauth2 nil
  "OAuth 2.0 Authorization Protocol."
  :group 'comm
  :link '(url-link :tag "Savannah"
                   "https://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/?h=externals/oauth2")
  :link '(url-link :tag "ELPA" "https://elpa.gnu.org/packages/oauth2.html"))

(defcustom oauth2-token-file (locate-user-emacs-file "oauth2.plstore")
  "File path where store OAuth tokens."
  :group 'oauth2
  :type 'file)

(defvar oauth2-debug nil
  "Enable verbose logging in oauth2 to help debugging.")

(defvar oauth2--url-advice nil)
(defvar oauth2--token-data)

(defvar oauth2--default-redirect-uri "urn:ietf:wg:oauth:2.0:oob")

(defun oauth2--do-warn (&rest msg)
  "Actual function to log MSG based on how `oauth2-debug' is set."
  (setcar msg (concat "[oauth2] " (car msg)))
  (apply (if (functionp oauth2-debug)
             oauth2-debug
           'message)
         msg))

(defun oauth2--do-trivia (&rest msg)
  "Output debug MSG when `oauth2-debug' is set to \\='trivia."
  (when (or (eq oauth2-debug 'trivia)
            (functionp oauth2-debug))
    (apply #'oauth2--do-warn msg)))

(defun oauth2--do-debug (&rest msg)
  "Output debug MSG when `oauth2-debug' is enabled."
  (when oauth2-debug
    (apply #'oauth2--do-warn msg)))

(defmacro oauth2--with-plstore (&rest body)
  "A macro that ensures the plstore is closed after running BODY."
  `(let ((plstore (plstore-open oauth2-token-file)))
     (unwind-protect
         (progn ,@body)
       (plstore-close plstore))))

(defun oauth2--current-timestamp ()
  "Get the current timestamp in seconds."
  (time-convert nil 'integer))

(defun oauth2--update-request-cache (host-name access-token request-timestamp
                                               &optional request-cache)
  "Update REQUEST-CACHE with HOST-NAME and ACCESS-TOKEN.
The REQUEST-CACHE has the following structure:

\\=((host-name-1 (:access-token access-token-1
               :request-timestamp request-timestamp-1))
 (host-name-2 (:access-token access-token-2
               :request-timestamp request-timestamp-2))
 ...)

The `expires-in' value is not stored here because experience says most
providers use the same expires-in value regardless of which host is
being requested.

Update REQUEST-CACHE with the given HOST-NAME, the new ACCESS-TOKEN, and
REQUEST-TIMESTAMP.  If REQUEST-CACHE is nil, create a new one.  If
HOST-NAME is nil, do nothing.

Returns the newly updated request-cache."
  (when host-name
    (let ((host-name (intern host-name)))
      (setq request-cache
          (plist-put request-cache host-name
                     `( :access-token ,access-token
                        :request-timestamp ,request-timestamp)))))
  request-cache)

(defun oauth2--get-from-request-cache (request-cache host-name slot)
  "Retrieve SLOT info from REQUEST-CACHE of HOST-NAME.
Returns nil if the slot is unavailable."
  (plist-get (plist-get request-cache (intern host-name)) slot))

(defun oauth2--update-plstore (plstore token)
  "Update the file storage with handle PLSTORE with the value in TOKEN."
  (plstore-put plstore (oauth2-token-plstore-id token)
               nil `(:request-cache
                     ,(oauth2-token-request-cache token)
                     :code-verifier
                     ,(oauth2-token-code-verifier token)
                     :access-response
                     ,(oauth2-token-access-response token)))
  (plstore-save plstore))

(defun oauth2--build-url-param-str (&rest data)
  "Build URL data string with values in DATA.
DATA should be a list of attribute name and value one by one, therefore
the length should be a multply of 2 or it will assert fail.  Each value
will be hexified to be URL-safe.  If a value is not a string or an empty
string, this pair of key value will be skipped.

Return a URL-safe string of parameter data."
  (unless (= (mod (length data) 2) 0)
    (error "Invalid parameters.  Must be attribute name value pairs.  Data: %s"
           data))
  (let (data-list)
    (while data
      (let ((key (pop data))
            (value (pop data)))
        (when (and (stringp value)
                   (not (string-empty-p value)))
          (push (concat key "=" (url-hexify-string value)) data-list))))
    (setq data-list (reverse data-list))
    (url-encode-url (string-join data-list "&"))))

(defun oauth2--build-url (address &rest data)
  "Build a URL string with ADDRESS and DATA.
DATA can be a string or an alist of attributes.  If it is a string, it
will be encoded; if it is an alist it will be converted to a URL-safe
string using oauth2--build-url-param-str.  It will then be combined with
address to build the full URL."
  (let ((data-str (if (> (length data) 1)
                      (apply #'oauth2--build-url-param-str
                             data)
                    (url-encode-url (car data)))))
    (concat address "?" data-str)))

(defun oauth2--generate-code-verifier (&optional verifier-length)
  "Generate a random string of VERIFIER-LENGTH long for code_challenge.
The string should be of length 43 to 128 (inclusive).  If
VERIFIER-LENGTH is nil, we default to 90 as mutt_oauth2.py did.  See
RFC7636 for more details."
  (let* ((valid-chars
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
         (verifier-length (or verifier-length 90))
         result-list)
    (dotimes (_ verifier-length)
      (let ((i (random (length valid-chars))))
        (push (substring valid-chars i (1+ i)) result-list)))
    (base64url-encode-string (string-join result-list))))

(defun oauth2--get-challenge-from-verifier (code-verifier)
  "Get the code_challenge from CODE-VERIFIER."
  ;; base64url-encode-string returns a string that ends with '=' so the last
  ;; character should be skipped.
  (substring (base64url-encode-string (secure-hash 'sha256
                                                   code-verifier
                                                   nil nil t))
             0 -1))

(defun oauth2-request-authorization (auth-url client-id &optional scope state
                                              redirect-uri user-name
                                              code-verifier)
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider which uses
REDIRECT-URI when requesting an access-token.  The default redirect_uri
for desktop application is usually \"urn:ietf:wg:oauth:2.0:oob\".  SCOPE
identifies the resources that your application can access on the user's
behalf.  STATE is a string that your application uses to maintain the
state between the request and redirect response. USER-NAME is used to
provide the login_hint which will fill the login user name on the
requesting webpage to save users some typing.  CODE-VERIFIER when
provided enables the PKCE extension and will generate and provide the
code_challenge using method S256 when requesting authorization.

Returns the code provided by the service."
  (let* ((func-name "oauth2-request-authorization")
         (url (let ((param `("client_id" ,client-id
                             "response_type" "code"
                             "redirect_uri"
                             ,(or redirect-uri oauth2--default-redirect-uri)
                             "scope" ,scope
                             "state" ,state
                             "login_hint" ,user-name
                             "access_type" "offline"
                             "prompt" "consent")))
                (when (and code-verifier
                           (not (string-empty-p code-verifier)))
                  (setq param (plist-put param "code_challenge"
                                         (oauth2--get-challenge-from-verifier
                                          code-verifier)))
                  (setq param (plist-put param
                                         "code_challenge_method" "S256")))
                (push auth-url param)
                (apply 'oauth2--build-url param))))
    (oauth2--do-trivia "[%s]: url: %s" func-name url)
    (browse-url url)
    (read-string (concat "Follow the instruction on your default browser, or "
                         "visit:\n" url
                         "\nEnter the code your browser displayed: "))))

(defun oauth2-request-access-parse ()
  "Parse the result of an OAuth request."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (json-read)))

(defun oauth2-make-access-request (url data)
  "Make an access request to URL using DATA in POST requests."
  (let ((func-name "oauth2-make-access-request"))
    (oauth2--do-trivia "[%s]: url: %s" func-name url)
    (oauth2--do-trivia "[%s]: data: %s" func-name data)
    (let ((url-request-method "POST")
          (url-request-data data)
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (with-current-buffer (url-retrieve-synchronously url)
        (let ((data (oauth2-request-access-parse)))
          (kill-buffer (current-buffer))
          (oauth2--do-trivia "[%s]: response: %s" func-name
                             (prin1-to-string data))
          data)))))

(cl-defstruct oauth2-token
  plstore
  plstore-id
  client-id
  client-secret
  access-token
  refresh-token
  request-cache
  code-verifier
  auth-url
  token-url
  access-response)

(defun oauth2-request-access (auth-url token-url client-id client-secret code
                                       &optional redirect-uri host-name
                                       code-verifier)
  "Request OAuth access at AUTH-URL.
TOKEN-URL is the URL for making the request.  CLIENT-ID and
CLIENT-SECRET are provided by the service provider.  The CODE should be
obtained with `oauth2-request-authorization'.  REDIRECT-URI is used when
requesting access-token.  The default value for desktop application is
usually \"urn:ietf:wg:oauth:2.0:oob\".  HOST-NAME is the server to
request access, e.g. IMAP or SMTP server address.  Its value should
match the one when calling `oauth2-auth-and-store'.  Leaving HOST-NAME
as nil effectively disables caching and will request a new token on each
request.  CODE-VERIFIER is used for the PKCE extension and is required
when it was already provided during authorization.

Returns an `oauth2-token'."
  (when code
    (let* ((request-timestamp (oauth2--current-timestamp))
           (access-response (oauth2-make-access-request
                             token-url
                             (oauth2--build-url-param-str
                              "client_id" client-id
                              "client_secret" client-secret
                              "code" code
                              "code_verifier" code-verifier
                              "redirect_uri" (or redirect-uri
                                                 oauth2--default-redirect-uri)
                              "grant_type" "authorization_code")))
           (access-token (cdr (assoc 'access_token access-response)))
           (refresh-token (cdr (assoc 'refresh_token access-response)))
           (request-cache (oauth2--update-request-cache host-name
                                                        access-token
                                                        request-timestamp)))
      (make-oauth2-token :client-id client-id
                         :client-secret client-secret
                         :access-token access-token
                         :refresh-token refresh-token
                         :request-cache request-cache
                         :code-verifier code-verifier
                         :auth-url auth-url
                         :token-url token-url
                         :access-response access-response))))

;;;###autoload
(defun oauth2-refresh-access (token &optional host-name)
  "Refresh OAuth access TOKEN.
TOKEN should be obtained with `oauth2-request-access'.  HOST-NAME is
optional but highly recommended which is required for the cache to work."
  (if-let* ((func-name "oauth2-refresh-access")
            (current-timestamp (oauth2--current-timestamp))
            (request-cache (oauth2-token-request-cache token))
            (request-timestamp (or (oauth2--get-from-request-cache
                                    request-cache host-name :request-timestamp)
                                   ;; host-name could be nil, so default to 0
                                   0))
            (timestamp-difference (- current-timestamp request-timestamp))
            (expires-in (cdr (assoc 'expires_in
                                    (oauth2-token-access-response token))))
            (cache-valid
             (progn
               (oauth2--do-trivia (concat "[%s]: current-timestamp: %d, "
                                          "previous request-timestamp: %d, "
                                          "timestamp difference: %d; "
                                          "expires-in: %d, ")
                                  func-name current-timestamp request-timestamp
                                  timestamp-difference expires-in)
               (< timestamp-difference expires-in))))
      (oauth2--do-debug "[%s]: reusing cached access-token." func-name)

    (oauth2--do-debug "[%s]: requesting new access-token." func-name)
    (let* ((client-id (oauth2-token-client-id token))
           (client-secret (oauth2-token-client-secret token))
           (refresh-token (oauth2-token-refresh-token token))
           (token-url (oauth2-token-token-url token))
           (url-param-str (oauth2--build-url-param-str
                           "client_id" client-id
                           "client_secret" client-secret
                           "refresh_token" refresh-token
                           "grant_type" "refresh_token"))
           (access-token (cdr (assoc 'access_token
                                     (oauth2-make-access-request
                                      token-url url-param-str))))
           (request-cache (oauth2-token-request-cache token)))
      (setf (oauth2-token-access-token token) access-token)
      (setf (oauth2-token-request-cache token)
            (oauth2--update-request-cache host-name access-token
                                          current-timestamp request-cache)))
    (oauth2--with-plstore
     (oauth2--update-plstore plstore token)))

  token)

;;;###autoload
(defun oauth2-auth (auth-url token-url client-id client-secret
                             &optional scope state redirect-uri user-name
                             host-name code-verifier)
  "Authenticate application via OAuth2 at AUTH-URL.
TOKEN-URL is the URL for making the request.  CLIENT-ID and
CLIENT-SECRET are provided by the service provider.  SCOPE identifies
the resources that your application can access on the user's behalf.
STATE is a string that your application uses to maintain the state
between the request and redirect response.  REDIRECT-URI is used when
requesting access-token.  The default value for desktop application is
usually \"urn:ietf:wg:oauth:2.0:oob\".  USER-NAME is the login user name
and is required to provide a unique plstore id for users on the same
service provider.  HOST-NAME is the server to request access, e.g. IMAP
or SMTP server address.  Its value should match the one when calling
`oauth2-auth-and-store'.  Leaving HOST-NAME as nil effectively disables
caching and will request a new token on each request.  CODE-VERIFIER is
used for the PKCE extension and is required when it was already provided
during authorization.

Returns an `oauth2-token'."
  (oauth2-request-access
   auth-url
   token-url
   client-id
   client-secret
   (oauth2-request-authorization auth-url client-id scope state redirect-uri
                                 user-name code-verifier)
   redirect-uri
   host-name
   code-verifier))

(defun oauth2-compute-id (auth-url token-url scope client-id user-name)
  "Compute an unique id mainly to use as plstore id.
The result is computed using AUTH-URL, TOKEN-URL, SCOPE, CLIENT-ID, and
USER-NAME to ensure the plstore id is unique."
  (secure-hash 'sha512 (concat auth-url token-url scope client-id user-name)))

;;;###autoload
(defun oauth2-auth-and-store (auth-url token-url scope client-id client-secret
                                       &optional redirect-uri state user-name
                                       host-name use-pkce)
  "Request access to a resource and store it.
AUTH-URL and TOKEN-URL are provided by the service provider.  CLIENT-ID
and CLIENT-SECRET should be generated by the service provider when a
user registers an application.  SCOPE identifies the resources that your
application can access on the user's behalf.  STATE is a string that
your application uses to maintain the state between the request and
redirect response.  USER-NAME is the login user name and is required to
provide a unique plstore id for users on the same service provider.
HOST-NAME is the server to request authentication, e.g. IMAP or SMTP
server address.  Leaving HOST-NAME as nil effectively disables caching
and will request a new token on each refresh.  USE-PKCE controls whether
to enable the PKCE extension of RFC7636 which is supported by most
OAuth2 providers and recommended.

Returns an `oauth2-token'."
  ;; We store a MD5 sum of all URL
  (oauth2--with-plstore
   (let* ((func-name "oauth2-auth-and-store")
          (plstore-id (oauth2-compute-id auth-url token-url scope client-id
                                         user-name))
          (plist (cdr (plstore-get plstore plstore-id))))
     (oauth2--do-trivia "[%s]: user-name: %s\nplstore-id: %s"
                        func-name user-name plstore-id)
     ;; Check if we found something matching this access and have a valid cache.
     (if-let* ((plist plist)
               (access-response (plist-get plist :access-response))
               (refresh-token (cdr (assoc 'refresh_token access-response)))
               (request-cache (plist-get plist :request-cache))
               (access-token (or (oauth2--get-from-request-cache
                                  request-cache host-name :access-token)
                                 ""))
               (code-verifier (plist-get plist :code-verifier)))
         (progn
           (oauth2--do-trivia "[%s]: found matching plstore-id from plstore."
                              func-name)
           (make-oauth2-token :plstore-id plstore-id
                              :client-id client-id
                              :client-secret client-secret
                              :access-token access-token
                              :refresh-token refresh-token
                              :request-cache request-cache
                              :code-verifier code-verifier
                              :auth-url auth-url
                              :token-url token-url
                              :access-response access-response))
       (oauth2--do-trivia
        (concat "[%s]: no matching plstore-id found or cache invalid.  "
                "Requesting new oauth2-token.")
        func-name)
       (let* ((code-verifier (if use-pkce
                                 (oauth2--generate-code-verifier)
                               ""))
              (token (oauth2-auth auth-url token-url
                                  client-id client-secret scope state
                                  redirect-uri user-name host-name
                                  code-verifier)))
         ;; Set the plstore
         (setf (oauth2-token-plstore-id token) plstore-id)
         (oauth2--update-plstore plstore token)
         token)))))

(provide 'oauth2)

;;; oauth2.el ends here
