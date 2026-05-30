;;; setup-gptel.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In-Emacs LLM chat / refactor client, configured to use Anthropic's Claude.
;;
;; API key: gptel reads it from auth-source, so add a line to ~/.authinfo (or
;; the encrypted ~/.authinfo.gpg) -- do NOT put the key in this file:
;;
;;   machine api.anthropic.com login apikey password sk-ant-...
;;
;; Usage:
;;   M-x gptel          -> open a dedicated chat buffer
;;   M-x gptel-send     -> send the region/buffer up to point (C-c RET in chat)
;;   M-x gptel-menu     -> transient menu: pick model, scope, system prompt, etc.
;;   M-x gptel-rewrite  -> rewrite/refactor the active region in place

;;; Code:

(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-menu gptel-rewrite)
  :custom
  ;; Render chat buffers as Org.
  (gptel-default-mode 'org-mode)
  ;; A current, well-balanced Claude model; switch any time from `gptel-menu'.
  (gptel-model 'claude-sonnet-4-6)
  :config
  ;; `gptel-make-anthropic' lives in gptel-anthropic.el (normally autoloaded);
  ;; require it explicitly so this works regardless of autoload state.
  (require 'gptel-anthropic)
  ;; Use Claude as the default backend.  The key is pulled from auth-source for
  ;; host api.anthropic.com (see the commentary above), so no secret lives here.
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t)))

(provide 'setup-gptel)
;;; setup-gptel.el ends here
