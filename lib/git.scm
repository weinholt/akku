;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later

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
#!r6rs

(library (akku lib git)
  (export
    is-git-repository?
    git-clone
    git-shallow-clone
    git-fetch
    git-fetch-tag
    git-checkout-commit
    git-checkout-branch
    git-checkout-tag
    git-ls-files
    git-remote-set-url
    git-rev-parse
    git-rev-list/first
    git-tag-list
    git-list-remotes
    git-remote-get-url)
  (import
    (rnrs (6))
    (only (srfi :13 strings) string-trim-right)
    (only (spells filesys) file-directory?)
    (only (akku private compat) open-process-ports putenv)
    (only (akku lib utils) string-split path-join run-command))

(define (get-command-output cmd)
  (let-values (((to-stdin from-stdout from-stderr _process-id)
                (open-process-ports cmd (buffer-mode block) (native-transcoder))))
    (close-port to-stdin)
    (let* ((output (get-string-all from-stdout))
           (errors (get-string-all from-stderr)))
      (unless (eof-object? errors)
        (display errors (current-error-port)))
      (close-port from-stdout)
      (close-port from-stderr)
      (if (eof-object? output)
          ""
          output))))

(define (is-git-repository? dir)
  (file-directory? (string-append dir "/.git")))

(define (git-clone directory repository)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_REPO" repository)
  (run-command "exec git clone \"$AKKU_REPO\" \"$AKKU_DIR\""))

(define (git-shallow-clone directory repository)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_REPO" repository)
  (run-command "exec git clone --single-branch --depth=1 \"$AKKU_REPO\" \"$AKKU_DIR\""))

(define (git-fetch directory)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (when (file-exists? (path-join directory ".git/shallow"))
    ;; Undo the cleverness of git-fetch-tag
    (run-command "cd \"$AKKU_DIR\" && exec git fetch -q --unshallow origin '+refs/heads/*:refs/remotes/origin/*'"))
  (run-command "cd \"$AKKU_DIR\" && exec git fetch -q origin"))

(define (git-fetch-tag directory tag)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_TAG" tag)
  (run-command "cd \"$AKKU_DIR\" && exec git fetch --depth=1 -q --tags origin \"refs/tags/$AKKU_TAG\""))

(define (git-checkout-commit directory commit)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_COMMIT" commit)
  (run-command "cd \"$AKKU_DIR\" && exec git checkout --detach -q \"$AKKU_COMMIT\""))

(define (git-checkout-branch directory branch)
  (git-checkout-commit directory branch))

(define (git-checkout-tag directory tag)
  (git-checkout-commit directory tag))

(define (git-ls-files directory)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (let ((output (get-command-output "cd \"$AKKU_DIR\" && exec git ls-files -z")))
    (filter (lambda (x) (> (string-length x) 0))
            (string-split output #\nul))))

(define (git-remote-set-url dir name newurl)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" dir)
  (putenv "AKKU_NAME" name)
  (putenv "AKKU_NEWURL" newurl)
  (run-command "cd \"$AKKU_DIR\" && exec git remote set-url \"$AKKU_NAME\" \"$AKKU_NEWURL\""))

(define (git-rev-parse directory rev)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_REV" rev)
  (let ((output (get-command-output "cd \"$AKKU_DIR\" && exec git rev-parse \"$AKKU_REV\"")))
    (car (string-split output #\newline))))

(define (git-rev-list/first directory rev)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_REV" rev)
  (let ((output (get-command-output "cd \"$AKKU_DIR\" && exec git rev-list -n1 \"$AKKU_REV\"")))
    (car (string-split output #\newline))))

(define (git-tag-list directory pattern)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_PATTERN" pattern)
  (let ((output (get-command-output "cd \"$AKKU_DIR\" && exec git tag -l \"$AKKU_PATTERN\"")))
    (string-split output #\newline)))

(define (git-list-remotes directory)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (let ((output (get-command-output "cd \"$AKKU_DIR\" && exec git remote")))
    (string-split (string-trim-right output #\newline) #\newline)))

(define (git-remote-get-url directory remote)
  (putenv "GIT_DIR" ".git")
  (putenv "AKKU_DIR" directory)
  (putenv "AKKU_REMOTE" remote)
  (let ((output (get-command-output "cd \"$AKKU_DIR\" && exec git remote get-url \"$AKKU_REMOTE\"")))
    (car (string-split output #\newline)))))
