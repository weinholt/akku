#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later
#!r6rs

;;; Linux build script for Akku.scm

(import
  (chezscheme)
  (only (srfi :1 lists) delete-duplicates)
  (only (akku lib utils) string-split path-join mkdir/recursive
        application-data-directory)
  (semver versions))

(define (which filename)                ;same as which(1)
  (let lp ((dirs (string-split (or (getenv "PATH") "") #\:)))
    (if (null? dirs)
        #f
        (let ((fn (path-join (car dirs) filename)))
          (if (and (file-exists? fn)
                   (> (bitwise-and (get-mode fn) #o111) 0))
              fn
              (lp (cdr dirs)))))))

(define (cp src dst)
  (putenv "SRC" src)
  (putenv "DST" dst)
  (system "set -x;/bin/cp -H \"$SRC\" \"$DST\""))

(define (mv src dst)
  (putenv "SRC" src)
  (putenv "DST" dst)
  (system "set -x;/bin/mv \"$SRC\" \"$DST\""))

(define (ln/s src dst)
  (putenv "SRC" src)
  (putenv "DST" dst)
  (system "set -x;/bin/ln -s \"$SRC\" \"$DST\""))

(define (get-version manifest)
  (call-with-input-file manifest
    (lambda (p) (read p) (cadadr (read p)))))

(define (copy-chez-notice target)
  ;; Apache License 2.0 requires LICENSE and NOTICE to be included in
  ;; redistributions.
  (cond ((and (file-exists? "/usr/share/doc/chezscheme/copyright")
              (file-exists? "/usr/share/doc/chezscheme/changelog.Debian.gz"))
         (cp "/usr/share/doc/chezscheme/copyright" target))
        ((let ((dir (or (getenv "CHEZSOURCEDIR")
                        (begin
                          (display "Please enter the location of the Chez Scheme source: ")
                          (get-line (current-input-port))))))
           (and (string? dir) (file-directory? dir) dir))
         =>
         (lambda (srcdir)
           (call-with-output-file target
             (lambda (outp)
               (call-with-input-file (path-join srcdir "NOTICE")
                 (lambda (np) (put-string outp (get-string-all np))))
               (call-with-input-file (path-join srcdir "LICENSE")
                 (lambda (np) (put-string outp (get-string-all np))))))))
        (else
         ;; Not exactly the same files, but close enough.
         (format #t "Warning: downloading Chez Scheme LICENSE and NOTICE from github.~%")
         (format #t "Press enter to continue.~%")
         (get-line (current-input-port))
         (putenv "LICENSE_URL" "https://raw.githubusercontent.com/cisco/ChezScheme/master/LICENSE")
         (putenv "NOTICE_URL" "https://raw.githubusercontent.com/cisco/ChezScheme/master/NOTICE")
         (putenv "TARGET_FILE" target)
         (system "curl \"$NOTICE_URL\" \"$LICENSE_URL\" > \"$TARGET_FILE\"")))
  (assert (file-exists? target)))

(define (copy-notices target source)
  (call-with-output-file target
    (lambda (outp)
      (let-values (((to-stdin from-stdout from-stderr _process-id)
                    (open-process-ports
                     (string-append "bin/akku license-scan --implementation=guile,chezscheme " source)
                     (buffer-mode block)
                     (native-transcoder))))
        (when (port-eof? from-stdout)
          (error 'copy-notices "Blank output from license-scan" source
                 (get-string-all from-stderr)))
        (put-string outp (get-string-all from-stdout))
        (display (get-string-all from-stderr))
        (close-port to-stdin)))))

(define (compile-akku)
  ;; Compile the akku binary.
  (let-values (((from-stdout to-stdin _) (apply values (process "scheme -q"))))
    ;; Compilation is done in a subprocess, because already loaded
    ;; libraries are not compiled again, which means no wpo files.
    (delete-file "bin/akku")
    (write '(parameterize ((compile-imported-libraries #t)
                           (generate-wpo-files #t)
                           (optimize-level 2))
              (compile-program "bin/akku.sps" "bin/akku.so")
              (let ((remaining (compile-whole-program "bin/akku.wpo" "bin/akku" #t)))
                (unless (null? remaining)
                  (when (file-exists? "bin/akku")
                    (delete-file "bin/akku"))
                  (error 'build "Some libraries were not compiled" remaining))))
           to-stdin)
    (close-port to-stdin)
    (display (get-string-all from-stdout))
    (close-port from-stdout)
    (chmod "bin/akku" #o755)
    (format #t "built bin/akku~%")))

(define (copy-common-files)
  (mkdir/recursive "dist")
  (cp "COPYING" "dist/COPYING")
  (cp "README.md" "dist/")
  (cp "CONTRIBUTING.md" "dist/")
  (mkdir/recursive "dist/share/man/man1")
  (cp "docs/akku.1" "dist/share/man/man1")
  ;; Bootstrap data
  (mkdir/recursive "dist/share/akku")
  (cp (path-join (application-data-directory) "index.db")
      "dist/share/akku/bootstrap.db")
  (mkdir/recursive "dist/share/akku/keys.d")
  (cp "akku-archive-2018.gpg" "dist/share/akku/keys.d/akku-archive-2018.gpg"))

(define (build-binary-distribution)
  ;; Build the distribution.
  (system "/bin/rm -rf dist")
  (let ((chez-version (car (reverse (string-split (scheme-version) #\space))))
        (machine (symbol->string (machine-type)))
        (long-machine-type
         (case (machine-type)
           ((a6le ta6le) "amd64-linux")
           ((i3le ti3le) "i386-linux")
           ((arm32le tarm32le) "arm-linux")
           (else
            (symbol->string (machine-type)))))
        (multiarch-name
         ;; https://wiki.debian.org/Multiarch/Tuples
         (case (machine-type)
           ((a6le ta6le) "x86_64-linux-gnu")
           ((i3le ti3le) "i386-linux-gnu")
           ((arm32le tarm32le) "arm-linux-gnueabihf")
           (else
            (error 'build-binary-distribution
                   "Unknown machine type" (machine-type)))))
        (akku-version (get-version "Akku.manifest")))
    ;; Licenses, copyrights, etc
    (mkdir/recursive "dist/notices")
    (copy-chez-notice "dist/notices/ChezScheme.txt")
    (guard (exn
            ((and (who-condition? exn)
                  (eq? (condition-who exn) 'copy-notices))
             (display "WARNING: Could not gather copyright notices\n"
                      (current-error-port))))
      (copy-notices "dist/notices/copyright.txt" "bin/akku.sps"))
    ;; Chez doesn't provide an easy way to find the default boot files...
    (let ((default-heap-path (format #f "lib/csv~d/~d" chez-version machine))
          (petite (which "petite")))
      (unless petite
        (error 'build "Could not find petite in PATH"))
      (let* ((heap-dir (path-join (path-parent (path-parent petite)) default-heap-path))
             (petite-boot (path-join heap-dir "petite.boot")))
        (unless (file-exists? petite-boot)
          (error 'build "Could not find petite.boot"))
        (copy-common-files)
        (format #t "bundling with ~d and ~d~%" petite petite-boot)
        (mkdir/recursive (format #f "dist/lib/~d/akku" multiarch-name))
        ;; Petite boot file
        (let ((dist-petite-boot (format #f "dist/lib/~d/akku/petite.boot" multiarch-name)))
          (cp petite-boot dist-petite-boot)
          (chmod dist-petite-boot #o644))
        (ln/s "petite.boot" (format #f "dist/lib/~d/akku/scheme-script.boot" multiarch-name))
        ;; Petite binary
        (let ((dist-petite (format #f "dist/lib/~d/akku/petite" multiarch-name)))
          (cp petite dist-petite)
          (chmod dist-petite #o755))
        (ln/s "petite" (format #f "dist/lib/~d/akku/scheme-script" multiarch-name))
        (cp "bin/akku" (format #f "dist/lib/~d/akku/akku" multiarch-name))
        ;; Shell wrapper script
        (let ((wrapper (format #f "dist/lib/~d/akku/akku.sh" multiarch-name)))
          (call-with-output-file wrapper
            (lambda (p)
              (format p "#!/bin/sh~%")
              (format p "LIBARCHDIR=~~/.local/lib/~d/akku~%" multiarch-name)
              (format p "exec \"$LIBARCHDIR/petite\" -b \"$LIBARCHDIR/petite.boot\" --program \"$LIBARCHDIR/akku\" \"$@\"~%")))
          (chmod wrapper #o755))
        ;; Install script
        (call-with-output-file "dist/install.sh"
          (lambda (p)
            (format p "#!/bin/sh~%")
            (format p "set -eu~%")
            (format p "datadir=${XDG_DATA_HOME:-~~/.local/share}~%")
            (format p "mkdir -p ~~/.local/bin \"$datadir\"~%")
            (format p "cp -a lib ~~/.local/~%")
            (format p "cp -a share/* \"$datadir\"~%")
            (format p "rm -f ~~/.local/bin/akku~%")
            (format p "ln -s ../lib/~d/akku/akku.sh ~~/.local/bin/akku~%" multiarch-name)
            (format p "echo You can now run '~~/.local/bin/akku'~%")
            (format p "[ -d ~~/.akku ] && echo To remove the legacy version: rm -rf '~~/.akku' '~~/bin/akku'~%")
            (format p "exit 0~%")))
        (chmod "dist/install.sh" #o755)
        ;; Build a tarball.
        (let* ((build-version (format #f "~d.~d" akku-version long-machine-type))
               (tarfile (format #f "akku-~d.tar.xz" build-version)))
          (assert (string->semver akku-version))
          (putenv "FILENAME" tarfile)
          (putenv "DISTVER" build-version)
          (system "tar --numeric-owner --owner 0 --group 0 -caf \"$FILENAME\" --transform s,^dist,akku-$DISTVER, dist/")
          (format #t "built ~d~%" tarfile))))))

(define (dependency-scan source impl*)
  (define (scan1 impl)
    (let-values (((to-stdin from-stdout from-stderr _process-id)
                  (open-process-ports
                   (string-append "bin/akku dependency-scan --implementation=" impl " " source)
                   (buffer-mode block)
                   (native-transcoder))))
      (when (port-eof? from-stdout)
        (error 'copy-notices "Blank output from dependency-scan" source
               (get-string-all from-stderr)))
      (close-port to-stdin)
      (let lp ((line* '()))
        (cond ((port-eof? from-stdout)
               (unless (port-eof? from-stderr)
                 (display (get-string-all from-stderr)))
               (close-port from-stderr)
               (close-port from-stdout)
               (reverse line*))
              (else
               (lp (cons (get-line from-stdout) line*)))))))
  (delete-duplicates
   (list-sort string-ci<=? (apply append (map scan1 impl*)))))

(define (build-source-distribution)
  (let ((akku-version (get-version "Akku.manifest")))
    (system "/bin/rm -rf dist")
    (copy-common-files)
    (for-each (lambda (fn)
                (let* ((dest (path-join "dist/lib/akku" (path-rest fn)))
                       (dir (path-parent dest)))
                  (mkdir/recursive dir)
                  (cp fn dest)))
              (dependency-scan ".akku/bin/akku.sps" '("chezscheme" "guile")))
    (mkdir/recursive "dist/lib/akku")
    ;; Install script
    (cp "private/install-src.sh" "dist/install.sh")
    (chmod "dist/install.sh" #o755)
    (let* ((build-version (format #f "~d.src" akku-version))
           (tarfile (format #f "akku-~d.tar.xz" build-version)))
      (assert (string->semver akku-version))
      (putenv "FILENAME" tarfile)
      (putenv "DISTVER" build-version)
      (system "tar --numeric-owner --owner 0 --group 0 -caf \"$FILENAME\" --transform s,^dist,akku-$DISTVER, dist/")
      (format #t "built ~d~%" tarfile))))

(assert (not (petite?)))
(system "/bin/rm -rf dist")
(system "akku.sps install")
(compile-akku)
(build-binary-distribution)
(build-source-distribution)
(system "/bin/rm -rf dist")
