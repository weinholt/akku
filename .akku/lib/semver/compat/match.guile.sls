;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2020 Göran Weinholt
#!r6rs

;;; (chibi match)

(library (semver compat match)
  (export
    match match-lambda)
  (import
    (ice-9 match)))
