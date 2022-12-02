;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-spell-check-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/spell-check-mode:spell-check-mode buffer))
      (assert-true (disable-modes* 'nyxt/spell-check-mode:spell-check-mode buffer)))))
