;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-macro-edit-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/macro-edit-mode:macro-edit-mode buffer))
      (assert-true (disable-modes* 'nyxt/macro-edit-mode:macro-edit-mode buffer)))))
