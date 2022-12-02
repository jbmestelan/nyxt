;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-expedition-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/expedition-mode:expedition-mode buffer))
      (assert-true (disable-modes* 'nyxt/expedition-mode:expedition-mode buffer)))))
