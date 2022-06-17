(in-package #:calm)

(swank:create-server :port 4242)

(defparameter *fan-speed-level* 0)

;; (defparameter *fan-speed-level-list* '(0 4 8 12))
(defparameter *fan-speed-level-list* '(0 12 16 20))

(defun on-fan-switch-click ()
  (let ((i (position *fan-speed-level* *fan-speed-level-list*)))
    (when (>= i 3) (setf i -1))
    (setf *fan-speed-level* (nth (1+ i) *fan-speed-level-list*))

    (unless (= *fan-speed-level* 0)
      (when (u:mix-is-playing) (u:mix-halt))
      (u:mix-play (str:concat "fan-" (write-to-string *fan-speed-level*) ".wav") -1))))


(defparameter *fan-blade-angle-degree-default-list*
  '(0 90 180 270))

(defparameter *fan-blade-angle-degree-list*
  *fan-blade-angle-degree-default-list*)

(defun draw-blade (&optional (degree 0))
  (c:save)
  (c:translate 300 165)
  (c:rotate (* degree (/ pi 180)))
  (c:move-to 0 -15)
  (c:line-to 0 -100)
  (c:curve-to 0 -110 100 -65 15 0)
  (c:stroke-preserve)

  (c:set-source-rgba (/ 12 255) (/ 55 255) (/ 132 255) 0.1)
  (c:fill-path)
  (c:restore))

(defparameter *fan-mousebutton-is-up* nil)

(defun on-mousebuttonup (&key button x y clicks)
  (declare (ignore button x y clicks))
  (setf *fan-mousebutton-is-up* t))

(defun draw-switch ()
  (c:new-path)
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:set-line-width 3)
  (c:arc 550 50 15 0 (* 2 pi))
  (if (c:in-fill *calm-mouse-x* *calm-mouse-y*)
      (progn
        (c:stroke-preserve)
        (c:fill-path) (u:set-cursor :hand)
        (when *fan-mousebutton-is-up*
          (setf *fan-mousebutton-is-up* nil)
          (on-fan-switch-click)))
      (progn (c:stroke) (u:set-cursor :arrow)
)))

(defun draw? ()
  (when (> (apply #'max *fan-blade-angle-degree-list*) 360)
    (setf *fan-blade-angle-degree-list*
          *fan-blade-angle-degree-default-list*))
  (if (= *fan-speed-level* 0)
      (progn
        (unless (equal *fan-blade-angle-degree-list* *fan-blade-angle-degree-default-list*)
        (setf *fan-blade-angle-degree-list* (mapcar #'(lambda (x)  (+ x 3)) *fan-blade-angle-degree-list*)))
        (when (u:mix-is-playing) (u:mix-halt)))
      (progn
        (setf *fan-blade-angle-degree-list* (mapcar #'(lambda (x)  (+ x *fan-speed-level*)) *fan-blade-angle-degree-list*))))
  t)

(defun draw ()
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:new-path)
  (c:set-line-width 6)
  (c:set-line-cap :round)

  (c:arc 300 165 120 (* 1.25 pi) (* 3.2 pi))
  (c:stroke)

  ;; (format t "X,Y: ~A,~A~%" *calm-mouse-x* *calm-mouse-y*)

  ;; neck

  (c:move-to 270 285)
  (c:line-to 255 375)

  (c:move-to 330 285)
  (c:line-to 345 375)

  ;; base

  (c:move-to 220 415)
  (c:line-to 380 415)

  (c:move-to 300 375)
  (c:curve-to 370 370 400 415 380 415)

  (c:move-to 280 375)
  (c:curve-to 220 375 200 415 220 415)
  (c:stroke)

  ;; blade
  (mapcar 'draw-blade *fan-blade-angle-degree-list*)

  ;; turbo
  (c:arc 300 165 10 0 (* 2 pi))
  (c:stroke)

  ;; switch

  (draw-switch)
  )
