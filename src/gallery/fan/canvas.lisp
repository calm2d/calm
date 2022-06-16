(in-package #:calm)

(swank:create-server :port 4242)

(defun draw ()
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:new-path)
  (c:set-line-width 6)
  (c:set-line-cap :round)

  (c:arc 300 165 120 (* 1.25 pi) (* 3.2 pi))
  (c:stroke)
  
  (format t "X,Y: ~A,~A~%" *calm-mouse-x* *calm-mouse-y*)

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
  (c:stroke))
