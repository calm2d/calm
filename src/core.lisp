(format t "LOADING CALM CORE ... ~%")

(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package #:calm)

(defparameter *calm-drawing* t)

(defparameter *calm-debuging* nil)
(defparameter *calm-frame-delay* 15)
(defparameter *calm-width* 600)
(defparameter *calm-height* 500)
(defparameter *calm-title* "CALM")
(defparameter *calm-window-flags* '(:shown :allow-highdpi))

(defparameter *calm-status-mouse-inside-window* nil)

(defparameter *calm-png-file* nil)
(defparameter *calm-svg-file* nil)
(defparameter *calm-pdf-file* nil)

(defparameter *calm-window* nil)
(defparameter *calm-renderer* nil)
(defparameter *calm-renderer-width* 800)
(defparameter *calm-renderer-height* 600)
(defparameter *calm-dpi-scale* 1)

(defparameter *calm-mouse-x* 0)
(defparameter *calm-mouse-y* 0)

(defun draw? () nil)
(defun draw ())
(defun on-textinput (text) (declare (ignore text)))
(defun on-keydown (key) (declare (ignore key)))
(defun on-keyup (key) (declare (ignore key)))
(defun on-mousewheel (x y direction) (declare (ignore x y direction)))
(defun on-mousebuttonup (&key button x y clicks) (declare (ignore button x y clicks)))
(defun on-mousebuttondown (&key button x y clicks) (declare (ignore button x y clicks)))

(defmacro with-canvas ((&key (title '*calm-title*)
                          (x :centered) (y :centered)
                          (w '*calm-width*) (h '*calm-height*)
                          (flags '*calm-window-flags*))
                       &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (*calm-window* :title ,title :x ,x :y ,y :w ,w :h ,h :flags ,flags)
       (sdl2:with-renderer (*calm-renderer* *calm-window*)
         (sdl2:with-event-loop (:method :poll)
           (:quit () (uiop:quit))
           (:mousewheel (:x x :y y :direction direction)
                        (on-mousewheel x y direction)
                       (redraw))
           (:textinput (:text text)
                       (on-textinput text)
                       (redraw))
           (:textediting (:text text :start start :length length)
                       (describe text)
                       (format t "~%******TEXT-EDITING: ~A ~A ~A~%" text start length)
                       (redraw))
           (:keydown (:keysym k :state s)
                     (format t "KEY-DOWN, Scancode: ~A, STATE: ~A~%" (sdl2:scancode k) s)
                     (on-keydown (sdl2:scancode k))
                     (redraw))
           (:keyup (:keysym k :state s)
                   (format t "KEY-UP, Scancode: ~A, STATE: ~A~%" (sdl2:scancode k) s)
                   (on-keyup (sdl2:scancode k))
                   (redraw))
           (:windowevent (:event e)
                         ;; (format t "Window EVENT: ~A ~%" e)
                         (cond ((equal e sdl2-ffi:+sdl-windowevent-size-changed+) (redraw))
                               ((equal e sdl2-ffi:+sdl-windowevent-enter+)
                                (setf *calm-status-mouse-inside-window* t) (redraw))
                               ((equal e sdl2-ffi:+sdl-windowevent-leave+)
                                (setf *calm-status-mouse-inside-window* nil) (redraw))))
           (:mousemotion (:x x :y y)
                         ;; (format t "Mouse Motion EVENT: X:~A Y:~A ~%" x y)
                         (setf *calm-mouse-x* x
                               *calm-mouse-y* y)
                         ;; (redraw)
                         )
           (:mousebuttonup (:button button :x x :y y :clicks clicks)
                           ;; (format t "Mouse Button EVENT: ~A ~%" button)
                           (on-mousebuttonup :button button :x x :y y :clicks clicks)
                           (redraw))
           (:mousebuttondown (:button button :x x :y y :clicks clicks)
                           ;; (format t "Mouse Button EVENT: ~A ~%" button)
                           (on-mousebuttondown :button button :x x :y y :clicks clicks)
                           (redraw))
           (:idle ()
                  (when (or *calm-drawing* (draw?))
                    (when *calm-debuging*
                      (format t "Canvas Redraw, SDL Tick: ... ~A.~%" (sdl2:get-ticks)))
                    (setf *calm-drawing* nil)
                    (multiple-value-bind (*calm-renderer-width* *calm-renderer-height*)
                        (sdl2:get-renderer-output-size *calm-renderer*)
                      (setf *calm-dpi-scale* (/ *calm-renderer-width* ,w))
                      (sdl2:render-clear *calm-renderer*)
                        (with-texture (texture)
                          (with-cairo (texture)
                            ,@body))
                        (sdl2:render-present *calm-renderer*)))
                  (when *calm-frame-delay*
                    (sdl2:delay *calm-frame-delay*))
                  ))))))

(defmacro with-texture ((texture-sym) &body body)
  `(let ((,texture-sym (sdl2:create-texture
                        *calm-renderer*
                       sdl2:+pixelformat-argb8888+
                       sdl2-ffi:+sdl-textureaccess-streaming+
                       *calm-renderer-width*
                       *calm-renderer-height*)))
     (unwind-protect
          (progn
            (sdl2:set-texture-blend-mode ,texture-sym sdl2-ffi:+sdl-blendmode-blend+)
            ,@body)
       (sdl2:destroy-texture ,texture-sym))))

(defmacro with-cairo ((texture) &body body)
  `(let* ((pixels-and-pitch (multiple-value-list (sdl2:lock-texture ,texture)))
          (cr-surface
            (cl-cairo2:create-image-surface-for-data
             (car pixels-and-pitch)
             :argb32
             *calm-renderer-width*
             *calm-renderer-height*
             (cadr pixels-and-pitch)))
          (cr-context (cl-cairo2:create-context cr-surface)))
     (unwind-protect
          (progn
            (cl-cairo2:with-context (cr-context)
              ;; set current context
              (setf cl-cairo2:*context* cr-context)
              ;; set current surface
              (setf cl-cairo2:*surface* cr-surface)

              (cl-cairo2:scale *calm-dpi-scale* *calm-dpi-scale*)
              (cl-cairo2:set-antialias :BEST)
              (cl-cairo2:font-options-set-antialias (cl-cairo2:get-font-options) :CAIRO_ANTIALIAS_BEST)
              ;; default background color
              (cl-cairo2:set-source-rgb 1 1 1)
              (cl-cairo2:paint)
              ;; default font size
              (cl-cairo2:set-font-size 80)
              ;; default color
              (cl-cairo2:set-source-rgb 0 0 0)
              ;; default position
              (cl-cairo2:move-to 200 150)
              ,@body
              ))
       (sdl2:unlock-texture texture)
       (sdl2:render-copy
        *calm-renderer*
        texture)

       (when *calm-png-file*
         (format t "Saving to PNG: ~A~%" *calm-png-file*)
         (let* ((png-surface (c:create-image-surface :argb32 *calm-renderer-width* *calm-renderer-height*))
                (png-cr (c:create-context png-surface))
                (src-surface c:*surface*))
           (c:set-source-surface src-surface 0 0 png-cr)
           (c:paint png-cr)
           (c:surface-flush png-surface)
           (c:surface-write-to-png png-surface *calm-png-file*)

           (c:destroy png-surface)
           (c:destroy png-cr)))

       (when *calm-svg-file*
         (format t "Saving to SVG: ~A~%" *calm-svg-file*)
         (let* ((svg-cr (c:create-svg-context *calm-svg-file* *calm-renderer-width* *calm-renderer-height*))
                (src-surface c:*surface*))
           (c:set-source-surface src-surface 0 0 svg-cr)
           (c:paint svg-cr)
           (c:destroy svg-cr)))

       (when *calm-pdf-file*
         (format t "Saving to PDF: ~A~%" *calm-pdf-file*)
         (let* ((pdf-cr (c:create-pdf-context *calm-pdf-file* *calm-renderer-width* *calm-renderer-height*))
                (src-surface c:*surface*))
           (c:set-source-surface src-surface 0 0 pdf-cr)
           (c:paint pdf-cr)
           (c:destroy pdf-cr)))

       (cl-cairo2:destroy cr-surface)
       (cl-cairo2:destroy cr-context))))

(defun redraw ()
  (setf *calm-drawing* t))

(defun load-default-canvas ()
  (format t "DEFAULT CANVAS LOADING ~%")

  ;; (swank:create-server :port 4242)

  (defparameter *calm-width* 800)
  (defparameter *calm-height* 300)

  (defun draw ()
    (c:set-source-rgb 1 1 1)
    (c:paint)
    (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
    (c:move-to 20 180)
    (c:set-font-size 120)
    (c:show-text "DON'T PANIC")
    (c:move-to 320 250)
    (c:set-font-size 18)
    (c:show-text "READ THE MANUAL"))

  (when (uiop:getenv "CI")
    (format t "Running in CI Environment, quit without showing GUI.~%")
    (uiop:quit)))

(defun load-canvas ()
  (let ((user-dir (uiop:getenv "USER_DIR")))
    (format t "USER_DIR: ~A ... ~%" (uiop:getenv "USER_DIR"))
    (when user-dir (uiop:chdir user-dir))
    (let ((canvas-file
            (or (probe-file (merge-pathnames "canvas.lisp" (or user-dir "")))
                (probe-file (merge-pathnames "canvas.lisp" (uiop:getcwd)))
                (probe-file (merge-pathnames "canvas.lisp" sb-ext:*core-pathname*)))))
      (format t "CUR_DIR: ~A ... ~%" (uiop:getcwd))
      (format t "CANVAS_FILE: ~A ... ~%" canvas-file)
      ;; load canvas
      (if canvas-file
          (load canvas-file)
          (load-default-canvas))))
  ;; draw
  (with-canvas ()
    (draw)))

;; ==========================
;; utils
;; ==========================
(in-package #:calm-utils)

(defun set-cursor (type)
  (cond
    ((equal type :hand) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+)))
    ((equal type :arrow) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+)))))

(sdl2-mixer:init)
(defun mix-play (pathname &optional (loops 0))
  (if (string= (str:downcase (pathname-type pathname)) "wav")
      (progn (sdl2-mixer:open-audio sdl2-ffi:+mix-default-frequency+ sdl2-ffi:+mix-default-format+ 2 4096)
             (sdl2-mixer:play-channel -1 (sdl2-mixer:load-wav pathname) loops))
      (format t "Only WAV supported, try sdl2-mixer.")))

(defun mix-halt ()
  (sdl2-mixer:halt-channel -1))

(defun mix-is-playing ()
  (not (= 0 (sdl2-mixer:playing -1))))


#+linux (calm::load-canvas)
#+(or win32 darwin) (sdl2:make-this-thread-main #'calm::load-canvas)
