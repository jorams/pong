(in-package :pong)

(defparameter +fps+ 60)
(defparameter *closedp* nil)

(defclass window (glop:window) ())

(defmethod glop:on-key ((window window) pressed keycode keysym string))

(defmethod glop:on-button ((window window) pressed button))

(defmethod glop:on-mouse-motion ((window window) x y dx dy)
  (with-accessors ((speed y-speed)
                   (old-y y))
      (player *field*)
    (let ((scale (/ (height *field*)
                    (glop:window-height window))))
      (setf speed (- (* scale y) old-y)))))

(defmethod glop:on-resize ((window window) w h)
  (gl:viewport 0 0 w h))

;; TODO: Use modern OpenGL
(defun render (window)
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer-bit)
  (labels ((vertex (pos reversep)
             ;; We're scaling the width of the field by 4 because we only show
             ;; half of it.
             (let ((width-scale (/ 4 (width *field*)))
                   (height-scale (/ 2 (height *field*))))
               (if reversep
                   (gl:vertex (- 1 (* width-scale (x pos)))
                              (- 1 (* height-scale (y pos))))
                   (gl:vertex (+ -1 (* width-scale (x pos)))
                              (- 1 (* height-scale (y pos)))))))
           (rectangle (object &optional reversep)
             (vertex (top-left object) reversep)
             (vertex (bottom-left object) reversep)
             (vertex (bottom-right object) reversep)
             (vertex (top-right object) reversep)))
    (gl:with-primitive :quads
      (gl:color 1 1 1)
      (rectangle (player *field*) (eq (side *field*) :right))
      (gl:color 1 1 1)
      (rectangle (ball *field*) (eq (side *field*) :right))))
  (gl:flush)
  (glop:swap-buffers window))

(defmethod glop:on-draw ((window window))
  (render window))

(defmethod glop:on-close ((window window))
  (setf *closedp* t))

(defun game-loop (window)
  (loop with *closedp* = nil
        until *closedp*
        for next-time = (+ (get-internal-real-time)
                           (/ internal-time-units-per-second +fps+))
        unless *playingp* do
          (start-playing)
        do (glop:dispatch-events window)
           (tick)
           (render window)
           (sleep (max 0 (/ (- next-time
                               (get-internal-real-time))
                            internal-time-units-per-second)))))

(defun game (side host port fullscreen)
  (glop:with-window (window "Pong" 800 450
                            :win-class 'window
                            :fullscreen fullscreen)
    (with-field (side)
      (ecase side
        (:left (let ((socket (socket-listen host port
                                            :reuse-address t)))
                 (unwind-protect
                      (progn
                        (setf (connection *field*)
                              (socket-accept socket
                                             :element-type 'nibbles:octet))
                        (start-playing)
                        (send-state (ball *field*)
                                    (socket-stream (connection *field*)))
                        (game-loop window))
                   (socket-close socket))))
        (:right (let ((socket (socket-connect host port
                                              :element-type 'nibbles:octet)))
                  (unwind-protect
                       (progn
                         (setf (connection *field*) socket)
                         (start-playing)
                         (receive-state (ball *field*) (socket-stream socket))
                         (game-loop window))
                    (socket-close socket))))))))

(defun start (side &key (host "0.0.0.0") (port 4320) (fullscreen t))
  (handler-case
      (game side host port fullscreen)
    (serious-condition ())))
