(in-package :pong)

(defvar *field*)
(defvar *playingp* nil)

(defparameter +field-width+ 3840)
(defparameter +field-height+ 1080)
(defparameter +paddle-width+ 30)
(defparameter +paddle-height+ 200)
(defparameter +paddle-x+ 50)
(defparameter +ball-width+ 30)
(defparameter +ball-height+ 30)

(defmacro define-simple-class (name superclasses slots &rest options)
  `(defclass ,name ,superclasses
     ,(loop for slot/pair in slots
            for (slot initform accessor-type)
              = (if (consp slot/pair) slot/pair (list slot/pair))
            collect `(,slot :initarg ,(intern (string slot)
                                              :keyword)
                            ,@(when initform (list :initform initform))
                            ,(or accessor-type :accessor) ,slot))
     ,@options))

(define-simple-class object ()
  (location
   width
   height
   (movement-speed (cons 0 0))))

;;; Easier location accessors

(defgeneric x (object)
  (:method ((object object))
    (x (location object)))
  (:method ((object cons))
    (car object)))

(defgeneric (setf x) (value object)
  (:method (value (object object))
    (setf (x (location object)) value))
  (:method (value (object cons))
    (setf (car object) value)))

(defgeneric y (object)
  (:method ((object object))
    (y (location object)))
  (:method ((object cons))
    (cdr object)))

(defgeneric (setf y) (value object)
  (:method (value (object object))
    (setf (y (location object)) value))
  (:method (value (object cons))
    (setf (cdr object) value)))

(defgeneric x-speed (object)
  (:method ((object object))
    (x-speed (movement-speed object)))
  (:method ((object cons))
    (car object)))

(defgeneric (setf x-speed) (value object)
  (:method (value (object object))
    (setf (x-speed (movement-speed object)) value))
  (:method (value (object cons))
    (setf (car object) value)))

(defgeneric y-speed (object)
  (:method ((object object))
    (y-speed (movement-speed object)))
  (:method ((object cons))
    (cdr object)))

(defgeneric (setf y-speed) (value object)
  (:method (value (object object))
    (setf (y-speed (movement-speed object)) value))
  (:method (value (object cons))
    (setf (cdr object) value)))

(defmacro define-edge-accessor (name operator location-accessor size-accessor)
  `(defgeneric ,name (object)
     (:method ((object object))
       (,operator (,location-accessor object)
                  (/ (,size-accessor object) 2)))))

(define-edge-accessor top - y height)
(define-edge-accessor bottom + y height)
(define-edge-accessor left - x width)
(define-edge-accessor right + x width)

(defmacro define-corner-accessor (name y-operator x-operator)
  `(defgeneric ,name (object)
     (:method ((object object))
       (cons (,x-operator object)
             (,y-operator object))))  )

(define-corner-accessor top-left top left)
(define-corner-accessor top-right top right)
(define-corner-accessor bottom-left bottom left)
(define-corner-accessor bottom-right bottom right)

(define-simple-class paddle (object)
  ((averaged-y-speed 0))
  (:default-initargs :width +paddle-width+
                     :height +paddle-height+
                     :location (cons +paddle-x+ 0)))

(defclass ball (object) ()
  (:default-initargs :width +ball-width+
                     :height +ball-height+))

(define-simple-class field ()
  ((width +field-width+)
   (height +field-height+)
   (side :left :reader)
   (player (make-instance 'paddle) :reader)
   (ball (make-instance 'ball) :reader)))

(defun start-playing (&optional (field *field*))
  (setf (location (ball field)) (cons (/ (width field) 2) (/ (height field) 2))
        (movement-speed (ball field)) (cons -10 0)
        *playingp* t))

(defun move-paddle (&optional (field *field*))
  (with-accessors ((y y) (y-speed y-speed)
                   (average averaged-y-speed))
      (player field)
    (incf y y-speed)
    (setf average (/ (+ y-speed average) 2))
    (setf y-speed 0)))

(defun move-ball (&optional (field *field*))
  (let+ (((&accessors (x-speed x-speed)
                      (y-speed y-speed)
                      (x x) (y y)
                      (height height)
                      (top top) (bottom bottom)
                      (left left) (right right))
          (ball field))
         (player (player field))
         (min-y (/ height 2))
         (max-y (- (height field) (/ height 2))))
    ;; Bounce at the top and bottom
    (setf y (max min-y
                 (min max-y
                      (+ y y-speed)))
          x (+ x x-speed))
    (when (or (= top 0)
              (= bottom (height field)))
      (setf y-speed (- y-speed)))
    (cond
      ;; Bounce on the paddle
      ((<= left (right player))
       (cond
         ((or (<= (top player) bottom (bottom player))
              (<= (top player) top (bottom player)))
          (setf x-speed (1+ (- x-speed)))
          (incf y-speed (/ (averaged-y-speed player)
                           (if (< x (x player))
                               3
                               -3))))
         (t (setf *playingp* nil))))
      ;; Bounce on the other side
      ((>= right (width field))
       (setf x-speed (- x-speed))))))

(defun tick (&optional (field *field*))
  (let ((*field* field))
    (move-paddle field)
    (move-ball field)))

(defmacro with-field ((&optional (side :left)) &body body)
  `(let ((*field* (make-instance 'field :side ,side))
         (*playingp* nil))
     ,@body))
