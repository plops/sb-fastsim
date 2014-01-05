(eval-when (:compile-toplevel) (require :cl-opengl)
 (require :cl-glut)
 (require :cl-glu))
(defpackage :g (:use :cl :gl))
(in-package :g)

(defun v-period (ax ay bx by)
  (declare (type fixnum ax ay bx by)
	   (values fixnum &optional))
  (if  (or (= bx 0)  (= 0 ay) (= 0 by) (= 0 ax)) ;(= 0 ax)
      0
      (if (= 0 bx)
	  by
	  (* (lcm (abs ay) (abs by))
	     (- (/ ax ay)
		(/ bx by))))))

(defun h-period (ax ay bx by)
  (declare (type fixnum ax ay bx by)
	   (values fixnum &optional))
  (if (= 0 by)
      bx
      (if (or (= bx 0)  (= 0 ay) (= 0 by) (= 0 ax))
	  0
	  (* (lcm (abs ax) (abs bx))
	     (- (* ay ax)
		(* by bx))))))

(defun k (ax ay bx by)
  (let* ((f (* 2 pi))
	 (kax (/ f ax))
	 (kay (/ f ay))
	 (kbx (/ f bx))
	 (kby (/ f by))
	 (kx (+ kax kbx))
	 (ky (+ kay kby)))
    (values (atan ky kx) (atan ay ax)
	    (sqrt (+ (expt kx 2)
		     (expt ky 2)))
	    (* (sin (- (atan ay ax)
		       (atan by bx)))
	     (sqrt (+ (expt bx 2)
		      (expt by 2)))))))


(k 1 2 3 4)

(defun dir (ax ay)
  (declare (type fixnum ax ay)
	   (values double-float &optional))
  (atan (* 1d0 ay) (* 1d0 ax)))

(defun period (ax ay bx by)
  (declare (type fixnum ax ay bx by)
	   (values double-float &optional))
  (* (sin (abs (- (atan (* 1d0 ay) (* 1d0 ax))
	      (atan (* 1d0 by) (* 1d0 bx)))))
     (sqrt (* 1d0 (+ (expt bx 2)
		     (expt by 2))))))

#+nil
(time
 (defparameter *bla*
   (let* ((n 90)
	  (res (make-array (expt n 4) :element-type 'double-float))
	  (i 0))
     (loop for ax from 0 below n do
	  (loop for ay from 0 below n do
	       (loop for bx from 0 below n do
		    (loop for by from 0 below n do
			 (setf (aref res i) (period ax ay bx by))
			 (incf i)))))
     res)))

#+nil
(defparameter *hist*
 (let* ((ma (reduce #'max *bla*))
	(mi (reduce #'min *bla*))
	(n 100)
	(s (/ (* 1d0 (- n 1)) (- ma mi)))
	(hist (make-array n :element-type 'fixnum))
	(bin (make-array n :element-type 'double-float)))
   (loop for e across *bla* do
	(incf (aref hist (floor (* s (- e mi)))))
	(setf (aref bin (floor (* s (- e mi)))) e))
   (loop for i below n collect (list (aref bin i)
				     (aref hist i)))))



#+nil
(progn
 (time
  (defparameter *bla*
    ;; 4 loops over the parameters ax ay bx and by collect all
    ;; gratings that have a period from 14 to 16 and that allow phase
    ;; steps dividable by 4 or 7. there are LOTS of these gratings. i
    ;; found millions when i put n to 130
    (let* ((n 60)
	   (nn (* 100000))
	   (ntheta 6)
	   (s (/ (- ntheta 1) (* 2 pi)))
	   (res nil)
	   (i 0))
      (tagbody 
	 (loop for ax from (- n) below n do
	      (loop for ay from (- n) below n do
		   (loop for bx from 0 below n do
			(loop for by from 0 below n do
			     (let ((p (period ax ay bx by)))
			       (when (< (abs (- p 3.2)) .03)
				 (let ((v (v-period ax ay bx by))
				       (h (h-period ax ay bx by)))
				   (when (and (or (= 0 (mod v 4))
						  (= 0 (mod h 4)))
					      (or (= 0 (mod v 7))
						  (= 0 (mod h 7))))
				     (let* ((angle (atan ay ax))
					    (a (floor (* s (- angle (- pi))))))
					;(setf (aref res a (aref i a)) p)
				       (when (< (abs (- (* pi (/ 180) 34.6) angle)) .2)
					 (push (list a (* 180 (/ pi) angle) p ax ay bx by) res)
					 (incf i)
					 (when (= 0 (mod i 100000))
					   (defparameter *bluf* res)
					   (format t "~a~%" (list i ax ay bx by))))
				       #+nil (when (<= nn (aref i a))
					       (break "array insufficient")
					       (go :finished)))))
				 ))))))
       :finished)
      res)))
 #+nil (with-open-file (s "/sys/power/state" :direction :output
		   :if-exists :append )
  (format s "mem")))

;; triangle rasterization
;; Tutorial - Introduction to Software-based Rendering: Triangle Rasterization - joshbeam.com.html
(defclass edge ()
  ((x1 :initform 0 :reader x1 :initarg :x1 :type double-float)
   (x2 :initform 0 :reader x2 :initarg :x2 :type double-float)
   (y1 :initform 0 :reader y1 :initarg :y1 :type double-float)
   (y2 :initform 0 :reader y2 :initarg :y2 :type double-float)))

(defmethod initialize-instance :after ((e edge) &key)
  (with-slots (x1 x2 y1 y2) e
    (when (< y2 y1) ;; ensure first point has lower y value
      (rotatef x1 x2) (rotatef y1 y2))))

(defmethod print-object ((e edge) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (x1 x2 y1 y2) e
     (format stream "~a ~a" (list x1 x2) (list y1 y2)))))

#+nil
(make-instance 'edge :x1 0 :x2 100 :y1 10 :y2 300)
#+nil
(make-instance 'edge :x1 0 :x2 100 :y1 300 :y2 10)

(defun draw-triangle (x1 y1 x2 y2 x3 y3)
 ; (declare (type double-float x1 y1 x2 y2 x3 y3))
  (let* ((points `(((,x1 ,y1) (,x2 ,y2))
		   ((,x2 ,y2) (,x3 ,y3))
		   ((,x3 ,y3) (,x1 ,y1))))
	 (edges (loop for ((a b) (c d)) in points collect
		     (make-instance 'edge
				    :x1 a :y1 b
				    :x2 c :y2 d)))
	 (long-edge (let ((ma 0)
			  (j 0))
		      (loop for e in (mapcar #'(lambda (x) (- (y2 x) (y1 x)))
					     edges)
			   and i from 0
			 do
			   (when (< ma e)
			     (setf ma e
				   j i)))
		      j))
	 (short-edge-1 (mod (+ long-edge 1) 3))
	 (short-edge-2 (mod (+ long-edge 2) 3)))
    (defparameter *edges* edges)
    (defparameter *long-edge* long-edge)
    (draw-spans-between-edges (elt edges long-edge)
			      (elt edges short-edge-1))
    (draw-spans-between-edges (elt edges long-edge)
			      (elt edges short-edge-2))))

(defclass span ()
  ((x1 :initform 0 :reader span-x1 :initarg :x1 :type double-float)
   (x2 :initform 0 :reader span-x2 :initarg :x2 :type double-float)))

(defmethod initialize-instance :after ((s span) &key)
  (with-slots (x1 x2) s
    (when (< x2 x1) ;; ensure first coordinate has lower x value
      (rotatef x1 x2))))

(defmethod print-object ((s span) stream)
  (print-unreadable-object (s stream :type t)
    (with-slots (x1 x2) s
     (format stream "~a" (list x1 x2)))))

(defmethod draw-spans-between-edges ((e1 edge) (e2 edge))
  (let ((e1ydiff (- (y2 e1) (y1 e1)))
	(e2ydiff (- (y2 e2) (y1 e2)))
	(e1xdiff (- (x2 e1) (x1 e1)))
	(e2xdiff (- (x2 e2) (x1 e2))))
    (unless (or (= 0 e1ydiff) (= 0 e2ydiff))
      (let ((factor1 (/ (* 1d0 (- (y1 e2) (y1 e1)))
			e1ydiff))
	    (factorstep1 (/ 1d0 e1ydiff))
	    (factor2 0d0)
	    (factorstep2 (/ 1d0 e2ydiff)))
	(loop for y from (y1 e2) below (y2 e2) do
	     (draw-span
	      (make-instance 'span
			     :x1 (+ (x1 e1) (* factor1 e1xdiff))
			     :x2 (+ (x1 e2) (* factor2 e2xdiff)))
	      y)
	     (incf factor1 factorstep1)
	     (incf factor2 factorstep2))))))

(defmethod draw-span ((s span) y)
  (let ((xdiff (- (span-x2 s) (span-x1 s))))
    (unless (= xdiff 0)
      (let ((factor 0d0)
	    (factorstep (/ 1d0 xdiff)))
	(loop for x from (span-x1 s) below (span-x2 s) do
	     (set-pixel  (floor x) (floor y))
	     (incf factor factorstep))))))
(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *c* nil)
(defparameter *grating* nil)
(defparameter *color* 0)
(defun set-pixel (x y)
  (when *a*
    (destructuring-bind (h w) (array-dimensions *a*)
      (setf (aref *a*
		  (max 0 (min (floor y) (1- h)))
		  (max 0 (min (floor x) (1- w))))
	    *color*))))

#+nil
(let ((i (min  9 (1- (length *bla*))))) ;loop for i from 0 below (length *bla*) do
     (destructuring-bind (a angle period ax ay bx by) (elt *bla* i)
       (sleep .3)
       (let* ((h (* (+ (abs ay) (abs by))))
	      (w (* (+ (abs ax) (abs bx))))
	      (x 0)
	      (y 0)
	      (a (make-array (list h w)
			     :element-type '(unsigned-byte 8)))
	      (c (make-array (list h w)
			     :element-type 'double-float)))
	 (defparameter *grating* (list ax ay bx by))
	 (defparameter *a* a)
	 (setf *color* 1)
	 (draw-triangle x y
			(+ x ax) (+ y ay)
			(+ x (floor bx 1))
			(+ y (floor by 1)))
	 (draw-triangle (+ x ax) (+ y ay)
			(+ x (floor bx 1)) (+ y (floor by 1))
			(+ x ax (floor bx 1)) (+ y ay (floor by 1)))
	 (setf *color* 2)
	#+nil (draw-triangle x y
			(+ x ax) (+ y ay)
			(+ x (/ bx 2))
			(+ y (/ by 2)))
	#+nil (draw-triangle (+ x ax) (+ y ay)
			(+ x (/ bx 2)) (+ y (/ by 2))
			(+ x ax (/ bx 2)) (+ y ay (/ by 2)))
	 (let* ((f (* 2 pi))
	       (kax (/ f ax))
	       (kay (/ f ay))
	       (kbx (/ f bx))
	       (kby (/ f by))
	       (kx (+ kax kbx))
	       (ky (+ kay kby)))
	  (dotimes (i w)
	    (dotimes (j h)
	      (setf (aref c j i) (sin (+ (* ky j) (* kx i)))))))
	 (defparameter *c* c)
	 (defparameter *b* *a*))))

;; now some code for visualization of the grating densities or drawing
;; there unit cells

(let ((l 2))
  (defun draw ()
    (declare (optimize (debug 3)))
    (gl:clear :color-buffer)
    (gl:color 1 1 1)
    (enable :blend)
    (blend-func :src-alpha :one-minus-src-alpha)
    (gl:load-identity) ; clear the matrix
    ;; viewing transformation
    (glu:look-at 0 0 5  0 0 0  0 1 0)
    ;; modeling transformation
    (let ((s .2)) (gl:scale s s s))
    (incf l)
    #+nil (rotate 30 0 1 0)
    #+nil (rotate 20 1 0 0)
    #+nil (rotate (+
	      240) 0 1 0)
    (line-width 1)
    (glut:wire-cube 10)
    
    (with-primitive :lines
      (color 1 0 0) (vertex 0 0) (vertex 10 0)
      (color 0 1 0) (vertex 0 0) (vertex 0 10)
      (color 0 0 1) (vertex 0 0) (vertex 0 0 10))
    
    (point-size 2)
    (with-pushed-matrix
      (let ((s .2))
	(scale s s s))
      (translate  -30 0 0)
      (when *c*
       (with-primitive :points
	 (destructuring-bind (h w) (array-dimensions *c*)
	   (dotimes (i w)
	     (dotimes (j h)
	       #+nil(color 1 1 1 (if (< 0 (aref *c* j i))
				1
				0))
	       
	       (color 1 1 1 (* .5 (+ 1 (aref *c* j i))))
	       (vertex i j))))))
      (translate  0 -66 0)
      (when *b*
       (with-primitive :points
	 (destructuring-bind (h w) (array-dimensions *a*)
	   (dotimes (i w)
	     (dotimes (j h)
	       (case (aref *b* j i)
		 (2 (color 1 1 1 .9))
		 (1 (color 1 1 1 .3))
		 (0 (color 1 0 0 .3)))
	       (vertex i j)))))
       
       (when *grating*
	 (line-width 3)
	(with-primitive :lines
	  (destructuring-bind (a b c d) *grating*
	    (color 1 0 0 .6)
	    (vertex 0 0) (vertex a b)
	    (color 0 1 0 .6)
	    (vertex 0 0) (vertex c d))))))
    (glut:swap-buffers)
    (sleep (/ 3200))
    (glut:post-redisplay)))
#+nil
(defparameter *gl-display-thread* 
  (sb-thread:make-thread #'rb-cube :name "gl-display-thread"))

(defclass cube-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :title "cube.lisp"
                     :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w cube-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((w cube-window))
  (draw))

(defmethod glut:reshape ((w cube-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 20)
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w cube-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defun rb-cube ()
  (glut:display-window (make-instance 'cube-window)))
