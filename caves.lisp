(in-package caves)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec4) ;; position
     (1 :vec3) ;; normal
     (2 :vec4) ;; uv
     (3 :vec4) ;; color
     )))

(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 0 x y z w))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun normal (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 2 x y z w))
(defun color (r g b &optional (a 1.0))
  (glim:attrib-f 3 r g b a))

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))
(defparameter *kx* 0)
(defparameter *ky* 0)
(defparameter *mouse* t)


(defclass caves (scratchpad)
  ((cave :initform (make-array '(100 100) :element-type 'bit :initial-element 0)
         :accessor cave)
   (seed :initform 0 :accessor seed)
   (mask-data :initform nil :accessor mask-data)
   (scale :initform 1 :accessor scale)
   (spacing :initform 16 :accessor spacing)
   (origin :initform :center :accessor origin)
   (shapes :initform nil :accessor shapes)
   (last-time :initform 0 :accessor last-time)
   (x1 :initform 0 :accessor x1)
   (x2 :initform 0 :accessor x2)
   (y1 :initform 0 :accessor y1)
   (y2 :initform 0 :accessor y2))

  (:default-initargs :shaders '((:tex :vertex vertex :fragment fragment)
                                (:solid :vertex vertex :fragment solid))))

(defmethod w ((c caves))
  (w (cave c)))
(defmethod h ((c caves))
  (w (cave c)))
(defmethod w ((c array))
  (array-dimension c 0))
(defmethod h ((c array))
  (array-dimension c 1))


(defvar *w* nil)

(defmacro do-cells ((map x y w h) &body body)
  (a:once-only (map)
    `(loop with ,w = (array-dimension ,map 0)
           with ,h = (array-dimension ,map 1)
           for ,y below ,h
           do (loop for ,x below ,w
                    do (progn ,@body)))))

(defun maybe-regen (w)
  (let ((time (get-internal-real-time)))
    (when (and (aref *flags* 1)
               (> (/ (- time (last-time w))
                     internal-time-units-per-second)
                  0.25))
      (setf (last-time w) time)
      (format t "seed was ~s~%" (seed w))
      (incf (seed w))
      (format t "seed => ~s~%" (seed w))
      (gen-map w))))

(defmethod display ((w caves) now)
  (setf *w* w)
  (maybe-regen w)
  (glim:with-state (*format*)

    (glim:uniform 'proj sb-cga:+identity-matrix+)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)

    (gl:enable :depth-test :sample-alpha-to-coverage
               :polygon-smooth :line-smooth
               :blend :multisample :texture-2d)

    (gl:blend-func :src-alpha :one-minus-src-alpha)

    (gl:disable :cull-face :lighting :light0 :light1
                :depth-test :sample-alpha-to-coverage)
    (glim:uniform 'debug1 *debug* )
    (glim:uniform 'tex0 0)
    (glim:uniform 'tex1 1)
    (glim:uniform 'flip 0)

    (let* ((x1 0)
           (y1 0)
           (scale (scale w))
           (step (spacing w))
           (x2 (/ (wx w) scale))
           (y2 (/ (wy w) scale)))
      (glim:with-pushed-matrix (:modelview)
        (ecase (origin w)
          (:center
           (setf x2 (/ (wx w) scale))
           (setf y2 (/ (wy w) scale))
           (setf x1 (- x2))
           (setf y1 (- y2)))
          (:lower-left
           (glim:translate -1 -1 0))
          ;; todo: add others
          )
        (glim:scale (/ (wx w)) (/ (wy w)) 1)
        (glim:translate 0.5 0.5 0)
        (glim:scale scale scale 1)
        (glim:uniform 'mv (glim:ensure-matrix :modelview))
        (glim:uniform 'mvp (sb-cga:matrix*
                            (glim:ensure-matrix :projection)
                            (glim:ensure-matrix :modelview)))
        (glim:uniform 'normal-matrix (glim:ensure-matrix :modelview))
        (setf (x1 w) x1)
        (setf (x2 w) x2)
        (setf (y1 w) y1)
        (setf (y2 w) y2)
        (glim:with-draw (:quads :shader :solid)
          (color 1 0.7 0.7 1)
          (let ((map (cave w)))
            (do-cells (map x y w h)
              (let ((sy1 (* step (- y (/ h 2))))
                    (sy2 (* step (- (1+ y) (/ h 2))))
                    (sx1 (* step (- x (/ w 2))))
                    (sx2 (* step (- (1+ x) (/ w 2)))))
                (when (plusp (aref map x y))
                  (vertex sx1 sy1 1)
                  (vertex sx1 sy2 1)
                  (vertex sx2 sy2 1)
                  (vertex sx2 sy1 1))))))
                                        ;(format t "x1=~s,~s, 2=~s,~s~%" x1 y1 x2 y2)
        (glim:with-draw (:lines :shader :solid)
          (color 0.1 0.1 0.2 1)
          (when (minusp y1)
            (loop for x from 0 downto x1 by step
                  do (vertex x y1)
                     (vertex x y2)))
          (loop for x from 0 upto x2 by step
                do (vertex x y1)
                   (vertex x y2))
          (when (minusp y1)
            (loop for y from 0 downto y1 by step
                  do (vertex x1 y)
                     (vertex x2 y)))
          (loop for y from 0 upto y2 by step
                do (vertex x1 y)
                   (vertex x2 y))

          (color 0.2 0.2 0.4 1)
          (vertex x1 0)
          (vertex x2 0)
          (vertex 0 y1)
          (vertex 0 y2)
          (color 1 0 0 1)
          (let ((w (array-dimension (cave w) 0))
                (h (array-dimension (cave w) 1)))
            (vertex (* step (/ w -2)) (* step (/ h -2)) 1)
            (vertex (* step (/ w -2)) (* step (/ h 2)) 1)

            (vertex (* step (/ w -2)) (* step (/ h 2)) 1)
            (vertex (* step (/ w 2)) (* step (/ h 2)) 1)

            (vertex (* step (/ w 2)) (* step (/ h 2)) 1)
            (vertex (* step (/ w 2)) (* step (/ h -2)) 1)

            (vertex (* step (/ w 2)) (* step (/ h -2)) 1)
            (vertex (* step (/ w -2)) (* step (/ h -2)) 1)))
        (dispatch-draws w)))))

(defmethod mouse ((w caves) button state x y)
  (format t "~s ~s~%" button state)
  (when (eql state :down)
    (case button
      (:left-button
       )
      (:right-button
       ))))

(defmethod mouse-wheel ((window caves) button state x y)
  (if (eql state :up)
      (setf (scale window) (* (scale window) 1.1))
      (setf (scale window) (/ (scale window) 1.1))))

(defun mask-circle (win x y w h rnd)
  (declare (ignorable win))
  (let ((r (sqrt (+ (expt (/ (- x (/ w 2) -1/2) w) 2)
                    (expt (/ (- y (/ h 2) -1/2) h) 2)))))
    (and (< r 0.5) (< rnd 0.55))))

(defun mask-metaball (win x y w h rnd)
  (declare (ignorable win w h))
  (when (< rnd 0.6)
    (loop with rmin = 10000
          with wmin = 0.55
          for (mx my mr2 w) in (mask-data win)
          for r2 = (+ (expt (- x mx -1/2) 2)
                      (expt (- y my -1/2) 2))
          for d = (/ mr2 r2)
          sum d into in
          when (< r2 rmin)
            do (setf wmin w rmin r2)
          finally (return (or (and (> in 1.5)
                                   (< rnd 0.3))
                              (and (< rnd wmin)
                                   (> in 1.75)))))))


(defun mask (win x y w h r)
  (mask-metaball win x y w h r))

(defun prefix (map)
  (let ((p (make-array (array-dimensions map) :element-type '(unsigned-byte 32)
                                              :initial-element 0))
        (left 0))
    (do-cells (map x y w h)
      (let ((up (if (zerop y)
                    0
                    (aref p x (1- y))))
            (c (aref map x y)))
        (setf left (if (zerop x)
                       0
                       (+ left c)))
        (setf (aref p x y) (+ left up))))
    p))

(defun smooth (map)
  (let ((p (prefix map)))
    (do-cells (map x y w h)
      (let* ((x1 (a:clamp (- x 2) 0 (1- w)))
             (x2 (a:clamp (1+ x) 0 (1- w)))
             (y1 (a:clamp (- y 2) 0 (1- h)))
             (y2 (a:clamp (1+ y) 0 (1- h)))
             (c (+ (aref p x1 y1)
                   (aref p x2 y2)
                   (- (aref p x1 y2))
                   (- (aref p x2 y1)))))
        (setf (aref map x y)
              (if (> c 4) 1 0))))))

(defun gen-map (win &key (smooth :auto))
  (let ((*random-state* (sb-ext:seed-random-state (seed win))))
    (setf (mask-data win)
          (loop with w = (float (w win))
                with h = (float (h win))
                with a = (* w h)
                for r = (+ (min 10 (* (min w h) 1/10))
                           (random (min 10 (* (min w h) 1/10))))
                repeat 100
                collect (list (+ (* 2 r) (random (- w (* r 4))))
                              (+ (* 2 r) (random (- h (* r 4))))
                              (expt (* r 1) 2)
                              (+ 0.5 (random 0.1)))
                sum (* 3.14 r r) into a2
                while (< (/ a2 a) 0.30)))
    (let ((map (cave win)))
      (do-cells (map x y w h)
        (if (mask win x y w h (random 1.0))
            (setf (aref map x y) 1)
            (setf (aref map x y) 0)))
      (when smooth
        (if (numberp smooth)
            (loop repeat smooth
                  do (smooth map))
            (loop repeat (a:clamp
                           (floor (- (* (log (* (w win) (h win))
                                             2))
                                     3))
                           3 10)
                  do (smooth map)))))))


(defmethod keyboard ((window caves) key x y)
  (declare (ignore x y))
  (print key)
  (case key
    (:key-right
     (setf *mouse* nil))
    (:key-left
     (setf *mouse* nil))
    (:key-down
     (setf *mouse* nil))
    (:key-up
     (setf *mouse* nil))

    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))

    (#\space
     (gen-map window))
    (#\d
     (time
      (gen-map window :smooth nil))
     )
    (#\m
     (smooth (cave window)))
    (#\n
     (format t "seed was ~s~%" (seed window))
     (incf (seed window))
     (format t "seed => ~s~%" (seed window))
     (gen-map window))
    (#\b
     #++(time (loop for i below 10 do (setf (seed window) i) (gen-map window))))
    (#\s (setf (cave window)
               (make-array '(35 35)
                           :element-type 'bit :initial-element 0))
     (gen-map window))
    (#\l
     (setf (cave window)
           (make-array '(100 100)
                       :element-type 'bit :initial-element 0))
     (gen-map window))
    (#\x
     (setf (cave window)
           (make-array '(300 300)
                       :element-type 'bit :initial-element 0))
     (time (gen-map window)))
    #++(#\m (setf *mouse* t))
    #++(#\d
        (unless *debug* (setf *debug* 0))
        (setf *debug* (mod (1+ *debug*) 4))
        (format t "debug = ~s~%" *debug*))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod entry ((w caves) state)
  ;;  (format t "mouse -> ~s~%" state)
  #++(when (eql state :left) (break "enter ~s~%" state))
  (setf *mouse* (eql state :entered)))

(defmethod init-gl ((w caves))
  (gl:pixel-store :unpack-alignment 1)
  (gl:disable :dither))

(defun caves (&rest args)
  (glut:display-window (apply #'make-instance 'caves args)))


;(setf *print-circle* nil)
#++
(ql:quickload 'caves)
#++
(caves :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(glut:show-window)



