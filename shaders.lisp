(defpackage #:caves-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex #:fragment
           #:position #:normal #:uv #:color
           #:mv #:mvp #:normal-matrix #:tex0 #:tex1 #:flip #:debug1
           #:ref-fragment #:solid))
(in-package #:caves-shaders)

(input position :vec4 :location 0)
(input normal :vec3 :location 1)
(input uv :vec4 :location 2)
(input color :vec4 :location 3)

(output color :vec4 :stage :fragment)


;; uniforms
(uniform mv :mat4) ;; model-view matrix
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform normal-matrix :mat4)
(uniform tex0 :sampler-2d)
(uniform tex1 :sampler-2d)
(uniform flip :bool)
(uniform debug1 :int)


;; output from vertex shader, interpolated by GL then sent as input to
;; fragment shader
;; visible in vertex shader as 'outs', and in fragment shader as 'ins'
(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec4)
  (normal :vec3)
  (color :vec4)
  (uv :vec4)
  (eye-direction :vec3)
  (light-direction :vec3))

;; some constants for lighting (would probably be uniforms in a real program)
(defconstant +ambient-color+ (vec3 0.0 0.024 0.06) :vec3)
(defconstant +diffuse-color+ (vec3 0.6 0.4 0.2)#++(vec3 0.2 0.4 0.6) :vec3)
(defconstant +specular-exponent+ 16 :float)
(defconstant +light-position+ (vec3 4 4 -5) :vec3)

;; generic vertex shader used for a few lighting models
(defun vertex ()
  (setf gl-position (* mvp position))
  (setf (@ outs normal) (* (mat3 normal-matrix) normal)
        (@ outs position) (* mv position)
        (@ outs uv) uv
        (@ outs color) color
        ;; interpolated lighting parameters
        (@ outs light-direction) (- +light-position+ (vec3 (* mv position)))
        (@ outs eye-direction) (- (vec3 (* mv position)))))

(defun fragment ()
  (let ((c (texture tex0 (.xy (@ ins uv)))))
    (when (zerop (.a c))
      (discard))
    ;;(setf color (vec4 (.xyz c) 1))
    (setf color c)))

(defun solid ()
  (setf color (@ ins color)))
