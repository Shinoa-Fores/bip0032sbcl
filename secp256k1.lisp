; Code for the Elliptic Curve secp256k1

; https://en.bitcoin.it/wiki/Secp256k1

; p : the 256 bit int prime in secp256k1
(defvar p (- (ash 1 256) (+ (ash 1 32) (ash 1 9) (ash 1 8) (ash 1 7) (ash 1 6) (ash 1 4) 1)))

; Z mod p operations

; x+y mod p
(defun add (x y) (mod (+ x y) p))

; x-y mod p
(defun sub (x y) (add x (- p y)))

; x*y mod p
(defun mul (x y) (mod (* x y) p))

; x^n mod p
(defun pow (x n) (if (= n 0) 1 (let ((z (pow (mul x x) (ash n -1)))) (if (evenp n) z (mul z x)))))

; Quotient function used for the Extended Euclidean Algorithm
; a,b : int
; return quotient and remainder
(defun quo (a b) (let ((r (mod a b))) (values (/ (- a r) b) r)))

; Extended Euclidean Algorithm
(defun eea (a b)
  (let ((x 0) (lastx 1) (y 1) (lasty 0) (q nil) (r nil))
    (loop while (> b 0) do
      (multiple-value-setq (q r) (quo a b))
      (setq a b b r)
      (let ((lx x) (ly y))
	(setq x (sub lastx (mul q x)) lastx lx)
	(setq y (sub lasty (mul q y)) lasty ly)))
    (values lastx lasty)))

; multiplicative inverse mod p
(defun inv (x) (multiple-value-bind (u v c) (eea p (mod x p)) v))

; elliptic curve
(defun ys (x) (add (pow x 3) 7))

; Tests if a point is on the curve
(defun e (x y) (= (mul y y) (ys x)))

; Addition for points on the elliptic curve
; Simplified from the general case using the fact that a is 0
; nil is used for the zero point
; (xp,yp) : point p on the curve
; (xq,yq) : point q on the curve
; return point p+q as two values xr yr (or nil)
(defun addp (xp yp xq yq)
  (if (and xp yp xq yq)
      (if (= xp xq)
	  (if (= (add yp yq) 0)
	      nil ; 'zero point'
	; otherwise assume yp = yq since they are points on the curve
	    (let* ((s (mul (inv (mul 2 yp)) (mul 3 (mul xp xp))))
		   (xr (add (mul s s) (mul (- p 2) xp)))
		   (yr (mod (- p (add yp (mul s (sub xr xp)))) p)))
	      (values xr yr)))
	(let* ((s (mul (sub yp yq) (inv (sub xp xq))))
	       (xr (sub (mul s s) (add xp xq)))
	       (yr (mod (- p (add yp (mul s (sub xr xp)))) p)))
	  (values xr yr)))
    (if (and xp yp)
	(values xp yp)
      (values xq yq))))

; Scalar multiplication
; k : int
; (xp,yp) : point p on the curve
; return point k*p as two values xr yr (or nil)
(defun smulp (k xp yp)
  (if (> k 0)
      (multiple-value-bind
	  (xq yq)
	  (addp xp yp xp yp)
	(multiple-value-bind
	    (xr yr)
	    (smulp (ash k -1) xq yq)
	  (if (evenp k)
	      (values xr yr)
	    (addp xp yp xr yr))))
    nil))

; base point g
(defvar xg #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)
(defvar yg #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8) 

; order of g
(defvar n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)
