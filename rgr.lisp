(defparameter *f1* 1.0)   ;  умова для першої гілки 
(defparameter *f11* 1.0)  ;  умова для другої гілки 
(defparameter *f21-default* 1.0) ; Припущене значення для i=21

;; Реалізація функції обчислення 
(defun calculate-f (i)
  (cond
    ((= i 1) *f1*)
    ((= i 11) *f11*)
    
    ;; гілка i = 2 - 10 
    ((and (>= i 2) (<= i 10))
     (let ((prev-f (calculate-f (1- i))))
       (* (cos prev-f) (sqrt i))))
    
    ;; гілка i = 12 - 20  
    ((and (>= i 12) (<= i 20))
     (let ((next-f (if (= i 20) 
                       *f21-default* (calculate-f (1+ i)))))
       (* (sin next-f) (sqrt i))))
    
    (t (error "Index outside the specified ranges (1..10 or 11..20).")))) 

(defun tests ()
  (format t "~%~7A | ~15A" "i" "F_i")
  (format t "~%--------------------------")

  (format t "~%Range 1 (i=1..10):")
  (loop for i from 1 to 10 do
        (format t "~%~7D | ~15,6F" i (calculate-f i)))
  
  (format t "~%~%Range 2 (i=11..20):")
  (loop for i from 11 to 20 do
        (format t "~%~7D | ~15,6F" i (calculate-f i)))
  (format t "~%--------------------------")) 
