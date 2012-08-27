#+nil
(ql:quickload '(hunchentoot cl-who parenscript cl-fad zpng))

(defpackage :webc
  (:use #:cl #:hunchentoot
	#:cl-who #:parenscript
	#:cl-fad))

(in-package :webc)

(setf parenscript:*js-string-delimiter* #\")

#+nil
(defparameter *acceptor*
 (start (make-instance 'easy-acceptor :port 8080
		       :access-log-destination *standard-output*
		       :message-log-destination *standard-output*)))
#+nil
*dispatch-table*
#+nil
hunchentoot::*easy-handler-alist*

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(define-easy-handler (tutorial1 :uri "/tutorial1" :acceptor-names t) ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "parenscript tutorial: exampl 1"))
     (:body (:h1 "parenscript tut 1")
	    "Pleas click link below." :br
	    (:a :href "#" :onclick (ps (alert "hello world"))
		"hello world")
	    ))))
(define-easy-handler (tutorial2 :uri "/tutorial2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial: 2nd example")
      (:script :type "text/javascript"
               (str (ps
                      (defun greeting-callback ()
                        (alert "Hello World"))))))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World")))))

(define-easy-handler (ajax/bla :uri "/ajax/bla") ()
    (format nil "~d" (random 123)))

(defun draw-mandelbrot (file pic-number)
  (let* ((png (make-instance 'zpng:png
                             :color-type :grayscale-alpha
                             :width 64
                             :height 64))
         (image (zpng:data-array png))
         (max 255))
    (dotimes (y 64 (zpng:write-png png file))
      (dotimes (x 64)
	(let ((c (complex (- (/ x 16.0) 1.5 (* pic-number .1)) (- (/ y 16.0) 1.5)))
              (z (complex 0.0 0.0))
              (iteration 0))
          (loop
	     (setf z (+ (* z z) c))
	     (incf iteration)
	     (cond ((< 4 (abs z))
		    (setf (aref image y x 1) iteration)
		    (return))
		   ((= iteration max)
		    (setf (aref image y x 1) 255)
		    (return)))))))))




#+NIL
(time 
 (draw-mandelbrot "/dev/shm/o.png"))

(define-easy-handler (mma :uri "/mma") (pic-number)
  (setf (hunchentoot:content-type*) "image/png")
  (let ((fn (make-pathname :name "mma" :type "png" :version nil)))
    (draw-mandelbrot fn (read-from-string pic-number))
    (with-open-file (in fn :element-type '(unsigned-byte 8))
      (let ((image-data (make-array (file-length in)
				    :element-type 'flex:octet)))
	(read-sequence image-data in)
	image-data))))


(define-easy-handler (tabs :uri "/tabs") ()
    (with-html-output-to-string (s nil :prologue t :indent t)
      (:html :lang "en"
	     (:head
	      (:link :rel "stylesheet" :type "text/css"
		     :href "jquery-ui/development-bundle/themes/ui-lightness/jquery.ui.all.css")
	      (:meta :http-equiv "Content-Type"
		     :content "text/html; charset=utf-8")
	      (:title "jQuery UI Tabs Example 1")
	      (:style "#draggable {width:150px; height:150px; padding:0.5em;}")
	      )
	     (:body 
	      (:div :id "tabs"
		    (:ul (:li (:a :href "#tab-focus" "focus"))
			 (:li (:a :href "#tab-mma" "mma"))
			 (:li (:a :href "#tab-lcos" "lcos"))
			 (:li (:a :href "#tab-cam" "camera")))
		    (:div :id "tab-mma"
			  "This is the content panel linked to the first tab.")
		    (:div :id "tab-focus"
			  (:ul
			       (:li (:select :id "selector"
					     (:option :value "1" "1")
					     (:option :value "2" "2")
					     (:option :value "3" "3")))
			       (:li (:input :id "value" :name "value" :type "text" :size "10" :maxlength "10"))
			       (:li (:div :id "value2"))
			       (:li (:div :id "draggable" :class "ui-widget-content"
					  (:p "drag me around")))
			       (:li (:div :id "slider"))
			       (:li (:table (loop for j below 3 do
						 (htm 
						  (:tr (loop for i below 3 do
							    (htm
							     (:td (:image :src (format nil "/mma?pic-number=~d" (+ j (* 3 i))))))))))))))
		    (:div :id "tab-lcos"
			  "This is the content panel linked to the first tab.")
		    (:div :id "tab-cam"
			  "second tab"))
	      
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/jquery-1.8.0.js")
	      (loop for e in '("core" "widget" "mouse" "slider" "tabs" "draggable") do
		   (htm (:script :type "text/javascript"
				 :src (concatenate 'string "jquery-ui/development-bundle/ui/jquery.ui." e ".js"))))
	      
	      (:script :type "text/javascript"
		       (str (ps ($ (lambda () 
				     ((chain ($ "#tabs") tabs))
				     ((chain ($ "#draggable") draggable))
				     ((chain ($ "#slider") 
					     (slider 
					      (create slide (lambda ()
							      ((@ $ get) (concatenate 'string 
										      "ajax/process-slider"
										      "?slider-value="
										      (encode-u-r-i-component (chain ($ "#slider") (slider "value")))) 
							       (lambda (r)
								 #+nil (chain ($ "#value") (html r))
								 (chain ($ "#value") (attr "value" r)))))))))
				     (chain ($ "#selector") 
					    (change
					     (lambda ()
					       ((@ $ get) "/ajax/bla" (lambda (r)
								       (chain ($ "#value") (html r))))))))))))))))

(hunchentoot:define-easy-handler (process-slider :uri "/ajax/process-slider") (slider-value)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" slider-value))


#+nil
(ql:quickload "hunchentoot-test")
#+nil
(defparameter *acceptor2*
 (start (make-instance 'acceptor :port 4242
		       :access-log-destination *standard-output*
		       :message-log-destination *standard-output*)))
#+nil
(hunchentoot-test:test-hunchentoot "http://localhost:4242")
