(eval-when (:execute :load-toplevel :compile-toplevel)
 (ql:quickload '(hunchentoot cl-who parenscript cl-fad zpng))
 (load "../cl-pl2303/libusb0.lisp"))

(defpackage :webc
  (:use #:cl #:hunchentoot
	#:cl-who #:parenscript
	#:cl-fad))

(in-package :webc)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

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



(progn
    (defvar *zeiss-connection*
      (make-instance 'libusb0::usb-connection
		     :vendor-id #x067b
		     :product-id #x2303
		     :configuration 1
		     ;;:endpoint #x83 ; 2 #x81 #x83qu
		     :interface 0))
    (libusb0::prepare-zeiss *zeiss-connection*))


(defmethod zeiss-mcu-read-position ((con libusb0::usb-connection))
 (loop for e in '(X Y Z) collect
      (let* ((str (format nil "~Ai;" e))
	     (a (make-array (length str)
			    :element-type '(unsigned-byte 8)
			    :initial-contents (map 'list #'char-code str))))
	(libusb0::bulk-write con a :endpoint 2)
	(sleep .1)
	(list e
	      (let ((str ;; remove trailing ^M
		     (map 'string #'code-char
			  (libusb0::bulk-read con #x5 :endpoint #x83))))
		(read-from-string (subseq str 0 (1- (length str)))))))))

#+nil
(zeiss-mcu-read-position *zeiss-connection*)

(defmethod zeiss-mcu-write-position-x ((con libusb0::usb-connection) pos)
  (let* ((str (format nil "!!XA~d;" pos))
	 (a (make-array (length str)
			:element-type '(unsigned-byte 8)
			:initial-contents (map 'list #'char-code str))))
    (libusb0::bulk-write con a :endpoint 2)))

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

(progn
 (defun draw-circle (file w h)
   (let* ((png (make-instance 'zpng:png
			      :color-type :grayscale
			      :width w
			      :height h))
	  (image (zpng:data-array png)))
     (dotimes (y h (zpng:write-png png file))
       (dotimes (x w)
	 (let ((xx (- (/ x w) .5))
	       (yy (- (/ y h) .5)))
	   (if (< (+ (expt xx 2) (expt yy 2)) (expt .4 2))
	       (setf (aref image y x 0) 0)
	       (setf (aref image y x 0) 255)))))))

 (define-easy-handler (circle :uri "/circle") (width height)
   (setf (hunchentoot:content-type*) "image/png")
   (let ((fn (make-pathname :name "circle" :type "png" :version nil)))
     (draw-circle fn (read-from-string width) (read-from-string height))
     (with-open-file (in fn :element-type '(unsigned-byte 8))
       (let ((image-data (make-array (file-length in)
				     :element-type 'flex:octet)))
	 (read-sequence image-data in)
	 image-data)))))




(define-easy-handler (tabs :uri "/tabs") ()
    (with-html-output-to-string (s nil :prologue t :indent t)
      (:html :lang "en"
	     (:head
	      (:link :rel "stylesheet" :type "text/css"
		     :href "jquery-ui/development-bundle/themes/base/jquery.ui.all.css")
	      (:meta :http-equiv "Content-Type"
		     :content "text/html; charset=utf-8")
	      (:title "jQuery UI Tabs Example 1")
	      (:style "#draggable {width:150px; height:150px; padding:0; margin:0;}
#camera-chip {width:400px; height:300px; padding:0; margin:0;}
#draggable p { text-align: center; margin: 0; }")
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
			  #+nil(:div :id "camera-chip" :class "ui-widget-content"
				(:img :src "/circle?width=320&height=240")
				(:div :id "draggable" :class "ui-widget-content"
				      (:p :class "ui-widget-content" "region of interest")))
			  (:ul
			       #+nil(:li (:select :id "selector"
					     (:option :value "1" "1")
					     (:option :value "2" "2")
					     (:option :value "3" "3")))
			       (:li (:input :id "value" :name "value" :type "text" :size "10" :maxlength "10"))
			      #+nil (:li (:div :id "value2"))
			       (:li (loop for (coord val) in
					 (zeiss-mcu-read-position *zeiss-connection*) do
					 (htm (:input :id coord
						      :type "text" :maxlength "5"
						      :value val))))
			       (:li (:div :id "slider"))
			      #+nil (:li (:table
				     (loop for j below 3 do
					  (htm 
					   (:tr (loop for i below 3 do
						     (htm
						      (:td (:image 
							    :src
							    (format nil "/mma?pic-number=~d" 
								    (+ j (* 3 i))))))))))))))
		    (:div :id "tab-lcos"
			  "This is the content panel linked to the first tab.")
		    (:div :id "tab-cam"
			  "second tab"))
	      
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/jquery-1.8.1.min.js")
	      (loop for e in '("core" "widget" "mouse" "slider" "tabs" "draggable" "resizable") do
		   (htm (:script :type "text/javascript"
				 :src (concatenate 'string "jquery-ui/development-bundle/ui/jquery.ui." e ".js"))))
	      
	      (:script 
	       :type "text/javascript"
	       (str (ps ($ (lambda () 
			     ((chain ($ "#tabs") tabs))
			     #+Nil (chain ($ "#draggable") (draggable (create containment "#camera-chip")))
			     #+nil (chain ($ "#draggable") (resizable (create
								       containment "#camera-chip"
								       )))
			     (chain ($ "#X")
				    (change
				     (lambda ()
				       ((@ $ get) 
					(concatenate 'string
						     "/ajax/x-motor-controller"
						     "?value=" ;; send value to server
						     (encode-u-r-i-component
						      (chain ($ "#X") (val))))
					(lambda (r) ;; replace value with response
					  (chain ($ "#X") (val r)))))))
			     #+nil((chain ($ "#slider") 
				     (slider 
				      (create slide
					      (lambda ()
						((@ $ get)
						 (concatenate 'string 
							      "ajax/process-slider"
							      "?slider-value="
							      (encode-u-r-i-component 
							       (chain ($ "#slider") (slider "value")))) 
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

(hunchentoot:define-easy-handler (x-motor-controller :uri "/ajax/x-motor-controller") (value)
  (setf (hunchentoot:content-type*) "text/plain")
  (zeiss-mcu-write-position-x *zeiss-connection* (read-from-string value))
  value)

#+nil
(zeiss-mcu-write-position-x *zeiss-connection* 3230)
#+nil
(zeiss-mcu-read-position *zeiss-connection*)

#+nil
(ql:quickload "hunchentoot-test")
#+nil
(defparameter *acceptor2*
 (start (make-instance 'acceptor :port 4242
		       :access-log-destination *standard-output*
		       :message-log-destination *standard-output*)))
#+nil
(hunchentoot-test:test-hunchentoot "http://localhost:4242")
