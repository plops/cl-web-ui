(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(eval-when (:compile-toplevel)
 (ql:quickload '(hunchentoot cl-who parenscript cl-fad zpng cl-json))
 (load "../cl-pl2303/libusb0.lisp")
 (load "../mma-reverse/net-control.lisp")
 (setf asdf:*central-registry* 
       #+(and win32 x86-64) '(
			      "C:/Users/martin/stage/sb-andor2-win/")
       #+linux '("~/stage/sb-andor2-win/"))
 (require :sb-andor2-win))

#+nil
(progn
 (defparameter libusb0::*forthdd*
   (make-instance 'libusb0::usb-connection
		  :vendor-id #x19ec
		  :product-id #x0300
		  :configuration 1
		  :interface 0)))
#+nil
(libusb0::close-connection libusb0::*forthdd*)

#+nil
(c::mma-connect)
#+nil
(c::mma-init)
#+nil
(c::upload-disk-image :radius .5)
#+nil
(c::with-tcp c::*tcp* (c::status))

#+nil
(time
 (progn ;; initialize camera
   (and::initialize)
   (defparameter *clara-parameters* (make-instance 'clara-camera
						   :accumulations 1))
   (clara-set-parameters *clara-parameters*)))

#+nil
(and::get-status)

(defvar *clara-image* nil)
(defun clara-capture-image ()
  (when (multiple-value-bind (err stat) (and::get-status*)
	  (eq 'sb-andor2-win-internal::DRV_NOT_INITIALIZED
	      (and::lookup-error err)))
    (return-from clara-capture-image))
  (if (eq (and::get-status)
	  'SB-ANDOR2-WIN-INTERNAL::DRV_IDLE)
      (progn
	(and::start-acquisition)
	(and::wait-for-acquisition*)
	(multiple-value-bind (a b c)
	    (and::get-acquisition-timings)
	  (declare (ignore a bn))
	  (sleep c))
	(and::get-most-recent-image))
      *clara-image*))
#+nil
(defparameter *clara-image* (clara-capture-image))

#+nil
(and::*bla*)

#+nil 
(and::get-status)



(defclass clara-camera ()
  ((xstart :accessor xstart :initform 470 :initarg :xstart)
   (xend :accessor xend :initform 850 :initarg :xend)
   (ystart :accessor ystart :initform 350 :initarg :ystart)
   (yend :accessor yend :initform 750 :initarg :yend)
   (w :accessor w :initform 1392 :initarg :w)
   (h :accessor h :initform 1040 :initarg :h)
   (exposure-time :accessor exposure-time :initform .02f0
		  :initarg :exposure-time)
   (cycle-time :accessor cycle-time :initform .045f0 :initarg :cycle-time)
   (accumulations :accessor accumulations :initform 10 :initarg :accumulations)
   (slow-readout :accessor slow-readout :initform t :initarg :slow-readout)
   ))

#+nil
(clara-set-parameters (make-instance 'clara-camera :accumulations 1))

(defmethod clara-set-parameters ((camera clara-camera))
  (with-slots
	(xstart xend ystart yend w h exposure-time cycle-time accumulations slow-readout)
      camera
    (and::set-acquisition-mode 'and::kinetics)
    (and::set-exposure-time exposure-time)
    (and::check (and::set-number-accumulations* accumulations))
    ;; (check (set-accumulation-cycle-time* .2))
    (and::check (and::set-kinetic-cycle-time* cycle-time))
    (and::check (and::set-number-kinetics* 1))
    (and::set-read-mode 'and::image)
    (and::set-vs-speed)
    ;; Note: there is a 10us gap in SHUTTER before FIRE starts
    (and::check (and::set-shutter* 1
				   0 ;; auto
				   0 ;; closing time 
				   2 ;; opening time in ms
				   )) 
    (and::check (and::set-frame-transfer-mode* 0))
    (if slow-readout 
	(and::set-slowest-hs-speed)
	(and::set-fastest-hs-speed))
    (and::set-trigger-mode 'and::internal)
    (and::check (and::set-temperature* -40))
    (and::check (and::cooler-on*))
    ;;(set-isolated-crop-mode* 0 *h* *w* 1 1)
    (and::set-image :xstart xstart :ystart ystart :xend xend :yend yend)
    (setf and::*w* w
	  and::*h* h)
    (json:encode-json-to-string 
     (multiple-value-list (and::get-acquisition-timings)))))


(defmethod initialize-instance :after ((cam clara-camera) &rest args)
  (declare (ignore args))
  (with-slots (xstart xend ystart yend w h) cam
     (setf w (1+ (- xend xstart))
	   h (1+ (- yend ystart)))))

(defmethod print-object ((cam clara-camera) stream)
  (with-slots (exposure-time w h) cam
   (format stream "<cam ~a>" (list exposure-time w h))))



#+nil
(make-instance 'clara-camera)


(defpackage :webc
  (:use #:cl #:hunchentoot
	#:cl-who #:parenscript
	#:cl-fad))

(in-package :webc)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(setf parenscript:*js-string-delimiter* #\")

#+nil
(defparameter *acceptor*
 (start (make-instance 'easy-acceptor :port 8080
		       :access-log-destination *standard-output*
		       :message-log-destination *standard-output*)))
#+nil
(stop *acceptor*)
#+nil
*dispatch-table*

#+nil
(defparameter *zeiss-connection* nil)
(defvar *zeiss-connection* nil)
#+nil
(progn ;; open usb serial converter
    (defparameter *zeiss-connection*
      (make-instance 'libusb0::usb-connection
		     :vendor-id #x067b
		     :product-id #x2303
		     :configuration 1
		     :interface 0))
    (libusb0::prepare-zeiss *zeiss-connection*))



#+nil
(libusb0::prepare-zeiss *zeiss-connection*)
#+nil
(libusb0::close-connection *zeiss-connection*)


(defmethod zeiss-mcu-read-position ((con libusb0::usb-connection))
 (loop for e in '(X Y Z) collect
      (let* ((str (format nil "~Ai;" e))
	     (a (make-array (length str)
			    :element-type '(unsigned-byte 8)
			    :initial-contents (map 'list #'char-code str))))
	(libusb0::bulk-write con a :endpoint 2)
	(sleep .2)
	(list e
	      (let ((str ;; remove trailing ^M
		     (map 'string #'code-char
			  (libusb0::bulk-read con #x5 :endpoint #x83))))
		(read-from-string
		 (if (eq (aref str (1- (length str)))
			 #\Return)
		     (subseq str 0 (1- (length str)))
		     str)))))))



#+nil
(zeiss-mcu-read-position *zeiss-connection*)

(defmethod zeiss-mcu-write-position-x ((con libusb0::usb-connection) pos)
  (let* ((str (format nil "!!XA~d;" pos))
	 (a (make-array (length str)
			:element-type '(unsigned-byte 8)
			:initial-contents (map 'list #'char-code str))))
    (format t "mcu: ~a~%" pos)
    (libusb0::bulk-write con a :endpoint 2)))

(defmethod zeiss-mcu-write-position-y ((con libusb0::usb-connection) pos)
  (let* ((str (format nil "!!YA~d;" pos))
	 (a (make-array (length str)
			:element-type '(unsigned-byte 8)
			:initial-contents (map 'list #'char-code str))))
    (format t "mcu: ~a~%" pos)
    (libusb0::bulk-write con a :endpoint 2)))

(defmethod zeiss-mcu-write-position-z ((con libusb0::usb-connection) pos)
  (let* ((str (format nil "!!ZA~d;" pos))
	 (a (make-array (length str)
			:element-type '(unsigned-byte 8)
			:initial-contents (map 'list #'char-code str))))
    (format t "mcu: ~a~%" pos)
    (libusb0::bulk-write con a :endpoint 2)))


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

(defvar *clara-image* nil)

(define-easy-handler (clara-image :uri "/clara-image") (time)
  (declare (ignore time))
  (setf (hunchentoot:content-type*) "image/png")
  (defparameter *clara-image* (clara-capture-image))
  (if *clara-image*
    (destructuring-bind (w h) (array-dimensions *clara-image*)
     (let* ((png (make-instance 'zpng:png
				:color-type :grayscale
				:width w
				:height h))
	    (image (zpng:data-array png))
	    (fn (make-pathname :name "clara-image" :type "png" :version nil))
	    (ci1 (sb-ext:array-storage-vector *clara-image*))
	    (mi (reduce #'min ci1))
	    (gamma (let* ((n (length ci1))
			  (a (make-array n
					 :element-type 'single-float)))
		     (dotimes (i n)
		       (setf (aref a i) (expt (+ (aref ci1 i) .01 (- mi))
					      1.s0)))
		     a))
	    (ma2 (reduce #'max gamma))
	    (mi2 (reduce #'min gamma))
	    (d2 (- ma2 mi2)))
       (dotimes (y h (zpng:write-png png fn))
	 (dotimes (x w)
	   (setf (aref image y x 0)
		 (if *clara-image*
		     (min 255 (max 0 
				   (if (< 0 d2)
				       (floor (- (aref gamma (+ x (* w y))) mi2)
					      (/ d2 256s0))
				       0)))
		     128))))
       (with-open-file (in fn :element-type '(unsigned-byte 8))
	 (let ((image-data (make-array (file-length in)
				       :element-type '(unsigned-byte 8))))
	   (read-sequence image-data in) 
	   image-data))))
    "no image available"))

(defvar *clara-parameters* nil)



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
		    #+nil(:ul (:li (:a :href "#tab-focus" "focus"))
			 (:li (:a :href "#tab-mma" "mma"))
			 (:li (:a :href "#tab-lcos" "lcos"))
			 (:li (:a :href "#tab-cam" "camera")))
		    #+nil(:div :id "tab-mma"
			  "This is the content panel linked to the first tab.")
		    (:div :id "tab-focus"
			  (:table 
			   (:tr (:td
				 (:canvas :width 320 :height 100 :id "histogram-canvas"))
				(:td "settings"
				     (:div :id "progressbar")))
			   (:tr
				   (:td (if *clara-parameters*
					    (htm (:img :id "clara-image" :src "/clara-image"
						       :width (w *clara-parameters*)
						       :height (h *clara-parameters*)))
					    (htm "camera not initialized")))
				   (:td (:p (:button :id "capture-button" "capture") (:button :id "clara-start" "start") (:button :id "clara-stop" "stop"))
					(when *clara-parameters*
					  
					  (with-slots (exposure-time xstart xend ystart yend accumulations) *clara-parameters*
					    (htm (:form :id "clara-form"
							(:p (:input :type "checkbox" :id "slow-readout" ;:checked "checked"
								    )
							    (:label :from "slow-readout" "slow readout"))
							(:p (:input :type "radio" :name "capture-type" :id "capture-when-change" :checked "checked")
							    (:label :from "capture-when-change" "capture when change"))
							(:p (:input :type "radio" :name "capture-type" :id "capture-continuous")
							    (:label :from "capture-continuous" "live")
							    (:div :id "image-number" "0"))
							(:p (:input :type "radio" :name "capture-type" :id "capture-manual")
							    (:label :from "capture-manual" "manual"))
							(:p "exposure" (:input :id "exposure-time" :value exposure-time 
									       :maxlength "6" :size "6"))
							(:p "accumulations" (:input :id "accumulations" :value accumulations 
										    :maxlength "4" :size "4"))
							(:p "cam x"
							    (:input :id "xstart" :value xstart :maxlength "4" :size "4")
							    (:input :id "xend" :value xend :maxlength "4" :size "4"))
							(:p "cam y"
							    (:input :id "ystart" :value ystart :maxlength "4" :size "4")
							    (:input :id "yend" :value yend :maxlength "4" :size "4"))
							(:div :id "clara-timings" "()")
							 (when *zeiss-connection*
							   (loop for (coord val) in (zeiss-mcu-read-position *zeiss-connection*) and step in '(10 10 5)
							      do
								(htm (:div (:input :id coord
										   :type "number" 
										   :value val
										   :step step
										   :maxlength "4" :size "4")
									   (:label :from coord (str coord)))))
							   (htm (:p "note: increase Z to move into sample")))
							(:p "lcos pic" (:input :id "forthdd-picnumber"
									       :type "number" 
									       :value 108
									       :step 1
									       :maxlength "4" :size "4"))
							(:form :id "mma-form"
							 (:p "mma radius" (:input :id "mma-radius"
										  :type "number" 
										  :value 1.0
										  :step .1
										  :maxlength "4" :size "4"))
							 (:p "mma rho" (:input :id "mma-rho"
									       :type "number" 
									       :value 0.0
									       :step .1
									       :maxlength "4" :size "4"))
							 (:p "mma theta" (:input :id "mma-theta"
										 :type "number" 
										 :value 0.0
										 :step .1
										 :maxlength "4" :size "4"))))))))))
		       
			  #+nil(:div :id "camera-chip" :class "ui-widget-content"
				(:img :src "/circle?width=320&height=240")
				(:div :id "draggable" :class "ui-widget-content"
				      (:p :class "ui-widget-content" "region of interest")))
			  
			  #+nil(:li (:select :id "selector"
					     (:option :value "1" "1")
					     (:option :value "2" "2")
					     (:option :value "3" "3")))
			  #+nil (:li (:input :id "value" :name "value" :type "text" :size "10" :maxlength "10"))
			  #+nil (:li (:div :id "value2"))
			  
			  #+nil (:li (:div :id "slider"))
			  #+nil (:li (:table
				      (loop for j below 3 do
					   (htm 
					    (:tr (loop for i below 3 do
						      (htm
						       (:td (:image 
							     :src
							     (format nil "/mma?pic-number=~d" 
								     (+ j (* 3 i)))))))))))))
		  #+nil  (:div :id "tab-lcos"
			  "This is the content panel linked to the first tab.")
		   #+nil (:div :id "tab-cam"
			  ))
	      
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/jquery-1.8.1.min.js")
	      (loop for e in '("core" "widget" "mouse" "slider" "tabs" "draggable" 
			       "resizable" "button" "progressbar") do
		   (htm (:script :type "text/javascript"
				 :src (concatenate 'string "jquery-ui/development-bundle/ui/jquery.ui." e ".js"))))
	      
	      (:script 
	       :type "text/javascript"
	       
	       (str (ps ($ (lambda () 
			     #+nil((chain ($ "#tabs") tabs))
			     ((chain ($ "#capture-button") button))
			     ((chain ($ "#progressbar") progressbar))
			     
			     ((chain ($ "#clara-start") button))
			     ((chain ($ "#clara-stop") button))
			     

			     (chain 
			      ($ "#clara-form")
			      (change (lambda ()
					((@ $ get) 
					 (concatenate 'string
						      "/ajax/camera-settings"
						      "?exposure-time=" (encode-u-r-i-component (chain ($ "#exposure-time") (val)))
						      "&xstart=" (encode-u-r-i-component (chain ($ "#xstart") (val)))
						      "&xend=" (encode-u-r-i-component (chain ($ "#xend") (val)))
						      "&ystart=" (encode-u-r-i-component (chain ($ "#ystart") (val)))
						      "&yend=" (encode-u-r-i-component (chain ($ "#yend") (val)))
						      "&accumulations=" (encode-u-r-i-component (chain ($ "#accumulations") (val)))
						      "&slow-readout=" (encode-u-r-i-component (chain ($ "#slow-readout") (attr "checked"))))
					 (lambda (r) 
					   (chain ($ "#clara-timings") (html r))
					   (setf (chain window integration-time) (aref r 2))
					   (when (string= "checked"
							  (chain ($ "#capture-when-change") (attr "checked")))
					     (chain ($ "#capture-button") (click))))))))
			     
			     (let ((image-number 10)
				   (interval-func nil))
			       (labels ((update-clara-image ()
					 (chain ($ "#capture-button") (click))
					  (chain ($ "#image-number") (html (incf image-number)))))
				 (chain ($ "#capture-continuous")
					(change 
					 (lambda ()
					   (if (string= "checked"
							(chain ($ "#capture-continuous")
							       (attr "checked")))
					       (setf interval-func
						     (set-interval #'update-clara-image
								   (+ 200 (* 1000 (if (chain window integration-time)
										      (* 1.1 (chain window integration-time)) 
										      1)))))))))
				 (chain ($ "#capture-manual")
					(change 
					 (lambda ()
					   (when (and interval-func
						      (string= "checked"
							       (chain ($ "#capture-manual")
								      (attr "checked"))))
					     (clear-interval interval-func)
					     (setf image-number 0)))))))
				     
			     (chain ($ "#clara-image") 
				    (load ;; draw histogram, when a new image is loaded
				     (lambda ()
				       ((@ $ get)
					"/ajax/histogram-data.json"
					(lambda (r)
					  (let ((c (chain ($ "#histogram-canvas") (get 0)
							  (get-context "2d"))))
					    (setf (chain window bla) r)
					    
					    (chain c (clear-rect 0 0 320 240))
					    (chain c (fill-text (chain r 0) 6 95))
					    (chain c (fill-text (chain r 1) 290 95))
					    (chain c (begin-path))
					    (chain c (move-to 0 0))
					    (chain c (line-to 320 0))
					    (chain c (line-to 320 100))
					    (chain c (line-to 0 100))
					    (chain c (line-to 0 0))
					    (chain c (stroke))
					    (chain c (begin-path))
					    (chain c (move-to 0 0))
					    (let ((n (chain r 2 length)))
					      (dotimes (i n)
						(chain c (line-to (* (/ 320 n) i) (- 100 (aref (chain r 2) i))
								  ))))
					    (chain c (stroke))))))))
			     
			     
			     
			     (chain ($ "#capture-button") 
				    (click 
				     (lambda ()
				       (chain ($ "#clara-image")
					      (attr "src" 
						    (concatenate 'string "/clara-image?"
								 ((chain (new (*date)) get-time)))))
				       (let* ((i 0) ;; program the progress bar to update itself during exposure
					      (exp-ms (* 1000 (chain window integration-time)))
					      (time-step (min 500 (max 50 (/ exp-ms 30))))
					      (d (* 100 (/ time-step exp-ms)))) 
					 (labels ((update-progressbar ()
						    (chain ($ "#progressbar") 
							   (progressbar "option" "value" (incf i d)))
						    (when (< i 100)
						      (set-timeout #'update-progressbar time-step))))
					   (set-timeout #'update-progressbar time-step))))))
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

			     (chain ($ "#Y")
				    (change
				     (lambda ()
				       ((@ $ get) 
					(concatenate 'string
						     "/ajax/y-motor-controller"
						     "?value=" ;; send value to server
						     (encode-u-r-i-component
						      (chain ($ "#Y") (val))))
					(lambda (r) ;; replace value with response
					  (chain ($ "#Y") (val r)))))))
			     (chain ($ "#Z")
				    (change
				     (lambda ()
				       ((@ $ get) 
					(concatenate 'string
						     "/ajax/z-motor-controller"
						     "?value=" ;; send value to server
						     (encode-u-r-i-component
						      (chain ($ "#Z") (val))))
					(lambda (r) ;; replace value with response
					  (set-timeout
					   (chain ($ "#Z") (val r))
					   1000))))))
			     
			     (chain ($ "#forthdd-picnumber")
				    (change
				     (lambda ()
				       ((@ $ get) 
					(concatenate 'string
						     "/ajax/forthdd-settings"
						     "?pic-number=" ;; send value to server
						     (encode-u-r-i-component
						      (chain ($ "#forthdd-picnumber") (val))))
					(lambda (r))))))

			     (chain ($ "#mma-form")
				    (change
				     (lambda ()
				       ((@ $ get) 
					(concatenate 'string
						     "/ajax/mma-disk"
						     "?radius=" (encode-u-r-i-component (chain ($ "#mma-radius") (val)))
						     "&rho=" (encode-u-r-i-component (chain ($ "#mma-rho") (val)))
						     "&theta=" (encode-u-r-i-component (chain ($ "#mma-theta") (val))))
					(lambda (r))))))
			     
			     #+Nil (chain ($ "#draggable") (draggable (create containment "#camera-chip")))
			     #+nil (chain ($ "#draggable") (resizable (create
								       containment "#camera-chip"
								       )))
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


(defun calc-histogram (img2)
  (let* ((a1 (sb-ext:array-storage-vector img2))
	 (n (length a1))
	 (mi (reduce #'min a1))
	 (ma (reduce #'max a1))
	 (d (- ma mi))
	 (nh 45)
	 (hist (make-array nh :element-type 'fixnum)))
   (dotimes (i n)
     ;; make histogram with bins from [0 .. nh-1]
     (incf (aref hist (min (1- nh) 
			   (max 0 (if (< 0 d)
				      (round (* (1- nh) (- (aref a1 i) mi))
					     d)
				      0))))))
   (let* ((logs (map 'list #'(lambda (x) (if (< 0 x) (log x) 0s0)) hist))
	  (lmi (reduce #'min logs))
	  (lma (reduce #'max logs))
	  (dl (- lma lmi)))
     ;; scale log values to be integers in [0 .. 100]
     `(
       ,mi
       ,ma
       ,(mapcar #'(lambda (x) (max 0 (min 100 
					  (if (< 0 dl)
					      (floor (* 100 (- x lmi))
						     dl)
					      0)))) logs)))))

#+nil
(calc-histogram *clara-image*)

(hunchentoot:define-easy-handler (histogram-data :uri "/ajax/histogram-data.json") ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string (calc-histogram *clara-image*)))




(hunchentoot:define-easy-handler (process-slider :uri "/ajax/process-slider") (slider-value)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" slider-value))

(hunchentoot:define-easy-handler (mma-disk :uri "/ajax/mma-disk") (radius rho theta)
  (setf (hunchentoot:content-type*) "image/png")
  (progn ;; change mma image
    (if c::*tcp*
	(progn (c::upload-disk-image :radius (read-from-string radius)
				     :rho (read-from-string rho)
				     :theta (read-from-string theta))
	       (with-open-file (in "c:/Users/martin/stage/cl-web-ui/mma/0000.png"
				   :element-type '(unsigned-byte 8))
		 (let ((image-data (make-array (file-length in)
					       :element-type '(unsigned-byte 8))))
		   (read-sequence image-data in)
		   image-data)))
	"")))


(hunchentoot:define-easy-handler (forthdd-settings :uri "/ajax/forthdd-settings") 
    (pic-number)
  (setf (hunchentoot:content-type*) "text/plain")
  (progn ;; switch image/running order
    (when libusb0::*forthdd*
      (libusb0::forthdd-talk libusb0::*forthdd* #x23 
		    (list (read-from-string pic-number)))))
  "")

(defun capture-scan-lcos (pics)
  (dolist (p pics)
    (format t "~a~%" p)
    (libusb0::forthdd-talk libusb0::*forthdd* #x23 
			   (list p))
    (sleep .1)
    (defparameter *clara-image* (clara-capture-image))
    (sleep .1)
    (when *clara-image*
     (destructuring-bind (h w) (array-dimensions *clara-image*)
       (let* ((png (make-instance 'zpng:png
				  :color-type :grayscale
				  :width w
				  :height h))
	      (image (zpng:data-array png))
	      (fn (make-pathname :name (format nil "clara-image~4,'0d" p)
				 :type "png" :version nil))
	      (ci1 (sb-ext:array-storage-vector *clara-image*))
	      (mi (reduce #'min ci1))
	      (ma (reduce #'max ci1))
	      (d (- ma mi)))
	 (dotimes (y h (zpng:write-png png fn))
	   (dotimes (x w)
	     (setf (aref image y x 0)
		   (min 255
			(max 0 
			     (if (< 0 d)
				 (floor (- (aref ci1 (+ y (* h x))) mi)
					(/ d 256s0))
				 0))))))
	 (with-open-file (in fn :element-type '(unsigned-byte 8))
	   (let ((image-data (make-array (file-length in)
					 :element-type '(unsigned-byte 8))))
	     (read-sequence image-data in) 
	     image-data)))))))

#+nil
(time
 (capture-scan-lcos (concatenate 'list 
		     (loop for i from 50 below 75 collect i)
		     (loop for i from 100 below 200 collect i))))


(hunchentoot:define-easy-handler (camera-settings :uri "/ajax/camera-settings") 
    (exposure-time xstart xend ystart yend accumulations slow-readout)
  (setf (hunchentoot:content-type*) "application/json")
  (setf *clara-parameters* (make-instance 'clara-camera
					  :exposure-time (read-from-string exposure-time)
					  :xstart (read-from-string xstart)
					  :xend (read-from-string xend)
					  :ystart (read-from-string ystart)
					  :yend (read-from-string yend)
					  :accumulations (read-from-string accumulations)
					  :slow-readout (when (string= "checked" slow-readout)
							  t)))
  (clara-set-parameters *clara-parameters*))

(hunchentoot:define-easy-handler (x-motor-controller :uri "/ajax/x-motor-controller") (value)
  (setf (hunchentoot:content-type*) "text/plain")
  (when *zeiss-connection*
    (zeiss-mcu-write-position-x *zeiss-connection* (read-from-string value)))
  value)

(hunchentoot:define-easy-handler (y-motor-controller :uri "/ajax/y-motor-controller") (value)
  (setf (hunchentoot:content-type*) "text/plain")
  (when *zeiss-connection*
    (zeiss-mcu-write-position-y *zeiss-connection* (read-from-string value)))
  value)

(hunchentoot:define-easy-handler (z-motor-controller :uri "/ajax/z-motor-controller") (value)
  (setf (hunchentoot:content-type*) "text/plain")
  (zeiss-mcu-write-position-z *zeiss-connection* (read-from-string value))
  value)

#+nil
(zeiss-mcu-write-position-x *zeiss-connection* 3230)
#+nil
(zeiss-mcu-read-position *zeiss-connection*)

