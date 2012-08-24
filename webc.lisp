#+nil
(ql:quickload '(hunchentoot cl-who parenscript cl-fad))

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

(define-easy-handler (tabs :uri "/tabs") ()
    (with-html-output-to-string (s nil :prologue t :indent t)
      (:html :lang "en"
	     (:head
	      (:link :rel "stylesheet" :type "text/css"
		     :href "jquery-ui/development-bundle/themes/ui-lightness/jquery.ui.all.css")
	      (:meta :http-equiv "Content-Type"
		     :content "text/html; charset=utf-8")
	      (:title "jQuery UI Tabs Example 1")
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
			       (:li (:div :id "value"))
			       (:li (:div :id "slider"))
			       (:li 
				"This is the content panel linked to the first tab.")))
		    (:div :id "tab-lcos"
			  "This is the content panel linked to the first tab.")
		    (:div :id "tab-cam"
			  "second tab"))
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/jquery-1.8.0.js")
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/ui/jquery.ui.core.js")
	      
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/ui/jquery.ui.widget.js")
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/ui/jquery.ui.mouse.js")
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/ui/jquery.ui.slider.js")
	      (:script :type "text/javascript"
		       :src "jquery-ui/development-bundle/ui/jquery.ui.tabs.js")

	      (:script :type "text/javascript"
		       (str (ps ($ (lambda () 
				     ((chain ($ "#tabs") tabs))
				     ((chain ($ "#slider") (slider (create slide (lambda ()
										   ((@ $ get) "/ajax/bla" (lambda (r)
													    (chain ($ "#value") (html r)))))))))
				     (chain ($ "#selector") 
					    (change
					     (lambda ()
					       ((@ $ get) "/ajax/bla" (lambda (r)
								       (chain ($ "#value") (html r))))))))))))))))


#+nil
(ql:quickload "hunchentoot-test")
#+nil
(defparameter *acceptor2*
 (start (make-instance 'acceptor :port 4242
		       :access-log-destination *standard-output*
		       :message-log-destination *standard-output*)))
#+nil
(hunchentoot-test:test-hunchentoot "http://localhost:4242")
