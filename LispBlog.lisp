(defpackage :lisp-blog
  (:use :common-lisp))

(in-package :lisp-blog)

(require 'hunchentoot)
(require 'cl-who)

(defvar *server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242)))

(defvar *blogposts* nil)
(defvar *results* nil)


; The homepage for the blog 

(defun blog-home () 
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
    (:html
     (:head (:title "A Blog in Lisp!")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
      (:div :id "main"
      (:div :id "header"
      (:div :id "site-title"
	    (:a :href "/"
       "A Blog in Common Lisp")))
      (:div :id "access"
	    (:div :class "menu-header"
		  (:ul 
		   (:li
        (:a :href "/create-post" "Add new Post"))
		   (:li 
	(:a :href "/rss" "RSS")))))
	(:div :id "main"
	      (:div :id "content" 
		    
      (loop for ( post title posts link postss description postsss pubdate) in *blogposts*
	
	  do (cl-who:htm (:p
		     (:div :class "entry-title"
			   (:a :href link 
		      (cl-who:str title )))
			   (:div :class "entry-meta" "Date: "
				 (cl-who:str pubdate))
			   (:div :class "entry-content"
				 (cl-who:str description))))))))))))


 
; The RSS feed

(defun rss ()
  (cl-who:with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>" :indent t)
    (:rss :|version| "2.0"
	  (:channel (:title "Test title")
		    (:link "http://www.example.com")
		    
		    (:description "A sample feed")
		    (loop for ( post title posts link postss description postsss pubdate) in *blogposts*
		    
		 do (cl-who:htm (:item
		    (cl-who:htm (:title (cl-who:str title))
				    (:link (cl-who:str link))
					   (:description (cl-who:str description I))))))))))


(defun create-post ()
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
    (:html 
     (:head (:title "Add Post")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
         (:h1 "Create a new post")
    
      (:form :action "/save-post" :method "post"
	 (:div "Title:" (:input :type "text" :name "title"))
	   ;  (:div "Link:" (:input :type "text" :name "link"))
	     (:div "Main text:" (:input :type "text" :name "description"))
	  ;   (:div "author" (:input :type "text" :name "author"))
	  ;   (:div "comments" (:input :type "text" :name "comments"))
	  ;   (:div "guid" (:input :type "text" :name "guid"))
	  ;   (:div "category" (:input :type "text" :name "category"))
	 ;    (:div "source" (:input :type "text" :name "source"))
	  ;   (:div "enclosure" (:input :type "text" :name "enclosure"))
	     (:input :type "submit" :value "save"))))))


(defun save-post ()
  (push (make-blog-post (hunchentoot:parameter "title") (concatenate 'string "/post?i=" (hunchentoot:parameter "title")) (hunchentoot:parameter "description") (get-universal-time) (concatenate 'string "/post?i=" (hunchentoot:parameter "title")))
	     *blogposts*)     
  (save-db "/home/robojamie/Dropbox/file.lisp")
       (hunchentoot:redirect "/"))


(defun make-blog-post (title link description pubdate guid)
  (list :title title :link link :description description :pubdate pubdate :guid guid))

(defun add-blog-post (post) (push post *blogposts*))

; Regex dispatchers

(push (hunchentoot:create-regex-dispatcher "^/$" 'blog-home)
      hunchentoot:*dispatch-table*)


(push (hunchentoot:create-regex-dispatcher "^/save-post$" 'save-post)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/resources/(\\d*)" #'resource-function)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/create-post$" 'create-post)
      hunchentoot:*dispatch-table*)


(push (hunchentoot:create-regex-dispatcher "^/rss$" 'rss)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-static-file-dispatcher-and-handler "/style.css"
							     "/home/robojamie/Dropbox/LispBlog/style.css")
      hunchentoot:*dispatch-table*)



(defun get-pub-post (&optional i)
 
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
    (:html
     (:head (:title "A Blog in Lisp!")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
      (:div :id "main"
      (:div :id "header"
      (:div :id "site-title"
	    (:a :href "/"
       "A Blog in Common Lisp")))
      (:div :id "access"
	    (:div :class "menu-header"
		  (:ul 
		   (:li
        (:a :href "/create-post" "Add new Post"))
		   (:li 
	(:a :href "/rss" "RSS")))))
	(:div :id "main"
	      (:div :id "content" 
      (get-page i)
      (loop for ( post title posts link postss description postsss pubdate) in *results*
	
	  do (cl-who:htm (:p
		     (:div :class "entry-title"
			   (:a :href link 
		      (cl-who:str title )))
			   (:div :class "entry-meta" "Date: "
				 (cl-who:str pubdate)
				 
)
			   (:div :class "entry-content"
				 (cl-who:str description))))))))))))



(hunchentoot:define-easy-handler (post-getter :uri "/post") (i)
  (get-pub-post i))

(defun get-page (page)
(setf *results* 
(remove-if-not
     (lambda (post) (equal (getf post :title)  page)) *blogposts*)))




; Save and load posts from 

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *blogposts* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *blogposts* (read in)))))



(load-db "/home/robojamie/Dropbox/LispBlog/file.lisp")






