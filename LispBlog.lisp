(defpackage :lisp-blog
  (:use :common-lisp))

(in-package :lisp-blog)

(require 'hunchentoot)
(require 'cl-who)

; Variables for content (temporary!) 

(defvar *server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242)))
(defvar *channels* nil)
(defvar *items* nil)
(defvar *users* nil)

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
        (:a :href "/create-item" "Add new Post"))
		   (:li 
	(:a :href "/rss" "RSS")))))
	(:div :id "main"
	      (:div :id "content" 
		    
      (dolist (I *items*)
	(cl-who:htm (:p
		     (:div :class "entry-title"
		      (:a :href (link I) (cl-who:str (title I)))))
		    (:div :class "entry-meta"  "Date: "
		     (cl-who:str (pubdate I)))
		    (:div :class "entry-content"
		     (cl-who:str (description I))))))))))))

 
; The RSS feed

(defun rss ()
  (cl-who:with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>" :indent t)
    (:rss :|version| "2.0"
	  (:channel (:title "Test title")
		    (:link "http://www.example.com")
		    
		    (:description "A sample feed")
		    (dolist (I *items*)
		    (cl-who:htm (:item
		    (cl-who:htm (:title (cl-who:str (title I)))
				    (:link (cl-who:str (link I)))
					   (:description (cl-who:str (description I)))))))))))

; For creating a channel (perhaps when starting the blog) 

(defun create-channel ()
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
    (:html 
     (:head (:title "Add Channel")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
         (:h1 "Add a new channel")
    
      (:form :action "/save-channel" :method "post"
	     (:div "title:" (:input :type "text" :name "title"))
	     (:div "link:" (:input :type "text" :name "link"))
	     (:div "description:" (:input :type "text" :name "description"))
	    
	     
	     (:input :type "submit" :value "save"))))))

; Create a user/password (for login and admin stuff)

(defun create-user ()
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
       (:html 
     (:head (:title "Add User")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
         (:h1 "Add a new user")
    
      (:form :action "/save-user" :method "post"
	     (:div "username:" (:input :type "text" :name "username"))
	     (:div "password:" (:input :type "text" :name "password"))
	    )
	    
	     
	     (:input :type "submit" :value "save")))))

; Create an item to post on the home page 

(defun create-item ()
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
    (:html 
     (:head (:title "Add Post")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
         (:h1 "Create a new post")
    
      (:form :action "/save-item" :method "post"
	 (:div "Title:" (:input :type "text" :name "title"))
	     (:div "Link:" (:input :type "text" :name "link"))
	     (:div "Main text:" (:input :type "text" :name "description"))
	  ;   (:div "author" (:input :type "text" :name "author"))
	  ;   (:div "comments" (:input :type "text" :name "comments"))
	  ;   (:div "guid" (:input :type "text" :name "guid"))
	  ;   (:div "category" (:input :type "text" :name "category"))
	 ;    (:div "source" (:input :type "text" :name "source"))
	  ;   (:div "enclosure" (:input :type "text" :name "enclosure"))
	     (:input :type "submit" :value "save"))))))

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
	     (:div "Link:" (:input :type "text" :name "link"))
	     (:div "Main text:" (:input :type "text" :name "description"))
	  ;   (:div "author" (:input :type "text" :name "author"))
	  ;   (:div "comments" (:input :type "text" :name "comments"))
	  ;   (:div "guid" (:input :type "text" :name "guid"))
	  ;   (:div "category" (:input :type "text" :name "category"))
	 ;    (:div "source" (:input :type "text" :name "source"))
	  ;   (:div "enclosure" (:input :type "text" :name "enclosure"))
	     (:input :type "submit" :value "save"))))))

; Save the channel -- right now only to the variable

; Save the channel -- right now only to the variable 

(defun save-channel ()
  (let ((title (hunchentoot:parameter "title"))
	(link (hunchentoot:parameter "link"))
	(description (hunchentoot:parameter "description"))
)

       (push (make-instance 'channel :title title :link link :description description)
	     *channels*)      
       (hunchentoot:redirect "/")))

; Function to save posts 

(defun save-item ()
  (let ((title (hunchentoot:parameter "title"))
	(link (hunchentoot:parameter "link"))
	(description (hunchentoot:parameter "description"))
	(author (hunchentoot:parameter "author")))

       (push (make-instance 'item :title title :link link :description description :author author)
	     *items*)      
       (hunchentoot:redirect "/")))

; Function to save users

(defun save-user ()
  (let ((username (hunchentoot:parameter "username"))
	(password (hunchentoot:parameter "password")))

       (push (make-instance 'user :username username :password password)
	     *users*)      
       (hunchentoot:redirect "/")))

(defun save-post ()
  (push (make-blog-post (hunchentoot:parameter "title") (hunchentoot:parameter "link") (hunchentoot:parameter "description") (get-universal-time))
	     *blogposts*)      
       (hunchentoot:redirect "/")))



; Regex dispatchers

(push (hunchentoot:create-regex-dispatcher "^/$" 'blog-home)
      hunchentoot:*dispatch-table*)



(push (hunchentoot:create-regex-dispatcher "^/save-item$" 'save-item)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/save-post$" 'save-post)
      hunchentoot:*dispatch-table*)


(push (hunchentoot:create-regex-dispatcher "^/create-post$" 'create-post)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/create-item$" 'create-item)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/save-channel$" 'save-channel)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/createchannel$" 'create-channel)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/create-user$" 'create-user)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/save-user$" 'save-user)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/rss$" 'rss)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-static-file-dispatcher-and-handler "/style.css"
							     "/home/robojamie/Dropbox/LispBlog/style.css")
      hunchentoot:*dispatch-table*)


; Classes for content and users

(defclass channel ()
  ((title 
	    :reader title
	    :initarg :title)
   (link 
    :reader link
    :initarg :link)
   (description
    :reader description
    :initarg :description)))

(defclass user ()
  ((username 
    :reader username
    :initarg :username)
   (password
    :reader password
    :initarg :password)))

(defclass item ()
  ((title 
    :reader title
    :initarg :title)
   (link 
    :reader link
    :initarg :link)
   (description 
    :reader description
    :initarg :description)
   (author 
    :reader author
    :initarg :author)
   (comments
    :reader comments
    :initarg :comments)
   (guid 
    :reader guid
    :initarg :guid)
   (pubdate 
    :reader pubdate
    :initform (get-universal-time))
   (source
    :reader source
    :initarg :source)
   (enclosure
    :reader enclosure
    :initarg :enclosure)))


(defvar *blogposts* nil)

(defun make-blog-post (title link description pubdate)
  (list :title title :link link :description description :pubdate pubdate))

(defun add-blog-post (post) (push post *blogposts*))

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

 