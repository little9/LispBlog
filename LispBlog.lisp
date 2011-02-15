(require 'hunchentoot)
(require 'cl-who)

(defvar *server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242)))
(defvar *channels* nil)
(defvar *items* nil)
(defvar *users* nil)

(defun blog-home () 
  (cl-who:with-html-output-to-string (str nil :prologue t :indent t)
    (:html
     (:head (:title "A Blog in Lisp!")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "/style.css"))
     (:body 
      (:h1 "A Blog in Common Lisp")
        (:a :href "/create-item" "Add new Post")
      (dolist (I *items*)
	(cl-who:htm (:p
		     (:h2
		      (:a :href (link I) (cl-who:str (title I))))
		    (:p "Date: "
		     (cl-who:str (pubdate I)))
		    (:p
		     (cl-who:str (description I))))))))))




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


(defun save-channel ()
  (let ((title (hunchentoot:parameter "title"))
	(link (hunchentoot:parameter "link"))
	(description (hunchentoot:parameter "description"))
)

       (push (make-instance 'channel :title title :link link :description description)
	     *channels*)      
       (hunchentoot:redirect "/")))


(defun save-item ()
  (let ((title (hunchentoot:parameter "title"))
	(link (hunchentoot:parameter "link"))
	(description (hunchentoot:parameter "description"))
	(author (hunchentoot:parameter "author")))

       (push (make-instance 'item :title title :link link :description description :author author)
	     *items*)      
       (hunchentoot:redirect "/")))

(defun save-user ()
  (let ((username (hunchentoot:parameter "username"))
	(password (hunchentoot:parameter "password")))

       (push (make-instance 'user :username username :password password)
	     *users*)      
       (hunchentoot:redirect "/")))


(push (hunchentoot:create-regex-dispatcher "^/$" 'blog-home)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/save-item$" 'save-item)
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


(push (hunchentoot:create-static-file-dispatcher-and-handler "/style.css"
							     "/home/robojamie/Dropbox/style.css")
      hunchentoot:*dispatch-table*)




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











 