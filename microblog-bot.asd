(asdf:defsystem #:microblog-bot
  :depends-on (#:cl-twit)
  :components ((:file "package")
	       (:file "microblog-bot")))
