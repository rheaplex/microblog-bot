(asdf:defsystem #:microblog-bot
  :depends-on (#:cl-twit)
  :components ((:file "microblog-bot")
	       (:file "package")))
