(= chats (table))
(= users (table))

(def new-user()
     (= key (rand-string 10))
     
     (= (users key) (list 0 0 "noroom"))
     key)

(def new-chat(user1 user2)
     (= key (rand-string 10))
     (= ((users user1) 0) 1)
     (= ((users user1) 2) key)
     
     (= ((users user2) 0) 1)
     (= ((users user2) 2) key)
     (= (chats key) (list user1 user2 "")))

(def find-partner(user1)
      (random-elt (trues (fn(k) (if (~is user1 k) k)) ( map (fn(k) (> 1 ((users k) 0)) k) (keys users)))))

     
(def create-room(user1)
     (= user2 (find-partner user1))
     (if user2 (new-chat user1 user2) (pr "Looking for user")))

(def send-msg(user msg)
     (= chat ((users user) 2))
     (= ((chats chat) 2) (string ((chats chat) 2)  "<br>" msg)))


(defop newuser req (pr (new-user)))
(defop go req (gofunc(arg req "key")))
(defop newmsg req (send-msg (arg req "key") (arg req "txt")))
(def gofunc(key)

     (= u (users key))
     
     (= state (u 0))
     (= roomkey (u 2))

     
    (if (is state 1)	 
	 (prn ((chats roomkey) 2))
         (if (~create-room key) (prn "looking for user"))))   