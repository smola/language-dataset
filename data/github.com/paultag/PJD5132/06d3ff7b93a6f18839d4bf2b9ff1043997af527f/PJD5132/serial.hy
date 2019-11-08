
(defn read-length [serial length]
  (setv buf (serial.read length))
  (if (= (len buf) length)
    buf
    (raise (ValueError (.format "Short read: Expected {} got {}" length (len buf))))))


(defn read-response/raw [serial]
  (let [flavor   (octet (read-length serial 1) false)
        protocol (octet (read-length serial 2) false)
        length   (octet (read-length serial 2) false)
        payload         (read-length serial length)
        checksum        (read-length serial 1)]

    (if (!= protocol 20) (raise (ValueError
        (.format "Bad protocol version: Protocol read as: {}" protocol))))

    (if (!= (len payload) 0) payload nil)))


(defn octet [data big]
  (setv ret 0)
  (if big (setv data (reversed data)))
  (for [(, step num) (enumerate data)]
    (setv ret (+ ret (<< num (* step 8)))))
  ret)
