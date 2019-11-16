#|
ここを臨機応変にいじろう!
|#
;;;; デフォルト値は0。このファイルの関数のユニットテストを行う場合はここを1にする。その場合、call-python.soなどは呼ばれない。
(defvar *unit-test-mode* 0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *error-handler*を上書き ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; my-eval, my-eval-with-proxyでの作業中にeuslispでエラーが起きたら捻り潰してerror:なる文字列を返せるようになっている。
;;; スレッド並行処理とか入れ始めたときに環境のエラーハンドラが書き換わっちゃってまずそうなので別々に登録できるinstall-error-handler関数を使用しようと思ったがマルチスレッドeuslispにしかないっぽいのでオーバーライド
;;; (eus.c見るにsolaris2だとthread.hがincludeされる?)
(defun original-error-handler (code msg1 form &optional (msg2))
  "original error handling function that throws 'euslispside-error"
  (terpri)
  (throw 'euslispside-error (python-like-string-from-string (format nil "error code: ~A (~A). ~A in s-expression ~A" code msg1 msg2 form))))



;;; このファイルのコード自体を開発しているときはコメントアウトしたほうがいい(謎の無限ループに陥る)
;; (setq *error-handler* #'original-error-handler)




;;; 文字コード値を文字列へ変換するtrans関数
;;; Calledbyc内部だけではなく、地味にpython-like-string-from-string関数内でも使用されている
(defun trans (val)
  "translate a character code (integer) to a character"
  (format nil "~c" val))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string copyのための関数たち。将来的にはProxy objectを作った時でもPython側で__str__(), __repr__()を読んだ時にこの関数を呼べるようにしておくと良さそう。;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; euslispのリスト, コンスを受け取り、pythonのリストとして評価できるように成形された文字列を生成して返す。コンスの場合は最後のセルのtailの値も拾われる。
;;; リスト, コンスの要素は任意で、その型に応じた変換が再帰的にそれぞれ成される。
;;; (python-like-string-from-seq '(1 2 3 4))  -> "[1,2,3,4]"
;;; (python-like-string-from-list '((1 2) (3 4 (5))) -> "[[1,2],[3,4,[5]]]"
;;; (python-like-string-from-list '(1 . 2)) -> "[1,2]"
(defun python-like-string-from-list (lst)
  "build python-like string notation of a list from a list object in euslisp"
  (if (listp lst)
      (with-output-to-string (str)
        (princ "[" str)
        (let ((len (list-length lst)))
          (dotimes (i len)
            (let ((elm (elt lst i)))
              (princ (python-like-string-from-obj elm) str)
              (if (not (= i (1- len)))
                  (princ "," str)
                  (if (cdr (last lst)) (progn (princ "," str) (princ (python-like-string-from-obj (cdr (last lst))) str))))))
          )        
        (princ "]" str))
      (warn "error: the argument of python-like-string-from-lst should be a list. received: ~A~%" lst)))


;;; euslispの文字列を受け取り、pythonの文字列っぽく表現した文字列を生成して返す
;;; (python-like-string-from-string "hoge") -> "'hoge'" 
;;; (python-like-string-from-string "3") -> "'3'"
;;; (python-like-string-from-string "hoge\"pi\"yo") -> "'hoge\\"pi\\"yo'" 
;;; (python-like-string-from-string "koko'ta'ro") -> "koko\'ta\'ro"
;;; Python側でevalした際シンボルとしてではなく文字列自身として評価してもらうためにクオートでのネストとエスケープを行う必要がある。
;;; 上記の例ではリテラルで各都合上左の関数の引数に \ が存在するが文字列として処理するときにはもちろん存在しないことに留意。
(defun python-like-string-from-string (arg)
  "build the pyhon-like string notation of the string from a string object in euslisp"
  (if (stringp arg)
      (with-output-to-string (str)
        (princ "'" str)
        (if (or (substringp "\"" arg) (substringp "'" arg))
            (dotimes (i (length arg))
              (cond ((= (elt arg i) 34) (progn (princ "\\" str) (princ "\"" str)))
                    ((= (elt arg i) 39) (progn (princ "\\" str) (princ "'" str)))
                    (t (princ (trans (elt arg i)) str))))
            (princ arg str))
        (princ "'" str))
      (warn "error: the argument of python-like-string-from-string must be string. received: ~A" arg)))


;;; euslispの2階実数値行列を受け取り、pythonのリストとして評価できるように整形された文字列を生成して返す
;;; (python-like-string-from-matrix #2f((1.0 2.0 3.0) (4.0 5.0 6.0))) -> "[[1.0,2.0,3.0],[4.0,5.0,6,0]]"
(defun python-like-string-from-matrix (mat)
  "build the python-style string notation of the 2-d list from a matrix object in euslisp"
  (if (matrixp mat)
      (let ((r (array-dimension mat 0)) str)
        (with-output-to-string (str)
          (princ "[" str)
          (dotimes (i r)
            (princ (python-like-string-from-vector (matrix-row mat i)) str)
            (if (not (= i (1- r))) (princ "," str)))
          (princ "]" str)))
      (warn "error: the argument of python-like-string-from-matrix must be a matrix. received: ~A~%" mat)))


;;; euslispのベクトル(文字列を除く。1階整数ベクトル、1階実数ベクトル、1階ビットベクトル、1階一般化ベクトル、ユーザー定義ベクトル)を受け取り、pythonのリストとして評価できるように成形された文字列を生成して返す
;;; (python-like-string-from-seq #i(1 2 3 4)) -> "[1,2,3,4]"
(defun python-like-string-from-vector (vec)
  "build python-like string notation of a list from a vector object in euslisp"
  (if (vectorp vec)
      (with-output-to-string (str)
        (princ "[" str)
        (let ((len (length vec)))
          (dotimes (i len)
            (let ((elm (cond ((or (integer-vector-p vec) (float-vector-p vec)) (aref vec i))    ; 整数ベクトル、実数ベクトル
                             ((bit-vector-p vec) (bit vec i))   ; ビットベクトル
                             ((eq (send (class vec) :name) 'vector) (svref vec i))    ; 一般化ベクトル(1階だが、それらの要素がリストであることもあるし、それぞれの型が異なることもある。)
                             (t (aref vec i)))))    ; ユーザー定義ベクトル 
              (princ (python-like-string-from-obj elm) str)
              (if (not (= i (1- len))) (princ "," str))))
          )
        (princ "]" str))
      (warn "error: the argument of python-like-string-from-vector should be a vector. received: ~A~%" vec)))


;;; euslispの任意階配列(うちvector, matrixを除く)受け取り、pythonのリストとして評価できるように成形された文字列を生成して返す
;;; (python-like-string-from-obj #2a((1 2) (3 4))) -> "[[1,2],[3,4]]"
(defun python-like-string-from-array (arr)
  "build python-like string notation of a list from an array object in euslisp"
  (if (and (arrayp arr) (not (matrixp arr)) (not (vectorp arr)))
      (with-output-to-string (str)
        (let ((rank (array-rank arr)) (dim-list (array-dimensions arr)))
          (array-handling str arr rank dim-list)))
      (warn t "error: the argument of python-like-string-from-array should be an array and should not be a matrix or a vector. received: ~A~%")))


(defun array-handling (str arr rank dim-list &optional (lst nil))
  "this function is called inside the python-like-string-from-array function. str: string stream, arr: array, rank(int): array-rank, dim-list(list):dimension list"
  (if (null dim-list)
      (let ((prefix (cons arr (reverse lst))))
        (princ (python-like-string-from-obj (apply #'aref prefix)) str))
      (progn
        (let ((elm-num (car dim-list)) (res (cdr dim-list))) 
          (princ "[" str)
          (dotimes (i elm-num)
            (unless (= i 0) (princ "," str))
            (setq lst (cons i lst))
            (array-handling str arr rank res lst)
            (setq lst (cdr lst)))
          (princ "]" str)))))


;;; euslispのハッシュテーブルを受け取り、pythonの辞書っぽく表現した文字列を生成して返す
;;; (python-like-string-from-hash (let ((hsh (make-hash-table))) (progn (setf (gethash "a" hsh) 10) (setf (gethash "b" hsh) 20)) hsh)) -> "{a:10,b:20}"
(defun python-like-string-from-hash (hashtable)
  "build the python-style string notation of the dictionary from a hash-table object in euslisp"
  (if (hash-table-p hashtable)
      (with-output-to-string (str)
        (princ "{" str)
        (let ((tmp "")) 
          (maphash #'(lambda (key value) (setq tmp (concatenate string (format nil "'~A':~A," key value) tmp))) hashtable)    ; strはストリームなので","を末尾からstripした文字列をsetすることはできない。一旦tmpにためておく。
          (princ tmp str))
        (princ "}" str))
      (warn "error: the argument of python-like-string-from-hash should be a hash table. received: ~A" hashtable)))


;;; euslispのパス名オブジェクトを受け取り、文字列化して返す
(defun python-like-string-from-path (path)
  (if (pathnamep path)
      (python-like-string-from-string (namestring path))
      (warn "error: the argument of python-like-string-from-path should be a pathname object. received: ~A" path)))


;;; euslisp側で変換をできるだけやる場合の実装
;;; integer, float → 数値の文字列表記に
;;; null → "None"に ("'None'"ではない)
;;; t → "True"に ("'True'"ではない)
;;; vector/string → 文字列に
;;; matrix(2「階」の配列で要素は実数) → 二次元リストの文字列表記に
;;; ユーザー定義ベクトル, vector, vector/float-vector, vector/integer-vector, vector/bit-vector, array(任意「階」の配列。テンソルなど), cons, list → 適切な次元のリストの文字列表記に
;;; hash-table → 辞書の文字列表記に
;;; pathnameオブジェクト → 文字列表記に
;;; そのほか → 文字列表現に。その他のユーザー定義オブジェクトとかは"'#<testclass #X55be17517c38>'"といった文字列がそのまま送られることになる。
(defun python-like-string-from-obj (obj)
  "build the python-like srting notation from an object in euslisp"
  (let ((str))
    (cond ((or (integerp obj) (floatp obj)) (setq str (prin1-to-string obj)))
          ((null obj) (setq str "None"))
          ((eq t obj) (setq str "True"))
          ((listp obj) (setq str (python-like-string-from-list obj)))
          ((stringp obj) (setq str (python-like-string-from-string obj)))
          ((matrixp obj) (setq str (python-like-string-from-matrix obj)))
          ((vectorp obj) (setq str (python-like-string-from-vector obj)))
          ((arrayp obj) (setq str (python-like-string-from-array obj)))
          ((hash-table-p obj) (setq str (python-like-string-from-hash obj)))
          ((pathnamep obj) (setq str (python-like-string-from-path obj)))
          (t (setq str (python-like-string-from-string (prin1-to-string obj)))))
    str))


;;; S式(S式の文字列表記ではない)を受取り、評価し、結果を文字列にして返す。
(defun my-eval (sformula)
  "evaluate the s-expression and then returns the string notation"
  ;; errorが生じたらこのeuslispside-errorでキャッチできるようにする。エラー時の返り値には"'error code: ...'"が入る。call stackをターミナルに吐き出してしまうのは今の所仕様。
  (let ((result (catch 'euslispside-error (python-like-string-from-obj (eval sformula)))))
    result))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Proxy objectを返すようなモードのための関数、クラスたち ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FFIで呼び出された戻り値を束縛しておく用のシンボルテーブルを管理するためのクラス
(defclass ffi-symbols-manager :super object :slots (ffi-symbol-table symbol-count))
(defmethod ffi-symbols-manager 
  (:set-symbol (obj)
    (progn
      (incf symbol-count)
      (setf (gethash symbol-count ffi-symbol-table) obj)
      symbol-count))
  (:get-symbol-value (key-num)
    (gethash key-num ffi-symbol-table))
  (:get-symbol-table ()
    ffi-symbol-table)
  (:get-symbol-count ()
    symbol-count)
  (:delete-symbol-pair (key-num)
    (remhash key-num ffi-symbol-table))
)

;;; ffi-symbol-tableを要素0のハッシュテーブルで, symbol-countを0で初期化。
(defvar *ffi-symbols* (make-instance ffi-symbols-manager :ffi-symbol-table (make-hash-table) :symbol-count 0))


;;; この関数を使用するようなS式がPythonから送られてくる。プロキシオブジェクトを作成させる際シンボルテーブルに登録した番号があるのでその値でもってテーブルを引く。その結果オブジェクトが手に入る。
(defun lookup-registered-object (key-num)
  "look up *ffi-symbols* table by key-num"
  (send *ffi-symbols* :get-symbol-value key-num))



;;; とりあえずグローバルな*ffi-symbols*のslotの辞書に登録をし、(鍵の数値[int] メソッドを列挙したリスト)を作成する。それをpythonで[key-num,pkg-name,cls-name]という形で受け取れるよう文字列に変換して返す。
(defun make-proxy (obj)
  "If the arugument is a number nor null, the function register the received argument to the symbol-table and returns python-style string of the list that contains key number, package name and class name.
   Otherwise, returns the normal string notation."
  (cond ((or (integerp obj) (floatp obj)) (format nil "~A" obj))
        ((null obj) "None")
        ((eq t obj) "True")
        (t (let ((key-num (send *ffi-symbols* :set-symbol obj))
                 (pkg-name (send *package* :name))
                 (cls-name (send (class obj) :name)))
             (python-like-string-from-string (concatenate string "proxy: " (python-like-string-from-list (list key-num pkg-name cls-name))))))))


;;; S式(S式の文字列表記ではない)を受け取り評価し、結果が値,nullの場合は文字列表記を、その他の場合は結果をシンボルにバインドしてプロキシオブジェクトをPython側で作らせるような命令文字列を返す。
;;; 「エラーが起きなければ」 "'proxy:[key-num,pkg-name,cls-name,]'"なる文字列が返る。
;;; エラー時には"'error code: ...'なる文字列が返る。
;;; モード(*string-copy-mode*の値)によってはCalledbyc内で呼ばれる。
(defun my-eval-with-proxy (sformula)
  "evaluate the s-expression and then returns the string notation using make-proxy function"
  (let ((result (catch 'euslispside-error (make-proxy (eval sformula)))))
  ;; (format t "sformula: ~A~%" sformula)
  ;; (format t "obj: ~A~%" (eval (read-from-string sformula)))
  ;; (format t "return from my-eval-with-proxy ~A~%" result)
  result))




;;;;;;;;;;;;;;;;;;;;
;;; メインの処理 ;;;
;;;;;;;;;;;;;;;;;;;;


;;; error hanclerを戻すかどうかは考えておく
(if (= *unit-test-mode* 0)
    (progn
      (format t "(euslisp)> Hello, euslisp.~%") 
      (setq saddr (make-socket-address :domain af_inet :host (second (unix:uname)) :port 50000))
      (setq sport (make-socket-port saddr))
      (format t  "(euslisp)> Waiting for connections...~%")
      (setq sstream (make-server-socket-stream sport 4096))    ;; このiszeがバッファサイズかは不明
      (format t "(euslisp)> Connected.~%")
      (loop
        (let (mode length result-string)
          (setq mode (read sstream))
          (setq sformula (read sstream))
          ;; (format t "(euslisp)> Recieved mode -> ~s~%" mode)    ; modeにはアルファベット一文字(S式そのもの)が入る
          ;; (format t "(euslisp)> Recieved s-expression -> ~s~%" sformula)    ; sには(S式の文字列表記ではなく)コンス(S式そのもの)が入る

          (cond ((string= mode "n") (setq result-string (prin1-to-string (eval sformula))))    ; デバッグ用。Pythonで評価できる形ではない、ただの文字列表記("(1 2 2)"とかが返る)
                ((string= mode "c") (setq result-string (my-eval sformula)))
                ((string= mode "p") (setq result-string (my-eval-with-proxy sformula)))
                (t (warn "mode should be 'n' or 'c' or 'p'. got ~s" mode)))
          ;; (format t "(euslisp)> return length: ~s~%" (length result-string))
          ;; (format t "(euslisp)> return: ~s~%" result-string)
          (write-long (length result-string) sstream)    ; データ長をバイトで送りつける
          (princ result-string sstream))    ; 続いて文字列表記を送りつける("は含まれない)
          (finish-output sstream)
      )
    )
)
