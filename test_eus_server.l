#|
ここを臨機応変にいじろう!
|#
;;; 0 → errorのみをキャッチする, 1 → passしたケースも全て出力していく。
(defvar *debug-mode* 1)


;;; testが通っていないケースをカウント
(defvar *error-count* 0)
;;; debug-modeが1の時はここにカウント結果が溜まっていき最後に出力される。
(defvar *passed-count* 0)

;;; assertion errorとなったテスト番号が追加されていく
(defparameter *error-test-num* nil)

;;; vがtであるか判定。nilならErrorを出力する。
(defun assert-true (v &optional msg)
    (if (null v)
        (progn
            (format t "Error ~a occurred when t-checking ~a" (if (null msg) "" msg) v)
            (terpri)
            (incf *error-count*)
            (setq *error-test-num* (cons (+ *error-count* *passed-count*) *error-test-num*)))
        (if (= *debug-mode* 1) 
            (progn
              (format t "OK! (t expected. got ~a)" v)
              (terpri)
              (incf *passed-count*)))
    ))

;;; v1とv2が構造的に等しいか判定。nilならErrorを出力する。
(defun assert-equal (v1 v2 &optional msg)
  (if (equal v1 v2)
      (if (= *debug-mode* 1)
          (progn
            (format t "OK! (~a expected. got ~a)" v2 v1)
            (terpri)
            (incf *passed-count*)))
      (progn (format t "Error: ~a" (if (null msg) "" msg))
              (terpri)
              (format t "got---~%")
              (print v1)
              (print v2)
              (format t "expected---~%")
              (incf *error-count*)
              (setq *error-test-num* (cons (+ *error-count* *passed-count*))))))


;;; vが"'some text here'"という形の文字列になっているか判定。nilならErrorを出力する。
(defun assert-nest-str (v &optional msg)
  (let ((last-ind (1- (length v))))
    (if (and (stringp v)    ; そもそも文字列表記になったか
             (string= v "'" :start1 0 :end1 0 :start2 0 :end2 0)    ; 始まりは'か
             (string= v "'" :start1 last-ind :end1 last-ind :start2 0 :end2 0))    ; 終わりは'か
        (if (= *debug-mode* 1)
          (progn
            (format t "OK! (nest-str expected. got ~a)" v)
            (terpri)
            (incf *passed-count*)))
        (progn
          (format t "Error ~a occurred when nest-str-checking ~a" (if (null msg) "" msg) v)
          (terpri)
          (incf *error-count*)))))




;;; ------------------------
;;; 以降にテストを書いていく
;;; ------------------------

(load "eus_server.l")
(terpri)
(print "start eus_server.l test...!")

;; trans関数のテスト
(format t "# test trans function.~%")
(assert-equal (trans 34) "\"")
(assert-equal (trans 35) "#")
(assert-equal (trans 39) "'")
(assert-equal (trans 40) "(")
(assert-equal (trans 41) ")")
(dotimes (i 10)
  (assert-equal (trans (+ i 48)) (string i)))
(assert-equal (trans 65) "A")
(assert-equal (trans 92) "\\")
(assert-equal (trans 97) "a")




;; python-like-string-from-obj関数のテスト
;; 内部でpython-like-string-from-string, python-like-string-from-hash, python-like-string-from-seq, python-like-string-from-matrix関数を呼び出しているので実質それらをテストしている
(terpri)
(format t "# test python-like-string-from-obj function.~%")


; integer, float
(assert-equal (python-like-string-from-obj 0) "0")
(assert-equal (python-like-string-from-obj 10) "10")
(assert-equal (python-like-string-from-obj #b1110) "14")    ; 2進数表記のリーダマクロ
(assert-equal (python-like-string-from-obj #xFF) "255")    ; 16進数表記のリーダマクロ
(assert-equal (python-like-string-from-obj 0.0) "0.0")
(assert-equal (python-like-string-from-obj 10.0) "10.0")
(assert-equal (python-like-string-from-obj #d0) "0.0")    ; 度数法から弧度法へ
(assert-equal (python-like-string-from-obj #r0) "0.0")    ; 弧度法から度数法へ


; nil
(assert-equal (python-like-string-from-obj nil) "None")

; t
(assert-equal (python-like-string-from-obj t) "True")

; string
(assert-equal (python-like-string-from-obj "hoge") "'hoge'")
(assert-equal (python-like-string-from-obj "nil") "'nil'")    ; nilなんていう文字列だった時
(assert-equal (python-like-string-from-obj "None") "'None'")    ; Noneなんていう文字列だった時
(assert-equal (python-like-string-from-obj "3") "'3'")
(assert-equal (python-like-string-from-obj "") "''")
(assert-equal (python-like-string-from-obj "hoge\"piyo") "'hoge\\\"piyo'")    ; 右の文字列はeuslispのリテラル表現を行うためにbackslashましましになっている。ここでeuslispが処理するエスケープシーケンスを除けば "'hoge\"piyo'" となる
(assert-equal (python-like-string-from-obj "hoge\"pi\"yo") "'hoge\\\"pi\\\"yo'")
(assert-equal (python-like-string-from-obj "koko'taro") "'koko\\'taro'")
(assert-equal (python-like-string-from-obj "koko'ta'ro") "'koko\\'ta\\'ro'")
(assert-equal (python-like-string-from-obj "hoge\"piyo'koko") "'hoge\\\"piyo\\'koko'")


; hash-table
(assert-equal (python-like-string-from-obj (let ((hsh (make-hash-table))) (progn (setf (gethash "a" hsh) 10) (setf (gethash "b" hsh) 20)) hsh)) "{'b':20,'a':10,}")    ; 順序は保証されていないはず。,が最後に残るのは仕様(evalすれば自然と無視されるので) 
(assert-equal (python-like-string-from-obj (let ((hsh (make-hash-table))) hsh)) "{}")


;integer-vector, float-vector, bit-vector
(assert-equal (python-like-string-from-obj #i(1)) "[1]")
(assert-equal (python-like-string-from-obj #i(1 2 3 4)) "[1,2,3,4]")
(assert-equal (python-like-string-from-obj (make-array 3 :element-type :integer)) "[0,0,0]")    ; #i(0 0 0)
(assert-equal (python-like-string-from-obj (make-array 3 :initial-contents '(1 2 3) :element-type :integer)) "[1,2,3]")    ; #i(1 2 3)
(assert-equal (python-like-string-from-obj #f(1.0)) "[1.0]")
(assert-equal (python-like-string-from-obj #f(1.0 2.0 3.0 4.0)) "[1.0,2.0,3.0,4.0]")
(assert-equal (python-like-string-from-obj (make-array 3 :element-type :float)) "[0.0,0.0,0.0]")    ; #f(0.0 0.0 0.0)
(assert-equal (python-like-string-from-obj (make-array 3 :initial-contents '(1 2 3) :element-type :float)) "[1.0,2.0,3.0]")    ; #f(1.0 2.0 3.0)
(assert-equal (python-like-string-from-obj #*11110000) "[1,1,1,1,0,0,0,0]")
(assert-equal (python-like-string-from-obj (instantiate bit-vector 8)) "[0,0,0,0,0,0,0,0]")    ; #*00000000

; matrix (matrixpに引っかかるやつ。2階の実数テンソルをmatrixと定義しているっぽい)
(assert-equal (python-like-string-from-obj #2f((1.0 2.0) (3.0 4.0))) "[[1.0,2.0],[3.0,4.0]]")
(assert-equal (python-like-string-from-obj #2f((1.0 2.0 3.0) (4.0 5.0 6.0))) "[[1.0,2.0,3.0],[4.0,5.0,6.0]]")
(assert-equal (python-like-string-from-obj (make-matrix 3 3)) "[[0.0,0.0,0.0],[0.0,0.0,0.0],[0.0,0.0,0.0]]")    ; #2f((0.0 0.0 0.0) (0.0 0.0 0.0) (0.0 0.0 0.0))

; vector (vectorpに引っかかる奴から上記を除いたもの。あくまでも1階のベクトルのみ引っかかるためmatrixはvectorpを真としない。ユーザー定義ベクトルやただのベクトルはここに書いていく。)
(assert-equal (python-like-string-from-obj #((1.0 2.0 3.0) (4.0 5.0 6.0))) "[[1.0,2.0,3.0],[4.0,5.0,6.0]]")
(assert-equal (python-like-string-from-obj #((1) 2 3 "moo")) "[[1],2,3,'moo']")    ; リストやらを要素に持っているだけのただの1階のベクトル
(assert-equal (python-like-string-from-obj (vector 1 10 100)) "[1,10,100]")    ; #(1 10 100)

(defclass original-vector2 :super vector :element-type :integer :size 2)
(assert-equal (python-like-string-from-obj (instantiate original-vector2)) "[0,0]")





; array (arraypに引っかかる奴から上記を除いたもの。整数行列、ただの行列、高階テンソルなどはarraypでしか拾えない。))
(assert-equal (python-like-string-from-obj #2i((1 2) (3 4))) "[[1,2],[3,4]]")
(assert-equal (python-like-string-from-obj #2a((1 2) (3 4))) "[[1,2],[3,4]]")
(assert-equal (python-like-string-from-obj #3a(((1 2 3) (4 5 6)) ((7 8 9) ("this" "is a" "string")))) "[[[1,2,3],[4,5,6]],[[7,8,9],['this','is a','string']]]")
(assert-equal (python-like-string-from-obj (make-array '(2 2) :element-type :integer)) "[[0,0],[0,0]]")    ; #2a((0 0) (0 0))
(assert-equal (python-like-string-from-obj (make-array '(2 2 2))) "[[[None,None],[None,None]],[[None,None],[None,None]]]")    ; #3a(((nil nil) (nil nil)) ((nil) (nil)))


; mysterious object
(defclass person :super object :slots (age name))
(setq murabitoA (make-instance person :age 20 :name "kokotaro"))
(assert-nest-str (python-like-string-from-obj murabitoA))


; list
(assert-equal (python-like-string-from-obj '(1 2 3 4)) "[1,2,3,4]")
(assert-equal (python-like-string-from-obj '(1.0 2.0 3.0 4.0)) "[1.0,2.0,3.0,4.0]")
(assert-equal (python-like-string-from-obj '(1)) "[1]")
(assert-equal (python-like-string-from-obj '(1.0)) "[1.0]")
(assert-equal (python-like-string-from-obj '(1 (2 3) (4 (5 6)))) "[1,[2,3],[4,[5,6]]]")
(assert-equal (python-like-string-from-obj '(((1.0)))) "[[[1.0]]]")
(assert-equal (python-like-string-from-obj '(nil (nil) ((nil 1) 2))) "[None,[None],[[None,1],2]]")
(assert-equal (python-like-string-from-obj '(t (t) 1)) "[True,[True],1]")


; cons (nil以外はlistなのでlistpにひっかかる)
(assert-equal (python-like-string-from-obj '(1 . 2)) "[1,2]")
(assert-equal (python-like-string-from-obj (cons 1 2)) "[1,2]")


; pathname
(assert-equal (python-like-string-from-obj #p"/etc/opt/") "'/etc/opt/'")
(assert-equal (python-like-string-from-obj (pathname "~/Documents/")) "'/home/ikezaki/Documents/'")





;; my-eval関数のテスト
(terpri)
(format t "# test my-eval function.~%")
(assert-equal (my-eval (read-from-string "(+ 1 2)")) "3")
(assert-equal (my-eval (read-from-string "(- 1 2)")) "-1")
(assert-equal (my-eval (read-from-string "(* 1 2)")) "2")
(assert-equal (my-eval (read-from-string "(/ 6 2)")) "3")
(assert-equal (my-eval (read-from-string "100")) "100")
(assert-equal (my-eval (read-from-string "(quote hoge)")) "'hoge'")
(assert-equal (my-eval (read-from-string "(abs -10.0)")) "10.0")
(assert-equal (my-eval (read-from-string "(sqrt 81)")) "9.0")
(assert-equal (my-eval (read-from-string "(numberp 10)")) "True")
(assert-equal (my-eval (read-from-string "(symbolp 'var)")) "True")
(assert-equal (my-eval (read-from-string "(oddp 2)")) "None")
(assert-equal (my-eval (read-from-string "(< 2 3)")) "True")
(assert-equal (my-eval (read-from-string "#i(1 2 3)")) "[1,2,3]")
(assert-equal (my-eval (read-from-string "#f(1.0 2.0)")) "[1.0,2.0]")
(assert-equal (my-eval (read-from-string "(apply #'+ '(2 3))")) "5")
(assert-equal (my-eval (read-from-string "(car '(1 2 3))")) "1")
(assert-equal (my-eval (read-from-string "(list 1 2)")) "[1,2]")
(assert-equal (my-eval (read-from-string "(cons 1 '(10 100))")) "[1,10,100]")
(assert-equal (my-eval (read-from-string "(listp (list 'hoge 'piyo))")) "True")
(assert-equal (my-eval (read-from-string "(atom 1)")) "True")
(assert-equal (my-eval (read-from-string "(length '(1 2 3 4))")) "4")
(assert-equal (my-eval (read-from-string "(if (< 5 10) 'under-10 'equals-to-or-over-10)")) "'under-10'")

; Euslispでは(Common Lispと違って)lambda関数の前に#'とつけて関数と知らせる必要がある
(assert-equal (my-eval (read-from-string "(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5))")) "[1,4,9,16,25]")
(assert-equal (my-eval (read-from-string "(mapcan #'(lambda (x) (list (+ x 10) \"x\")) '(1 2 3 4 5))")) "[11,'x',12,'x',13,'x',14,'x',15,'x']")



;; ffi-symbols-managerクラスのテスト
(terpri)
(format t "# test ffi-symbols-manager class.~%")
(assert-equal (send *ffi-symbols* :get-symbol-count) 0)
(assert-true (hash-table-p (send *ffi-symbols* :get-symbol-table)))
(assert-equal (send *ffi-symbols* :get-symbol-value 0) nil)
(defclass intvec3 :super vector :element-type :integer :size 3)
(setq iv1 (instantiate intvec3))
(setq iv2 (instantiate intvec3))
(dotimes (i 3) (setf (aref iv2 i) i))
(assert-equal (send *ffi-symbols* :set-symbol iv1) 1)
(assert-equal (send *ffi-symbols* :get-symbol-value 1) iv1)
(assert-equal (send *ffi-symbols* :get-symbol-count) 1)
(assert-equal (send *ffi-symbols* :set-symbol iv2) 2)
(assert-equal (send *ffi-symbols* :get-symbol-value 2) iv2)
(assert-equal (send *ffi-symbols* :get-symbol-count) 2)
(assert-equal (send *ffi-symbols* :delete-symbol-pair 1) nil)    ; どうもEuslispの場合はハッシュ値が登録されて用途されてまいとnilを返すっぽい...
(assert-equal (send *ffi-symbols* :delete-symbol-pair 100) nil)
(assert-equal (send *ffi-symbols* :get-symbol-value 1) nil)
(assert-equal (send *ffi-symbols* :get-symbol-value 2) iv2)
(assert-equal (send *ffi-symbols* :get-symbol-count) 2)


;; lookup-registered-objectの簡易テスト
(terpri)
(format t "# test lookup-registered-object.~%")
(assert-equal (lookup-registered-object 1) nil)    ; 登録されていない or 削除されたオブジェクトエントリをひこうとしたとき
(assert-equal (lookup-registered-object 2) iv2)


;; 上記を受けてmake-proxy関数の簡易テスト
(terpri)
(format t "# test make-proxy function.~%")

; proxyを作らないようなものたち
(assert-equal (make-proxy 1) "1")
(assert-equal (make-proxy 1.0) "1.0")
(assert-equal (make-proxy nil) "None")
(assert-equal (make-proxy t) "True")
; proxyを作るようなものたち
(assert-equal (make-proxy #i(1 2 3)) (format nil "'proxy:[~A,\\\'USER\\\',\\\'integer-vector\\\']'" (send *ffi-symbols* :get-symbol-count)))
(assert-equal (make-proxy #f(1 2 3)) (format nil "'proxy:[~A,\\\'USER\\\',\\\'float-vector\\\']'" (send *ffi-symbols* :get-symbol-count)))
(assert-equal (make-proxy '(1 2 3)) (format nil "'proxy:[~A,\\\'USER\\\',\\\'cons\\\']'" (send *ffi-symbols* :get-symbol-count)))



;; callback-functionの処理用の関数の簡易テスト
(assert-equal (make-python-callback-body '(1 2 3)) " 1 2 3")
(assert-equal (make-python-callback-body '(t nil 1.0)) " True None 1.0")
(assert-equal (make-python-callback-body '(#i(1 2 3 4) 5 6)) (format nil " proxy:[~A,\\\'USER\\\',\\\'integer-vector\\\'] 5 6" (send *ffi-symbols* :get-symbol-count)))

(assert-equal (make-python-callback-command "foreign1" '(10 20)) "'callback:foreign1 10 20'")




;; error時の挙動の簡易テスト
(terpri)
(format t "# test error behavior.~%")

(if *original-error-mode*
    (progn
      (format t "### please ignore the error message below (call stack will be shown.) ###~%")

      ; ファイルを読み込もうとし他のに失敗した時
      (assert-equal (my-eval (read-from-string "(require (string 'InvalidFileName))")) "'error_code:76 (). file #P\\\"invalidfilename\\\" not found in s-expression (require (string \\'invalidfilename))'")

      ; 未定義の参照を行った時
      (assert-equal (my-eval (read-from-string "(non-existent-func 1 2)")) "'error_code:13 (undefined function). non-existent-func in s-expression (eval sformula)'")
      (assert-equal (my-eval (read-from-string "(+ non-existent-symbol 2)")) "'error_code:12 (unbound variable). non-existent-symbol in s-expression (+ non-existent-symbol 2)'")


      (format t "### error checking section finished. ###~%"))
    
    (format t "Default error handler is not overwritten. Skipped this test.~%"))



;;; -------------------------
;;; -------------------------
(terpri)
(format t "================================~%")
(if (> *error-count* 0)
    (progn 
      (format t "Failure: ~a errors in ~a~%" *error-count* *error-test-num*)
      (if (= *debug-mode* 1)
        (format t "passed: ~a test cases~%" *passed-count*)))
    (format t "* Euslisp server UnitTest OK *~%"))
(format t "================================~%")
