;;;; このファイルをeuslisp側で読み込み、この関数をpython側から使おうとする。

(provide :test)

(unless (find-package "TEST") (make-package "TEST"))
(in-package  "TEST")


;;; グローバル変数アクセスのテスト

(defparameter *my-global-str* "this is my global variable")
(defvar const-pi 3.1415)


;;; 関数アクセスのテスト

; 引数なし
(defun no-arg-ret-nil ()
  nil)

(defun no-arg-ret-int()
  1)

(defun no-arg-ret-float()
  1.0)

(defun no-arg-ret-str()
  "returned string")

(defun no-arg-access-global()
  *my-global-str*)

(defun no-arg-ret-list()
  (let ((x '(1 2 3 4)))
    x))

(defun ret-list-with-nil()
  (let ((x '(1 (2 (3 nil)))))
    x))
  
(defun no-arg-ret-int-vec()
  (let ((x #i(1 2 3)))
    x))
  
(defun no-arg-ret-float-vec()
  (let ((x #f(1.0 2.0 3.0)))
    x))

(defun no-arg-ret-bit-vec()
  (let ((x (instantiate bit-vector 4)))
    x))    ; #*0000
  
(defun no-arg-ret-mat()
  (let ((x #2f((1.0 2.0 3.0) (4.0 5.0 6.0))))
    x))

(defun no-arg-ret-arr()
  (let ((x #3a(((1 2) ("a" "b")) ((3 4) ("c" "d"))) ))
    x))

(defun no-arg-ret-empty-hash ()
  (let ((m (make-hash-table)))
    m))

(defun no-arg-ret-a-1-hash ()
  (let ((m (make-hash-table)))
    (setf (gethash "a" m) 1)
    m))

; 1引数
(defun double(num)
  (* num 2))

(defun list-arg-ret-doubled-list(lst)
  (let ((tmp (mapcar #'(lambda (x) (* x 2)) lst)))
    tmp))

(defun original-nest (lst)
  "(1 2 3 4) -> (1 (2 3 4))ってだけ"
  (list (car lst) (cdr lst)))

(defun str-arg-ret-0 (str)
  (format nil "~c" (char str 0)))

; 複数引数
(defun three-args-ret-sum(a b c)
  (+ a b c))

(defun num-list-arg-ret-sum(num list)
  (let ((ans num))
    (dolist (i list)
      (setq ans (+ ans i)))
  ans))

(defun two-list-arg-ret-append(list1 list2)
  (append list1 list2))
 

; キーワード引数
(defun keyword-args-ret-int(&key a b c)
  (+ (* 3 a) (* 2 b) c))


; 複数引数とキーワード引数の合わせ技
(defun int-list-keyword-args-ret-int (num list &key a b c)
  (let ((ans num))
    (dolist (i list)
      (setq ans (+ ans i)))
    (+ ans (* 3 a) (* 2 b) c)))


;;; クラスアクセスのテスト 
(defclass person :super object :slots (name age))
(defmethod person
  (:print-hi ()
    (format nil "Hi,~A" name))
  (:get-name ()
    name)
  (:get-age ()
    age)
  (:set-name (str)
    (setq name str))
  (:set-age (num)
    (setq age num))
  (:incf-age ()
    (incf age))
  (:list-name-age ()
    (list name age)))




;;; 明確な型指定をpython側から行えないとパスできない関数たちのテスト
(defun dot-cons-p (cons)
  (listp (cdr cons)))

(defun access-symbol (sym)
  ; 'const-piなどの引数で呼ばれる
  (eval sym))

(defun 2d-array-arg-ret-0-0 (arr)
  ; 2次元arrayの0, 0成分を返す
   (aref arr 0 0))

(defun vector-arg-ret-0 (vec)
  (svref vec 0))

(defun int-vec-arg-ret-0 (iv)
  (if (integer-vector-p iv)
      (elt iv 0)))

(defun float-vec-arg-ret-0 (fv)
  (if (float-vector-p fv)
      (elt fv 0)))

(defun bit-vec-arg-ret-0 (bv)
  (if (bit-vector-p bv)
      (bit bv 0)))

(defun pathname-arg-ret-file-str (path)
  (if (pathnamep path)
      (pathname-name path)))


;;; socketが遅い理由をチェックするための調査関数
;;; 4092バイトの文字列を返す。python側からはまず4バイトの数値でデータサイズを受取り、次に4092バイト以上の任意のサイズを読み込もうとすると、4096-4=4092バイト読みこんでおしまいになるため、
;;; 1回の読み込みで必ず終了する。もし毎回4096バイトずつ読み込んでおり、読み込み待ちタイムアウトなどが悪さをしている場合これは高速になるはず。
;;; eval_foreign_vm_copyを使用することを考える。文字列で返すと両端に'がつくのでその2バイト文を引いた系4090バイトの文字列を生成しておく。
(setq str-4090 (instantiate string 4090))
(dotimes (i (length str-4090))
  (setf (elt str-4090 i) 97))    ;; これで"a"になる

(defun socket-check ()
  str-4090)


(in-package "USER")