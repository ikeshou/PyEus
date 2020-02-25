#!/usr/bin/env python2
# coding: utf-8

"""
pyeusのための補助関数たち
"""
import re, sys, string
from enum import Enum


#################
# send_sexp関連 #
#################
def valid_sexp_predicate(sexp):
    """
    文字列中の"のエスケープ判定もしている。文字列リテラルを除外しsexp中に出現する()が全て関数コールとなるようにしてから、(と)の対応関係をチェックする。
    Args:
        sexp (str): euslispに送りつけるS式
    Returns:
        bool: S式が正しいかどうか
    
    >>> valid_sexp_predicate("(+ 1 2)")
    True

    >>> valid_sexp_predicate('(princ "hoge")')
    True

    >>> valid_sexp_predicate('(TEST::consult-hash-with-key (let ((hsh (make-hash-table))) (progn (setf (gethash "moo" hsh) 100) (setf (gethash "bow" hsh) 200)) hsh) "moo")')
    True

    >>> valid_sexp_predicate("(+ 1 2 3))")    # excess ')'
    False

    >>> valid_sexp_predicate("(list 1 2 (3)")    # insufficient ')
    False
  
    >>> sexp = r'(concatenate string "really\\" weird(" "string\\"!")'    # valid s-expression (エスケープ処理ができてないと'(concatenate string weird( string))'なる文字列が残されinvalidと判定されてしまう)
    >>> valid_sexp_predicate(sexp)
    True
    """
    sub = re.sub(r'(?<!\\)\".*?(?<!\\)\"', '', sexp)    # 否定的先読み(?<!)で「 \ が前に存在しない " 」のペアを最短マッチングで(.*?)発見して削除する
    counter = 0
    for char in sub:
        if char == '(':
            counter += 1
        elif char == ')':
            if counter > 0:
                counter -= 1 
            else:
                return False
    if counter > 0:
        return False
    return True



##########################
# エラーハンドリング関連 #
##########################
class EusError(Exception):
    """Euslisp側で生じたエラーを知らせるメッセージをソケットストリームから受け取ったとき、そのエラーをラッピングするだけのエラークラス"""
    pass


# # まだ使用していない。EusErrorを投げたほうが親切?
# EUSERROR_TO_PYTHON_ERROR = \
#     [RuntimeError,     #   "",				/*0*/
#      OverflowError,    # 	"stack overflow",		/*1 errcode=1..10 are fatal errors*/
#      MemoryError,      # 	"allocation",			/*2*/
#      RuntimeError,     # 	"",				/*3*/
#      RuntimeError,     # 	"",				/*4*/
#      RuntimeError,     # 	"",				/*5*/
#      RuntimeError,     # 	"",				/*6*/
#      RuntimeError,     # 	"",				/*7*/
#      RuntimeError,     # 	"",				/*8*/
#      RuntimeError,     # 	"",				/*9*/
#      RuntimeError,     # 	"",				/*10	end of fatal error*/
#      RuntimeError,     # 	"attempt to set to constant",	/*11 E_SETCONST */
#      NameError,        # 	"unbound variable",		/*12 E_UNBOUND  */
#      NameError,        # 	"undefined function",		/*13 E_UNDEF    */
#      TypeError,        # 	"mismatch argument",		/*14 E_MISMATCHARG */
#      SyntaxError,      # 	"illegal function",		/*15 E_ILLFUNC */
#      SyntaxError,      # 	"illegal character",		/*16 E_ILLCH */
#      SyntaxError,      # 	"illegal delimiter",		/*17 E_READ */
#      RuntimeError,     # 	"write?",			/*18 E_WRITE*/
#      RuntimeError,     # 	"too long string",		/*19 E_LONGSTRING */
#      TypeError,        # 	"symbol expected",
#      TypeError,        # 	"list expected",
#      SyntaxError,      # 	"illegal lambda form",
#      SyntaxError,      #  "illegal lambda parameter syntax",
#      RuntimeError,     # 	"no catcher found",
#      SyntaxError,      # 	"no such block",
#      TypeError,        # 	"stream expected",
#      SyntaxError,      # 	"illegal stream direction keyword",
#      TypeError,        # 	"integer expected",
#      TypeError,        # 	"string expected",
#      OSError,          # 	"error in open file",
#      EOFError,         # 	"EOF hit",
#      TypeError,        # 	"number expected",
#      OverflowError,    # 	"class table overflow",
#      TypeError,        # 	"class expected",
#      TypeError,        # 	"vector expected",
#      ValueError,       # 	"array size must be positive",
#      RuntimeError,     # 	"duplicated object variable name",
#      RuntimeError,     # 	"cannot make instance",
#      IndexError,       # 	"array index out of range",		/*  E_ARRAYINDEX */
#      AttributeError,   # 	"cannot find method",
#      RuntimeError,     # 	"circular list",    # 3.5以前はRecursionErrorがない
#      SyntaxError,      # 	"unknown sharp macro",
#      TypeError,        # 	"list expected for an element of an alist",
#      TypeError,        # 	"macro expected",
#      ImportError,      # 	"no such package",    # ModuleNotFoundErrorは3.6から
#      ImportError,      # 	"package name",
#      SyntaxError,      # 	"invalid lisp object form",
#      NameError,        # 	"no such object variable",
#      TypeError,        # 	"sequence expected",
#      IndexError,       # 	"illegal start/end index",
#      NameError,        # 	"no super class",
#      SyntaxError,      # 	"invalid format string",
#      TypeError,        # 	"float vector expected",
#      ValueError,       # 	"char code out of range",
#      ValueError,       # 	"vector dimension mismatch",
#      TypeError,        # 	"object expected",
#      TypeError,        # 	"type mismatch",
#      SyntaxError,      # 	"declaration is not allowed here",
#      SyntaxError,      # 	"illegal declaration form",
#      SyntaxError,      # 	"cannot be used for a variable",
#      SyntaxError,      # 	"illegal rotation axis",
#      RuntimeError,     # 	"multiple variable declaration",
#      SyntaxError,      # 	"illegal #n= or #n= label",
#      SyntaxError,      # 	"illegal #f( expression",
#      SyntaxError,      # 	"illegal #v or #j expression", 
#      socket.gaierror,  # 	"invalid socket address",
#      TypeError,        # 	"array expected",
#      ValueError,       # 	"array dimension mismatch",
#      TypeError,        # 	"keyword expected for arguments",
#      TypeError,        # 	"no such keyword",
#      TypeError,        # 	"integer vector expected",
#      IndexError,       # 	"sequence index out of range",
#      TypeError,        # 	"not a bit vector",
#      NameError,        # 	"no such external symbol",
#      RuntimeError,     # 	"symbol conflict",
#      RuntimeError,     # 	"",
#      RuntimeError,     # 	"E_END",
#     ]



###########################################################
#### 名前処理関連のEus_pkgのクラス内で使う補助関数たち ####
###########################################################
"""
Euslispのパッケージ関連の挙動の補足。
lispのリーダはシンボルの大文字、小文字を区別せず全て大文字として認識する。が文字列はもちろん区別する。

(make-package "HOGE")    ; <- ここを小文字で作ることはないはず。hoge::tmpやHOGE::tmpという形でアクセスできなくなるので(両方HOGE::tmpとしてリーダに読まれる)
(in-package "HOGE")
(setq tmp 10)
(in-package "USER")
hoge::tmp    -> 10
HOGE::tmp    -> 10

; 作り方には他にも
; (make-package 'piyo) (make-package :koko) などの方法がある。    <- こちらは 'PIYO や :TARO でもよい。

; 上記でどの方法で作ったやつも (in-package :hoge) や (in-package :HOGE) や (in-package 'hoge) や (in-package 'HOGE) という形でアクセスできる。なんなら(in-package ':hoge)もOK。
; "SYM" と 'sym と 'SYM と :sym と :SYM ':symは区別されていない。
"""

def pkg_uniform_upper(pkg):
    """
    パッケージ名から ' や : といったprefixを取り除き、大文字表記にした文字列を返す。パッケージ表記が単一になり比較処理などが行いやすくなる。
    Args:
        pkg (str)
    Returns:
        pkg (str)
    
    >>> pkg_uniform_upper("'test-package")
    'TEST-PACKAGE'

    >>> pkg_uniform_upper(":test-package")
    'TEST-PACKAGE'

    >>> pkg_uniform_upper("TEST-PACKAGE")
    'TEST-PACKAGE'
    """
    pkg = pkg.upper()
    if pkg.startswith("'") or pkg.startswith(":"):
        pkg = pkg[1:]
    return pkg


def full_qualified(pkg, symbols):
    """
    パッケージ名と、文字列表記されたシンボルのリスト(このEus_pkgクラスと対応するEuslispのパッケージ以外のシンボルが含まれていても良い)を受け取る。
    対応するパッケージ内のシンボルのみを取り出し、それぞれパッケージ接頭語(大文字)をつけ、リストにして返す。

    >>> full_qualified("TEST", ['var1', 'OTHER-PACK::var2', 'LISP::var3'])
    ['TEST::var1', 'LISP::var3']

    Args:
        pkg (str): パッケージ名。大文字表記でprefixなしのもの。
        symbols (list): 文字列表記されたシンボルのリスト。パッケージ名は小文字表記でも大文字表記で登録されていても良い。(['test::tmp']でも、['TEST::tmp']でも可)
    Returns:
        result (list): 文字列表記されたシンボルのリスト。
    """
    result = []
    for symbol in symbols:
        if '::' in symbol:
            pkg_of_symbol , suffix_of_symbol = symbol.split('::')    # ここは'user'や'test'といった小文字表記で返ってくることがほとんど
            pkg_of_symbol = pkg_of_symbol.upper()
            symbol = pkg_of_symbol + '::' + suffix_of_symbol
        elif ':' in symbol:
            pkg_of_symbol , suffix_of_symbol = symbol.split(':')    # ここは'compiler'といった小文字表記がほとんどだろう。compiler:identifierはCOMPILER:identifierなるシンボルになる
            pkg_of_symbol = pkg_of_symbol.upper()
            symbol = pkg_of_symbol + ':' + suffix_of_symbol                        
        else:
            pkg_of_symbol = pkg
            symbol = pkg_of_symbol + '::' + symbol
        
        # LISP packageの組み込み関数などはパッケージ接頭語なしに使えるようになっている。この特定のパッケージ内からもあたかもパッケージ内の関数かのように使えるようにする。
        if pkg_of_symbol == pkg or pkg_of_symbol == "LISP":
            result.append(symbol)    # cls_pkg(大文字) + '::' or ':' + cls_name
    return result


def change_valid_name(pkg, name, option="underscore"):
    """
    Euslispでの変数名や関数名は+-*/@$%^&=<>~.を使えるのでpythonの識別子ではこれらを変換する必要がある。また、パッケージ接頭語HOGE::やPIYO:を取り除く必要がある。
    optionはunderscoreの他にunicodeにする技も考えたが、python2ではunicodeは変数名に使えないのでサポートしない。


    >>> change_valid_name("TEST", "TEST::*hoge*")
    '_hoge_'

    >>> change_valid_name("RCB4LISP", "RCB4LISP::compile-code")
    'compile_code'

    >>> change_valid_name("TEST", "LISP::reverse")
    'reverse'

    >>> change_valid_name("TEST", "LISP::*eusdir*")
    '_eusdir_'

    >>> change_valid_name("pkg", "pkg::some-name", "non_existent_option")
    Traceback (most recent call last):
     ...
    TypeError: Currently, only 'underscore' is allowed as an option argument.


    Args:
        pkg (str): パッケージ名(大文字)
        name (str): eusの関数名や変数名 (qualifyされていることを仮定する。つまり大文字でpackage接頭語がついているHOGE::varなどを想定。)
        option (str): どのようにpythonでの禁止シンボルを変換するか。今の所_に変換するのみ
    Returns:
        s_new (str): 新たに作成された変換後の文字列
    Raises:
        TypeError: optionで"underscore"以外がよばれたとき
    """
    if name.startswith(pkg+"::") or name.startswith("LISP::"):
        name = name[len(pkg)+2:]
    if name.startswith(pkg+":") or name.startswith("LISP:"):
        name = name[len(pkg)+1:]
        
    invalid_char = '+-*/@$%^&=<>~.;'
    if option == "underscore":
        table = string.maketrans(invalid_char, '_'*len(invalid_char))    # 2系はstr.maketransではなくstring.maketrans, 辞書で与えることはできず変換前-後の２つの文字列を渡す
        s_new = name.translate(table)
        if s_new[0].isdigit():
            s_new = "_" + s_new[1:]
        return s_new
    else:
        raise TypeError("Currently, only 'underscore' is allowed as an option argument.")



#############
# for debug #
#############
def my_assert(actual, expected):
    """
    何番目のテストケースが通って何番目のテストケースが死んだのかを表示するデバッグ用関数
    """
    # static変数ライクに使うために関数オブジェクトの属性に登録してしまう
    if not hasattr(my_assert, 'count_iter'):
        my_assert.count_iter = 0
        my_assert.count_passed = 0
        my_assert.count_error = 0
        my_assert.error_list = []

    my_assert.count_iter += 1

    if actual == expected:
        print("*test{} passed.* (actual={}, expected={})\n".format(my_assert.count_iter, actual, expected))
        my_assert.count_passed += 1
    else:
        print >> sys.stderr, "***test{} failed!*** (actual={}, expected={})\n".format(my_assert.count_iter, actual, expected)
        my_assert.count_error += 1
        my_assert.error_list.append(my_assert.count_iter)



if __name__ == "__main__":
    import doctest
    doctest.testmod()

    # sexp = '(concatenate string "really\\" weird(" "string\\"!")'    # valid s-expression (エスケープ処理ができてないと'(concatenate string weird( string))'なる文字列が残されinvalidと判定されてしまう)
    # print(sexp)
    # for char in sexp:
    #     print(char)
    # print()
    # sub = re.sub(r'(?<!\\)\".*?(?<!\\)\"', '', sexp)
    # print(sub)
    # for char in sub:
    #     print(char)
    # print(valid_sexp_predicate(sexp))