#!/usr/bin/env python2
# coding: utf-8

"""
ライブラリバージョン
"""
import socket, shlex, subprocess, time, math, struct
import sys, string, re


class EusError(Exception):
    pass


if __name__ == "pyeus":
    # ライブラリとして呼び出された場合ソケット接続を行いeus_server.lにS式を送れるようにする
    command = "irteus eus_server.l"
    eus_process = subprocess.Popen(shlex.split(command))    # shlex.split()なら複雑な状況(ファイル名にスペースとか)でも考慮してsplitできる

    host = socket.gethostname()    # Host name of the server here
    port = 50000    # Port number here


    def _connect_to_euslisp(host, port):
        failure_count = 0
        try:
            print "(python)> Trying to connect..."    
            client.connect((host, port))
            print "(python)> Connected."
        except socket.error as e:
            print "(python)> Failed to connect. Sleep for 1 sec."
            failure_count += 1
            time.sleep(1)
            if failure_count <= 10:
                _connect_to_euslisp(host, port)
            else:
                print "(python)> Failed in connecting more than 10 times. Give up connecting."


    client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)    # Create socket object (AF_INET -> IPv4 , SOCK_STREAM -> TCP)
    _connect_to_euslisp(host, port)    # Connect to the server


    def close_all_sockets():
        """ソケット切断関数"""
        client.close()


    def kill_eus_process():
        """フックしたEuslispプロセス終了関数"""
        eus_process.kill()



def send_sexp(sexp, mode='n'):
    try:
        assert mode in ('c', 'p', 'n')
        mode = '"' + mode + '" '    # 末尾にspaceを追加して"を追加する。Euslisp側から1つのS式、文字列としてreadされる
        client.send(mode)    # Send mode
        # print "(python)> send '%s' from python" %mode
        client.send(sexp)    # Send data
        # print "(python)> send '%s' from python" %sexp    # そのまま送るので1つのS式(シンボル)としてreadされる
        # 最初の4バイトはデータサイズ
        data_size = struct.unpack('i', client.recv(4))[0]    # ! -> ネットワークバイトオーダ(ビッグエンディアン)(でもEuslisp側が変換しないっぽいのでこの指定をするとバグる), i -> int

        # 以降がデータ。4096バイトごとに分割して受け取る。
        response = ""
        # for i in range(int(math.ceil(float(data_size) / 4096))):    # 小数が切り捨てられないように注意
        #     response += client.recv(4096)    # レシーブは適当な2の累乗にする（大きすぎるとダメ)

        # 改良版
        res_bytes = data_size
        # print(res_bytes)
        while res_bytes >= 4096:
            received_data = client.recv(4096)
            response += received_data
            res_bytes -= len(received_data)
            # print(res_bytes)
        
        if res_bytes:
            response += client.recv(res_bytes)
        
        # print("(python)> response: %s" %response)
        return response

    except socket.error as e:
        # some error handling here
        print "(python) Some Errors occurred when sending/receiveing the data."
        _connect_to_euslisp(host, port)


def eval_foreign_vm_copy(sexp):
    """
    Cで定義したforeign_vmモジュールのsend_sexp_copyメソッドはS式文字列をEuslispに送信し、評価後の戻り値を文字列で受け取る。
    Pythonでevalした際にPythonオブジェクトとして正しく評価されるように適切に文字列として構築されているため、「基本的には」それをevalし、結果を返す。      
    なお、Euslispでエラーが起きた際には
    error code: ~A (~A). ~A in s-expression ~A   (code, msg1, msg2, form)
    なる文字列が送られてくる。

    Args:
        sexp (str): euslispに送りつけるS式
    Returns:
        上記をeuslisp側で評価した結果得られたオブジェクトをPythonオブジェクトに変換したもの
    Raises:
        EusError: Euslispでなんらかのエラーが生じたとき        
    """
    evaluated_object = eval(send_sexp(sexp, mode='c'))
    if isinstance(evaluated_object, str) and evaluated_object.startswith("error code: "):
        raise EusError(evaluated_object)
    return evaluated_object


def eval_foreign_vm_proxy(sexp):
    """
    Cで定義したforeign_vmモジュールのsend_sexp_proxyメソッドはS式文字列をEuslispに送信し、評価後の戻り値を文字列で受け取る。
    Pythonでevalした際にPythonオブジェクトとして正しく評価されるように適切に文字列として構築されているため「基本的には」それをevalし、結果を返す。

    なお、Euslispでエラーが起きた際には
    error code: ~A (~A). ~A in s-expression ~A   (code, msg1, msg2, form)
    なる文字列が、
    Euslisp側にて「この戻り値はProxy objectを作るべきである」と判断した際には (戻り値が数値, nil, t以外のオブジェクトのとき)
    proxy: [~A,~A,~A,[~A]]      (key-num, pkg-name, cls-name)
    なる文字列が送られてくる。
    error code:...に対してはEusErrorを投げ、proxy:...に対してはプロキシオブジェクトを作成して返す。

    Args:
        sexp (str): euslispに送りつけるS式
    Returns:
        数値 or None or True or プロキシクラスのインスタンス
    Raises:
        EusError: Euslispでなんらかのエラーが生じたとき
        RuntimeError: proxyモードであるにもかかわらずエラーコードやプロキシ情報以外の文字列が返ってきたとき
    """
    evaluated_object = eval(send_sexp(sexp, mode='p'))

    if isinstance(evaluated_object, str): 
        if evaluated_object.startswith("error code: "):
            raise EusError(evaluated_object)

        elif evaluated_object.startswith("proxy: "):
            # [key-num,pkg-name,cls-name]なるリストになるはず
            # 整数値, パッケージ名文字列(大文字), クラス名文字列(metaclass, vectorclass, oreoreclassなど。
            # OreOreClassという形でdefclassしても必ず小文字でくる。pkg(小文字)::やpkg:があるかどうかは現在のパッケージ次第)を要素に持つリストになるはず
            key_num, current_pkg_name, cls_name = eval(evaluated_object.lstrip("proxy: "))

            # cls_nameをqualifyし、このクラスの存在するpackageも手に入れる
            if '::' in cls_name:
                pkg_of_symbol, suffix_of_symbol = cls_name.split('::')
                pkg_of_symbol = pkg_of_symbol.upper()
                qualified_cls_name = pkg_of_symbol + '::' + suffix_of_symbol
            elif ':' in cls_name:
                pkg_of_symbol, suffix_of_symbol = cls_name.split(':')
                pkg_of_symbol = pkg_of_symbol.upper()
                qualified_cls_name = pkg_of_symbol + ':' + suffix_of_symbol         
            else:
                pkg_of_symbol = current_pkg_name.upper()    # upper()は冗長だと思うけど一応。
                qualified_cls_name = pkg_of_symbol + '::' + cls_name

            return _make_proxy_instance(key_num, pkg_of_symbol, qualified_cls_name)
        
        raise RuntimeError("Unknown type of returned value in send_sexp_proxy function (please report). got {}".format(evaluated_object))

    return evaluated_object


def _make_proxy_instance(key_num, pkg_name, cls_name):
    """
    Args:
        key_num (int): Euslispのシンボルテーブル方で登録されている番号
        pkg_name (str): 現在のパッケージではなく、このオブジェクトが属するパッケージ名
        cls_name (str): qualify済み。'OREORECLASS::someclass'など。
    Returns:
        プロキシクラスのインスタンス
    """
    package_object = make_eus_instance(pkg_name)
    proxy_class = package_object.get_proxy_class(cls_name)    # クラスオブジェクトが返ってくる

    return proxy_class(key_num)    # key_numを引数にインスタンスを作成



############################################
#### 基本となるユーザーインターフェース ####
############################################

def load_library(src, module_name=None):
    """
    Euslisp側で指定されたファイルのロードを行う。(このEuslispスクリプトは適切に (provide :moduleName)を内部で行い、以降の二重定義を回避していると想定する。

    >>> load_library("/home/ikezaki/Documents/CSG_research/rcb4robots_analysis/rcb4robot.l")    # doctest: +SKIP

    >>> load_library("/home/ikezaki/Documents/CSG_research/rcb4robots_analysis/", module_name=":rcb4lisp")    # doctest: +SKIP

    >>> load_library("/home/ikezaki/Documents/CSG_research/rcb4robots_analysis/", module_name="'rcb4lisp")    # doctest: +SKIP

    >>> load_library("/home/ikezaki/Documents/CSG_research/rcb4robots_analysis/", module_name="RCB4LISP")    # doctest: +SKIP

    Args:
        src (str): euslispファイルのフルパス
        module_name (str): (oprional) 二重定義防止のための文字列。 "'rcb4lisp"や":rcb4lisp"や"RCB4LISP"など。 module_nameはEuslisp側では文字列ないしシンボルが許容されている。
    Returns:
        None
    Raises:
        ImportError: Euslispでloadに失敗したとき (呼び出し元でしっかりキャッチする)
    """
    module_name = module_name.upper()    # 大文字にしてあげる。symbol_mode=Falseなのにmodule_nameを小文字にして呼び出してしまった人用。"'rcb4lisp"が"'RCB4LSP"になったり":rcb4lisp"が":RCB4LISP"になったりするのはlispのリーダ的に問題ない。
    if module_name.startswith("'") or module_name.startswith(":"):
        module_name = module_name[1:]
    try:
        if module_name is None:
            # euslisp側でloadする
            eval_foreign_vm_copy('(load "{}")'.format(src))
        else:
            # euslisp側でrequireする
            eval_foreign_vm_copy('(require "{}" "{}")'.format(module_name, src))
    
    except EusError:
        raise ImportError("Failed to load modules {} in Euslisp.".format(src))



# パッケージオブジェクトをグローバルから引けるようにする。キーはパッケージ名で値は対応するEus_pkgオブジェクト
# キーとしてはpackage名のべたな大文字文字列表現(:や'はついていない)が登録されている。
eus_package_table = {}


def make_eus_instance(pkg="USER", option="underscore"):
    """
    擬似的なEuslispのオブジェクト(Euslisp package) としてふるまうPythonオブジェクトを作成する。
    複数個パッケージの参照を作っても同一のものを指すようになっている。
    load_library()を実行してで読み込みたいEuslispファイルを取り込んでから使用することを想定しているが、load_library()を実行せずにこの関数を使用することも可能。
    その場合、組み込み関数とデフォルトでセットされているグローバル変数のアクセスのみができる。
    
    >>> eus_user = make_eus_instance()    # doctest: +SKIP
    >>> eus_user.reverse([1,2,3])    # doctest: +SKIP
    [3,2,1]
    >>> eus_user._package_.name()    # doctest: +SKIP
    'USER'

    >>> load_library("/home/ikezaki/Documents/CSG_research/rcb4robots_analysis/rcb4robot.l")    # doctest: +SKIP
    >>> robop = make_eus_instance()    # doctest: +SKIP
    >>> robop.make_kxr_robot("kxrl6")    # make-kxr-robot()関数がpython側ではリネームされている。    # doctest: +SKIP 

    Args:
        pkg (str): (optional) パッケージ名 (大文字小文字は問わない。パッケージ接頭語':'やquoteがついていてもよいが結局大文字文字列表現に直される。)
        option (str): (optional) Python側でのリネーム規則。今のところサポートはunderscoreのみ。
    Returns:
        Eus_pkg class instance
    """
    pkg = pkg.upper()
    if pkg.startswith("'") or pkg.startswith(":"):
        pkg = pkg[1:]
     
    if pkg in eus_package_table:
        instance = eus_package_table[pkg]
        assert instance._option == option
        return instance
    else:
        instance = Eus_pkg(pkg, option)
        eus_package_table[pkg] = instance
        return instance


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


class EusContextManager(object):
    """
    現在のパッケージ次第で(variables "" 'pkg)の戻り値にパッケージ接頭語がつくかが決まったりして面倒なのでパッケージ移動を行ってから処理を行いたい。
    戻るのを忘れないようにするために、前のパッケージの記憶ともとの戻す作業をコンテキストマネージャで実装する。
    """
    def __enter__(self):
        self.previous_pkg = get_current_pkg()

    def __exit__(self,  exc_type, exc_value, traceback):
        if exc_type is None:
            set_current_pkg(self.previous_pkg)
        else:
            RuntimeError('Error occurred  EusContextManager (please report). exc_type:{}, exc_value:{}, traceback:{}'.format(exc_type, exc_value, traceback))


class Eus_pkg(object):    # 2系ではobjectを継承しないと旧スタイルのクラスとなり、__getattribute__が機能しない
    """
    EusLisp の package に対応するクラス。
    属性を参照すると package の内容が返ってくる。また、EusLisp 側で値を変更した場合でも変更後の値を参照できる。
    （属性を参照するたびに EusLisp インタープリターに評価させるので)

    なおこのクラスのインスタンスはmake_eus_instance関数にて作成される。
    """
    def __init__(self, pkg, option):
        """
        Args:
            pkg (str): (optional) パッケージ名 (すでに大文字表記となっており、パッケージ接頭語':'やquoteはついていない。':RCB4LISP'や':rcb4lisp'ではなく、'RCB4LISP'や'rcb4lisp')
            option (str): (optional) Python側でのリネーム規則。今のところサポートはunderscoreのみ。
        Returns:
            None
        Raises:
            ImportError: 指定されたパッケージ名をEuslisp側で発見できなかったとき
        """
        with EusContextManager():
            set_current_pkg(pkg)

            self._pkg = pkg
            self._option = option

            def full_qualified(symbols):
                """
                文字列表記されたシンボルのリスト(このEus_pkgクラスと対応するEuslispのパッケージ以外のシンボルが含まれていても良い)を受け取り、
                対応するパッケージ内のシンボルのみ取り出し、それぞれパッケージ接頭語(大文字)をつけ、リストにして返す。

                Args:
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
                        pkg_of_symbol = self._pkg    # current packageは指定したpkg名になっているはず。(コンテキストマネージャ内なので)
                        symbol = pkg_of_symbol + '::' + symbol
                    
                    # LISP packageの組み込み関数などはパッケージ接頭語なしに使えるようになっている。この特定のパッケージ内からもあたかもパッケージ内の関数かのように使えるようにする。
                    if pkg_of_symbol == self._pkg or pkg_of_symbol == "LISP":
                        result.append(symbol)    # cls_pkg(大文字) + '::'か':' + cls_name)
                return result
            
            # あらゆるパッケージ内に存在するシンボル名を文字列表記に直したものを要素にもつリストがきているはず。これらの要素は全てパッケージ解決演算子'::'を含む。 
            self._classlist = full_qualified(eval_foreign_vm_copy("(mapcar #'(lambda (x) (send x :name)) (system:list-all-classes))"))
            # LISPパッケージ内の変数も拾ってきて連結したリストを取得している。variables関数はclassも含んでしまっている(クラスはシンボルの名前空間内に定義されており、この関数はシンボルの名前空間を参照するのだろう)。それらを除く。
            self._varlist = filter(lambda name: name not in self._classlist, full_qualified(eval_foreign_vm_copy('(append (variables "" "LISP") (variables "" "{}"))'.format(self._pkg))))
            self._funclist = full_qualified(eval_foreign_vm_copy('(append (functions "" "LISP") (functions "" "{}"))'.format(self._pkg)))

            self._vartable = {}
            self._functable = {}
            self._classtable = {}
            
            for var_name in self._varlist:
                changed_var_name = _change_valid_name(self._pkg, var_name, self._option)    # optionに従って変換した文字列を取得。変換後のqualifyされた文字列からはpkg(大文字表記)::が消え、許可されない記号が変更されている。
                self._vartable[changed_var_name] = var_name    # python側のシンボルテーブルには変換後の文字列で登録される。値として対応するEuslispのシンボルの文字列表記をもつ。

            for class_name in self._classlist:
                changed_class_name = _change_valid_name(self._pkg, class_name, self._option)
                self._classtable[changed_class_name] = _make_proxy_class(self._pkg, class_name, self._option)    # 内部でメソッド名をクラスのもつ辞書に登録するので、その際_change_valid_name関数を呼ぶ必要がでてくる。それに必要な引数を渡す。
            
            # 関数の名前空間はシンボル(変数、クラス名)の名前空間とわかれている。名前がかぶっているものはfunc_をつけたもののみを、かぶっていないものは両方を定義する
            for func_name in self._funclist:
                changed_func_name = _change_valid_name(self._pkg, func_name, self._option)
                wrapper_function = _make_wrapper_func(func_name)

                if func_name not in self._varlist and func_name not in self._classlist:
                    self._functable[changed_func_name] = wrapper_function
                
                changed_func_name = "func_" + changed_func_name
                self._functable[changed_func_name] = wrapper_function


    def __getattr__(self, name):
        """
        あるパッケージの変数や関数、クラスをユーザが使おうとするとEus_pkg.somethingという形でフィールドアクセスが来るが、そんなものは存在しないのでここに処理が飛ばされてくる。
        _vartable, _functable, _classtableにはキーにPython側でのシンボルであるsomethingなる文字列が、値にはEuslisp側でのシンボル名や適切なラッパー関数オブジェクトやラッパークラスオブジェクトが登録されている。
        このテーブルを引いて対応するものを返す。
        """
        if name in self._vartable:
            return eval_foreign_vm_proxy(self._vartable[name] + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
        elif name in self._functable:
            return self._functable[name]
        elif name in self._classtable:
            return self._classtable[name]
        else:
            raise AttributeError("package '{}' do not contain '{}'".format(self._pkg, name))


    def get_proxy_class(self, class_name):
        """
        eval_foreign_vm関数内でproxyの処理の際に呼ばれるmake_proxy_instance関数の中で呼ばれる。
        対応表を用いてクラス名からプロキシクラスを引き、プロキシクラスオブジェクトを返す。

        Args:
            class_name (str): Euslispでのクラス名。大文字表記パッケージ解決演算子が含まれている。(qualifyされている。)
        Returns:
            プロキシクラスオブジェクト
        """
        assert '::' in class_name  or ':' in class_name  # class_name should be qualified
        changed_class_name = _change_valid_name(self._pkg, class_name, self._option)

        return self._classtable[changed_class_name]


########################################################################
#### 明示的な型指定を行えるようにするためのユーザー用のクラス、関数 ####
########################################################################

class EusConstructor(object):
    pass


class EusSym(EusConstructor):
    def __init__(self, name):
        if not isinstance(name, str):
            raise TypeError("EusSym takes a string as an argument. got {}".format(name))
        if re.search("[\'\(\)\.\"\;]", name):
            raise SyntaxError("Symbol name cannot contain ', (, ), \" and ;. got {}".format(name))
        if name.isdigit():
            raise SyntaxError("Symbol name that only consists of digit is invalid. got {}".format(name))
        self.eus_prefix = "'"
        self.python_literal = name


class EusFuncSym(EusConstructor):
    def __init__(self, func_name):
        if not isinstance(func_name, str):
            raise TypeError("EusFuncSym takes a string as an argument. got {}".format(func_name))
        if re.search("[\'\(\)\.\"\;]", func_name):
            raise SyntaxError("Function name cannot contain ', (, ), \" and ;. got {}".format(func_name))
        if func_name.isdigit():
            raise SyntaxError("Function name that only consists of digit is invalid. got {}".format(func_name))
        self.eus_prefix = "#'"
        self.python_literal = func_name


class EusCons(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusCons takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        self.eus_prefix = "'"
        self.python_literal = sequence


class EusList(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusPlist takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        self.eus_prefix = "'"
        self.python_literal = sequence


class EusPlist(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusPList takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        if not all(map(lambda x: len(x) == 2, sequence)):
            raise TypeError("The shape of property list should be (n, 2). got {}".format(sequence))
        self.eus_prefix = "'"
        self.python_literal = sequence


class EusArray(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusArray takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        self.eus_prefix = "#A"
        self.python_literal = sequence    


class EusVec(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        self.eus_prefix = "#"
        self.python_literal = sequence        


class EusIntVec(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusIntVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        if not all(map(lambda x: isinstance(x, (int, long)), sequence)):
            raise TypeError("Integer expected as an element. got {}".format(sequence))
        self.eus_prefix = "#I"
        self.python_literal = sequence        


class EusFloatVec(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusFloatVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        if not all(map(lambda x: isinstance(x, float), sequence)):
            raise TypeError("Float expected as an element. got {}".format(sequence))
        self.eus_prefix = "#F"
        self.python_literal = sequence        


class EusBitVec(EusConstructor):
    def __init__(self, sequence):
        if not isinstance(sequence, (list, tuple)):
            raise TypeError("EusBitVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
        if not all(map(lambda x: x == 0 or x == 1, sequence)):
            raise TypeError("0 or 1 expected as an element. got {}".format(sequence))
        self.eus_prefix = "#*"
        self.python_literal = sequence        


class EusPath(EusConstructor):
    def __init__(self, name):
        if not isinstance(name, str):
            raise TypeError("EusPath takes a string as an argument. got {}".format(name))
        self.eus_prefix = "#P"
        self.python_literal = name



#######################################################
#### Euslispのカレントパッケージをいじるための関数 ####
#######################################################
def get_current_pkg():
    """
    Returns:
        パッケージ名 (str): 常に大文字表記で返ってくる
    """
    return eval_foreign_vm_copy("(send *package* :name)")


def set_current_pkg(pkg):
    """
    Args:
        pkg (str): 大文字文字列表記
    Returns:
        None
    Raises:
        ImportError: Euslisp側でパッケージが発見できなかったとき
    """
    if eval_foreign_vm_copy('(if (find-package "{0}") (in-package "{0}"))'.format(pkg)):
        return None
    else:            
        raise ImportError("unknown package {} in Euslisp.".format(pkg))




#############################################
#### Eus_pkgのクラス内で使う補助関数たち ####
#############################################
def _change_valid_name(pkg, name, option="underscore"):
    """
    Euslispでの変数名や関数名は+-*/@$%^&=<>~.を使えるのでpythonの識別子ではこれらを変換する必要がある。また、パッケージ接頭語HOGE::やPIYO:を取り除く必要がある。
    optionはunderscoreの他にunicodeにする技も考えたが、python2ではunicodeは変数名に使えないのでサポートしない。


    >>> _change_valid_name("TEST", "TEST::*hoge*")
    '_hoge_'

    >>> _change_valid_name("RCB4LISP", "RCB4LISP::compile-code")
    'compile_code'

    >>> _change_valid_name("TEST", "LISP::reverse")
    'reverse'

    >>> _change_valid_name("TEST", "LISP::*eusdir*")
    '_eusdir_'

    >>> _change_valid_name("pkg", "pkg::some-name", "non_existent_option")
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



class Eus_proxy(object):
    """
    共通の基底クラス
    """
    def __init__(self, key_num):
        self.key_num = key_num

    def to_python(self):
        """
        proxyクラスのインスタンスを、以降はpythonの純粋なオブジェクト(リストやハッシュ、文字列など)として使えるように変換する関数
        """
        return eval_foreign_vm_copy("(lookup-registered-object {})".format(self.key_num))    # user defined classの取扱いをもう少しやる



def _make_proxy_class(pkg, class_name, option):
    """
    make_eus_instance()から呼ばれるEus_pkgクラスコンストラクタにて、eusの関数名のエントリからラッパー関数を作成する際の補助関数。(コンテキストマネージャ内なのでstring-copy-modeであることが保証される)
    引数部の構築は_translate_args()を用いて行う。

    Args:
        pkg (str): Eus_pkgのインスタンス変数self._pkgがくる。大文字でパッケージ接頭語':'はつかない。'RCB4EUS'など。
        class_name (str): qualifyずみ。'RCB4LISP::someclass'など。
    Returns:
        クラスオブジェクト
    """
    field = {}
    field['class_name'] = class_name
    changed_class_name = _change_valid_name(pkg, class_name, option)

    method_list = map(lambda s: s[1:], eval_foreign_vm_copy("(mapcar #'car (send {} :all-methods))".format(class_name)))    # [":prin1", ":super",...]のように:がメソッド名の先頭についている形でくる。python2ではmapはリストを返す
    for method_name in method_list:
        changed_method_name = _change_valid_name(pkg, method_name, option)
        field[changed_method_name] = _make_wrapper_method(method_name)

    return type(changed_class_name, (Eus_proxy, ), field)    # type(クラス名, 親クラス, 辞書)でクラス生成    # changed_class_nameなるクラス名で登録してシンボルとして適当であるようにする。本当の名前が知りたくなったらself.class_nameに聞いてくだい


def _make_wrapper_method(method_name):
    """
    make_eus_instance()から呼ばれるEus_pkgクラスコンストラクタにて、eusのメソッド名のエントリからラッパーメソッドを作成する際の補助関数。
    引数部の構築は_translate_args()を用いて行う。

    Args:
        method_name (str): もとのEuslispでのメソッド名。もちろんパッケージ解決演算子などは含まない。
    Returns:
        wrapper (function): 複数引数, キーワード引数を適切にEuslispで処理可能な形で変換しS式を送り込む処理を行う関数
    """

    def wrapper(self, *args, **kwargs):
        return eval_foreign_vm_proxy('(send (lookup-registered-object {}) :{}{})'.format(self.key_num, method_name, _translate_args(args, kwargs)))    # (lookup-registered-object {})のところでEuslisp側でオブジェクトを得る。それに対しsendを行う。

    return wrapper


def _make_wrapper_func(func_name):
    """
    make_eus_instance()から呼ばれるEus_pkgクラスコンストラクタにて、eusの関数名のエントリからラッパー関数を作成する際の補助関数。
    引数部の構築は_translate_args()を用いて行う。

    Args:
        func_name (str): もとのEuslispでの関数名でpkg::を含む。なお、関数は内部シンボルと仮定している。(exportされてたら外部シンボルアクセス:(1個)を使わないとならない)。
    Returns:
        wrapper (function): 複数引数, キーワード引数を適切にEuslispで処理可能な形で変換しS式を送り込む処理を行う関数
    """
    def wrapper(*args, **kwargs):
        return eval_foreign_vm_proxy('({}{})'.format(func_name, _translate_args(args, kwargs)))

    return wrapper
    


def _translate_args(t=tuple(), d=dict()):
    """
    _make_wrapper_argsでラッパー関数を作成する際の補助関数。
    関数呼び出しを行うS式の引数部分に埋め込む文字列を作成する。
    e.g.) 
        (func)なるS式を送り込む場合 ""を生成することを担当する。
        (func args)なるS式を送り込む場合 " args"を生成することを担当する。(冒頭のスペースに留意)

    文字列を埋め込む際変数は適切に評価を行ってから埋め込む。
    Euslispにはハッシュのリテラル表現はない(と思う)ので、タプルやリストのみを再帰的にLispのリスト表現に変換する。Lispのリスト表現の冒頭には(quote )を付加する。

    >>> _translate_args((), {})
    ''

    >>> var1, var2 = 100, 200
    >>> _translate_args(t=(var1, var2))
    ' 100 200'

    >>> L = ((1,2),(3,4))
    >>> _translate_args(t=L)
    ' (quote (1 2)) (quote (3 4))'

    >>> L2 = (((1,2),(3,4)),)
    >>> _translate_args(t=L2)
    ' (quote ((1 2) (3 4)))'

    >>> val1, val2 = 5, 10
    >>> _translate_args(d={"a":val1, "b":val2})
    ' :a 5 :b 10'

    >>> nested_list_dict = {"key":[1,[2,[3]]]}
    >>> _translate_args(d=nested_list_dict)
    ' :key (quote (1 (2 (3))))'

    >>> _translate_args(t=(1, 2), d={"x":3, "y":4})
    ' 1 2 :y 4 :x 3'
    
    >>> pi = 3.14
    >>> t, d = (("ho", "ge", pi), 1), {"x":3, "y":(4, 5)}
    >>> _translate_args(t, d)
    ' (quote ("ho" "ge" 3.14)) 1 :y (quote (4 5)) :x 3'


    Args:
        最初の呼び出しは(args, kwargs)を想定している。argsは関数呼び出し時に*argsにバインドされたタプル, kwargsは関数呼び出し時に**kwargsにバインドされたディクショナリ
        t (tuple or list)
        d (dictionary)
    Returns:
        str: 関数呼び出しを行うS式の引数部分に埋め込む文字列
    """
    return '{}{}'.format(_translate_tuple(t), _translate_dict(d))


def _translate_tuple(t, recursive=False):
    """
    引数のタプルをよしなに変換する(シンボルは評価される)。再帰呼出しからは(quote )が付加される。

    >>> _translate_tuple(tuple())
    ''

    # 空のリストを引数に関数呼び出しをするとき。tは空のリスト1要素を持つタプルなので以下のようになる。Euslispでは空リストとして評価される。
    >>> _translate_tuple(([], ))    
    ' (quote ())'

    >>> _translate_tuple(("a", "b", "c"))
    ' "a" "b" "c"'

    >>> _translate_tuple((1, [2, 3], [4, [5, 6]]))
    ' 1 (quote (2 3)) (quote (4 (5 6)))'
    """
    s = ''
    for ind, elm in enumerate(t):
        if not recursive or ind != 0:
            s += ' '
        
        if elm is None:
            s += 'nil'
        elif elm is True:
            s += 't'
        elif isinstance(elm, str):
            s += '"{}"'.format(elm)                 
        elif isinstance(elm, (list, tuple, xrange)):
            if recursive:
                s += '({})'.format(_translate_tuple(t=elm, recursive=True))
            else:
                s += '(quote ({}))'.format(_translate_tuple(t=elm, recursive=True))
        # Eus_proxyを継承したclass objectのとき
        elif type(elm) == type and "Eus_proxy" in map(lambda x: x.__name__, elm.__bases__):    # issubclass()はクラスがグローバルに登録されていないので使えない
            s += '{}'.format(elm.class_name)
        # Eus_proxyを継承したclassのinstanceのとき
        elif isinstance(elm, Eus_proxy):
            s += '(lookup-registered-object {})'.format(elm.key_num)
        # 明示的な型変換が行われているとき
        elif isinstance(elm, EusConstructor):
            # recursiveかどうかにより適宜クオートをつけるかどうか決める。#リーダーマクロはrecursiveだろうと(クオートされていても)必ずつける
            embedding = _translate_eus_constructor(elm)
            if isinstance(elm, (EusFuncSym, EusPath, EusArray, EusVec, EusIntVec, EusFloatVec, EusBitVec)):
                s += '{}{}'.format(elm.eus_prefix, embedding)
            elif not recursive:
                s += '{}{}'.format(elm.eus_prefix, embedding)
            else:
                s += embedding 
        else:
            s += '{}'.format(elm)
    return s


def _translate_dict(d):
    """
    引数の辞書をよしなに変換する(シンボルは評価される)。

    >>> _translate_dict(dict())
    ''

    >>> _translate_dict({"a":1, "b":2})
    ' :a 1 :b 2'

    >>> _translate_dict({"1": [1,2,3], "2":[4,5,6]})
    ' :1 (quote (1 2 3)) :2 (quote (4 5 6))'
    """
    s = ''
    for key in d:
        val = d[key]
        s += ' '

        if val is None:
            s += ':{} nil'.format(key)
        elif val is True:
            s += ':{} t'.format(key)
        elif isinstance(val, str):
            s += ':{} "{}"'.format(key, val)            
        elif isinstance(val, (list, tuple, xrange)):
            s += ':{} (quote ({}))'.format(key, _translate_tuple(t=val, recursive=True))    # _translate_dictは再帰呼出しされることはないので(Euslispにハッシュテーブルのリテラル表現がない以上送るのを諦めている。)revursiveチェックは不要。
        # Eus_proxyを継承したclass objectのとき
        elif type(val) == type and "Eus_proxy" in map(lambda x: x.__name__, val.__bases__):
            s += '{}'.format(val.class_name)
        # Eus_proxyを継承したclassのinstanceのとき
        elif isinstance(val, Eus_proxy):
            s += ':{} (lookup-registered-object {})'.format(key, val.key_num)
        # 明示的な型変換が行われているとき
        elif isinstance(val, EusConstructor):
            # ここは再帰呼出しされていないので、常にprefixをつければよい。
            embedding = _translate_eus_constructor(val)
            s += '{}{}'.format(val.eus_prefix, embedding)

        else:
            s += ':{} {}'.format(key, val)
    return s


def _translate_eus_constructor(mediator):
    """
    _translate_tuple, _translate_dictの内部で呼び出される補助関数。
    EusSym(str)などの仲介クラスをうけとり、すでに外のネストにおいてクオートされているときの適切な文字列表現を返す。

    >>> _translate_eus_constructor(EusSym("Python"))
    'Python'

    >>> _translate_eus_constructor(EusFuncSym(">"))
    '>'

    >>> _translate_eus_constructor(EusCons([1, 10, 100]))
    '(1 10 . 100)'

    >>> _translate_eus_constructor(EusPlist([[1,11], [2,22], [3,33]]))
    '((1 . 11)(2 . 22)(3 . 33))'

    >>> _translate_eus_constructor(EusList([[1],2,[3,4]]))
    '((1) 2 (3 4))'

    Args:
        mediator (EusConstructor instance)
    Returns:
        str
    Raises:
        RuntimeError: 既存のコードでハンドリングできていないEusConstructorクラスのオブジェクトが引数として渡されたとき
    """

    if isinstance(mediator, (EusSym, EusFuncSym, EusPath)):
        return '{}'.format(mediator.python_literal)

    elif isinstance(mediator, EusCons):
        if len(mediator.python_literal) == 0 or len(mediator.python_literal) == 1:
            return '({})'.format(_translate_tuple(t=tuple(mediator.python_literal), recursive=True))
        else:
            car = _translate_tuple(t=mediator.python_literal[:-1], recursive=True) 
            cdr = _translate_tuple(t=mediator.python_literal[-1:], recursive=True)    # tにリストがバインドされるのがポイント([-1]じゃだめ)
            return '({} . {})'.format(car, cdr)

    elif isinstance(mediator, EusPlist):
        pairs = ''
        for pair in mediator.python_literal:
            car = _translate_tuple(t=pair[:1], recursive=True)    # tにリストがバインドされるのがポイント([0]じゃだめ)
            cdr = _translate_tuple(t=pair[1:], recursive=True)
            pairs += '({} . {})'.format(car, cdr)
        return  '({})'.format(pairs)

    elif isinstance(mediator, (EusList, EusArray, EusVec, EusIntVec, EusFloatVec)):
        return '({})'.format(_translate_tuple(t=mediator.python_literal, recursive=True))

    elif isinstance(mediator, EusBitVec):
        return "".join(map(lambda b: str(b), mediator.python_literal))    # bit-vectorだけリテラルが #*1010のような表記であることに留意
    
    else:
        RuntimeError("Unknown EusConstructor class (please report). got {}".format(mediator))




###################
#### for debug ####
###################
def my_assert(actual, expected):
    """
    何番目のテストケースが通って何番目のテストケースが死んだのかを表示するデバッグ用関数
    """
    # static変数ライクに使うために関数オブジェクトの属性に登録してしまう
    if not hasattr(my_assert, 'count_iter'):
        my_assert.count_iter = 0
        my_assert.count_passed = 0
        my_assert.count_error = 0

    my_assert.count_iter += 1

    if actual == expected:
        print("*test{} passed.* (actual={}, expected={})".format(my_assert.count_iter, actual, expected))
        my_assert.count_passed += 1
    else:
        print >> sys.stderr, "***test{} failed!*** (actual={}, expected={})".format(my_assert.count_iter, actual, expected)
        my_assert.count_error += 1


if __name__ == "__main__":
    import doctest
    doctest.testmod()
