#!/usr/bin/env python2
# coding: utf-8

"""
ライブラリバージョンのpyeus
"""
import socket, shlex, subprocess, time, atexit, struct
import re
import pyeus_util as util
from pyeus_util import EusError


if __name__ == "pyeus":

    command = "irteus eus_server.l"
    eus_process = subprocess.Popen(shlex.split(command))    # shlex.split()なら複雑な状況(ファイル名にスペースとか)でも考慮してsplitできる

    # host = socket.gethostname()    # MacOSでは動作しない
    host = "127.0.0.1"
    port = 50000

    is_connected = False    # close_socketsや__del__で使用されるフラグ (コネクションが切れているのにメッセージを送ろうとするとエラーになる)
    client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)    # Create socket object (AF_INET -> IPv4 , SOCK_STREAM -> TCP)


    def _connect_to_euslisp(host, port):
        """
        Euslispのソケットに接続を試みる関数。Eus_processが何らかの原因で落ちていた場合は起動し直した上で接続を試みる。
        なお、失敗すると 1 sec 後に再接続を試みるが、トータルで5回失敗するとこれ以上の接続を諦め、プログラムを終了する。

        Args:
            host (str): host name
            port (int): port number
        """
        if not hasattr(_connect_to_euslisp, 'failure_count'):
            _connect_to_euslisp.failure_count = 0
        
        global eus_process
        if eus_process.poll() is not None:
            print("(python)> Euslisp process seems to be killed accidentally. Trying to run the Euslisp process again...")
            eus_process = subprocess.Popen(shlex.split(command))

        try:
            print("(python)> Trying to connect...")
            client.connect((host, port))
            global is_connected
            is_connected = True
            print("(python)> Connected.")
        except socket.error:
            print("(python)> Failed to connect. Sleep for 1 sec.")
            _connect_to_euslisp.failure_count += 1
            time.sleep(1)
            if _connect_to_euslisp.failure_count <= 5:
                _connect_to_euslisp(host, port)
            else:
                print("(python)> Failed in connecting more than 5 times. Give up connecting.")
                exit()

    def finalization():
        """下記の二つの関数を呼ぶ終了処理。ユーザープログラム終了時には必ず呼ばれる。"""
        close_sockets()
        kill_eus_process()

    def close_sockets():
        """ソケット切断関数"""
        global is_connected
        if is_connected:
            client.shutdown(socket.SHUT_RDWR)    # 即座に切断 (close()は即座に切断はしない)
            client.close()   # このソケットを閉じられたものとしてマーク。
        is_connected = False

    def kill_eus_process():
        """フックしたEuslispプロセスの終了関数"""
        if eus_process.poll() is None:
            eus_process.kill()


    atexit.register(finalization)    # プログラム終了時にソケット切断と子プロセスの終了処理を行う
    _connect_to_euslisp(host, port)



# Pythonのコールバック関数をEuslispに送る際、本体の関数オブジェクトを登録しておく辞書
global_callback_table = {}

# global_callback_tableのキーには, pyfunc1, pyfunc2, ...などとなるようにしたい。そのsuffixの数値を管理するためのカウンタ
global_callback_key_count = 0


def receive_response():
    """
    send_sexp()関数内で呼ばれる補助関数で、ソケットストリームからデータを取得する。

    なお、Euslisp内で評価を行う最中にPythonのcallback関数を呼ぶ必要が出てきた際には
    callback:~A [...]    (foreign-function-symbol, *args)
    なる文字列が送られてくるが、この場合呼び出し元に返すのではなく、(内密に盗み見て)こちら側でcallback関数の適用を行い直ちにEuslisp側へ結果を戻す。
    結果的にsend_sexpの戻り値はsexpの評価値で確定するようになる。

    Returns:
        response (str): result string that has not yet evaluated
    """
    # 最初の4バイトはデータサイズ
    data_size = struct.unpack('i', client.recv(4))[0]    # ! -> ネットワークバイトオーダ(ビッグエンディアン)(でもEuslisp側が変換しないっぽいのでこの指定をするとバグる), i -> int

    # 以降がデータ。4096バイトごとに分割して受け取る。
    response = ""
    res_bytes = data_size
    while res_bytes >= 4096:
        received_data = client.recv(4096)
        response += received_data
        res_bytes -= len(received_data)
    
    if res_bytes:
        response += client.recv(res_bytes)
    
    peeked_result = eval(response)
    if isinstance(peeked_result, str) and peeked_result.startswith('callback:'):   
        stripped = peeked_result.lstrip('callback:')
        foreign_function_symbol = stripped.split()[0]    # 文字列
        argstring_list = stripped.split()[1:]    # 文字列のリスト
        arg_list = [handling_proxy(elm) if elm.startswith('proxy:') else eval(elm) for elm in argstring_list]
        callback_applied_result = global_callback_table[foreign_function_symbol](*arg_list)    # proxy:などを処理した引数値をアンパックしてから関数適用
        send_sexp(str(callback_applied_result) + ' ', mode='b')    # あちらではmodeが'b'であるものとして(read stream)で待ち受けている。その期待に添うように「' 'を末尾に追加した」データを生成して返す
        
        response = receive_response()    # 何事もなかったかのように再帰
    
    # print("(python)> response: %s" %response)
    return response


def send_sexp(sexp, mode):
    """
    実際にEuslispプロセスへとデータを送り込みバイト列データの結果を受け取る。

    >>> send_sexp("(+ 1 2)", 'c')    # doctest: +SKIP
    '3'  

    Args:
        sexp (str): syntax expression
        mode (str): 'n' for non-response mode, 'c' for copy mode, p' for proxy mode, and 'b' for callback response mode. Currrently, 'n' is only used in destructor and macro call.
    Returns:
        response (str): result string that has not yet evaluated
    """
    try:
        if not util.valid_sexp_predicate(sexp):
            raise SyntaxError("got invalid sexp [excess '(' or ')']: {}".format(sexp))
        if mode not in ('n', 'c', 'p', 'b'):
            raise SyntaxError("got invalid mode (nor 'n' and 'c' and 'p' and 'b'): {}".format(mode))

        # print("(python)> send '%s' from python" %mode)
        print("(python)> send '%s' from python" %sexp)

        send_mode = '"' + mode + '" '    # 末尾にspaceを追加して"を追加する。Euslisp側から1つのS式、文字列としてreadされる
        client.send(send_mode + sexp)    # そのまま送るので1つのS式(シンボル)としてreadされる
        
        if mode == 'c' or mode == 'p':    # send_modeではない
            response = receive_response()
            return response

    except socket.error:
        print("(python) Some Errors occurred when sending/receiveing the data.")
        _connect_to_euslisp(host, port)


def eval_foreign_vm_copy(sexp):
    """
    Cで定義したforeign_vmモジュールのsend_sexp_copyメソッドはS式文字列をEuslispに送信し、評価後の戻り値を文字列で受け取る。

    Pythonでevalした際にPythonオブジェクトとして正しく評価されるように適切に文字列として構築されているため、「基本的には」それをevalし、結果を返す。      
    なお、Euslispでエラーが起きた際には
    error_code:~A (~A). ~A in s-expression ~A   (code, msg1, msg2, form)
    なる文字列が送られてくる。
    error_code:...に対してはEusErrorを投げる。
    また、Euslispでcallback関数の適用結果が必用になった場合は
    callback:~A ...    (foreign function symbol, args)
    なる文字列が送られてくるが、それらはrecieve_response関数内で適切に処理されここまであがってくることはない。

    >>> eval_foreign_vm_copy("(+ 1 2)")    # doctest: +SKIP
    3

    >>> eval_foreign_vm_copy("(list 1 2 3)")    # doctest: +SKIP
    [1, 2, 3]

    Args:
        sexp (str): euslispに送りつけるS式
    Returns:
        上記をeuslisp側で評価した結果得られたオブジェクトをPythonオブジェクトに変換したもの
    Raises:
        EusError: Euslispでなんらかのエラーが生じたとき        
    """
    evaluated_object = eval(send_sexp(sexp, mode='c'))
    if isinstance(evaluated_object, str) and evaluated_object.startswith("error_code:"):
        raise EusError(evaluated_object)

    return evaluated_object


def eval_foreign_vm_proxy(sexp):
    """
    Cで定義したforeign_vmモジュールのsend_sexp_proxyメソッドはS式文字列をEuslispに送信し、評価後の戻り値を文字列で受け取る。

    Pythonでevalした際にPythonオブジェクトとして正しく評価されるように適切に文字列として構築されているため「基本的には」それをevalし、結果を返す。
    なお、Euslispでエラーが起きた際には
    error_code:~A (~A). ~A in s-expression ~A   (code, msg1, msg2, form)
    なる文字列が送られてくる。
    Euslisp側にて「この戻り値はProxy objectを作るべきである」と判断した際には (戻り値が数値, nil, t以外のオブジェクトのとき)
    proxy:[~A,~A,~A,[~A]]      (key-num, pkg-name, cls-name)
    なる文字列が送られてくる。
    error_code:...に対してはEusErrorを投げ、proxy:...に対してはプロキシオブジェクトを作成して返す。
    また、Euslispでcallback関数の適用結果が必用になった場合は
    callback:~A ...    (foreign function symbol, args)
    なる文字列が送られてくるが、それらはrecieve_response関数内で適切に処理されここまであがってくることはない。

    >>> eval_foreign_vm_proxy("(+ 1 2)")    # doctest: +SKIP
    3

    >>> eval_foreign_vm_proxy("(list 1 2 3))    # doctest: +SKIP
    <pyeus.cons object at 0x7fad2f40ab10>

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
        if evaluated_object.startswith("error_code:"):
            raise EusError(evaluated_object)

        elif evaluated_object.startswith("proxy:"):
            return handling_proxy(evaluated_object)
        
        raise RuntimeError("Unknown type of returned value (type: str) in send_sexp_proxy function. Please report. got {}".format(evaluated_object))

    return evaluated_object


def handling_proxy(evaluated_object):
    """
    eval_foreign_vm_proxyで呼ばれる補助関数。proxy:...なる文字列からプロキシクラスのインスタンスを作成する。

    Args:
        evaluated_object (str): 'proxy:[key-num,pkg-name,cls-name]'なる文字列。key-num: 整数値、pkg-name: パッケージ名文字列(大文字表記)、cls-name: クラス名文字列(小文字表記、pkg接頭語がつくこともある)
    Returns:
        プロキシクラスのインスタンス
    """
    key_num, current_pkg_name, cls_name = eval(evaluated_object.lstrip("proxy:"))    # int, str, str

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


def _make_proxy_instance(key_num, pkg_name, cls_name):
    """
    パッケージ名とクラス名をもとに、そのパッケージオブジェクトのプロキシクラスを探し出し、そのコンストラクタに登録番号をわたしてプロキシクラスのインスタンスを作成する。

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
    module_name = util.pkg_uniform_upper(module_name)
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
    pkg = util.pkg_uniform_upper(pkg)
     
    if pkg in eus_package_table:
        instance = eus_package_table[pkg]
        assert instance._option == option
        return instance
    else:
        instance = Eus_pkg(pkg, option)
        eus_package_table[pkg] = instance
        return instance


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
            RuntimeError('Error occurred  EusContextManager. Please report. exc_type:{}, exc_value:{}, traceback:{}'.format(exc_type, exc_value, traceback))


class FunctionNamespace(object):
    """
    Eus_pkgクラス内でEuslsip関数の名前とEuslispシンボル(クラス、変数)の名前がぶつかった際、関数はrobop.func.hoge()のようにアクセスしたい。
    名前空間の分離を行うためだけのクラス。
    """
    def __init__(self, pkg):
        self._pkg = pkg
        self._inner_functable = {}

    def __getattr__(self, name):
        """
        下記Eus_pkgクラスの__getattr__の関数だけバージョン
        """
        if name in self._inner_functable:
            return self._inner_functable[name]
        else:
            raise AttributeError("package '{}' do not contain '{}'".format(self._pkg, name))


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
            
            # あらゆるパッケージ内に存在するシンボル名を文字列表記に直したものを要素にもつリストがきているはず。これらの要素は全てパッケージ解決演算子'::'を含む。 
            self._classlist =  util.full_qualified(self._pkg, eval_foreign_vm_copy("(mapcar #'(lambda (x) (send x :name)) (system:list-all-classes))"))
            # LISPパッケージ内の変数も拾ってきて連結したリストを取得している。variables関数はclassも含んでしまっている(クラスはシンボルの名前空間内に定義されており、この関数はシンボルの名前空間を参照するのだろう)。それらを除く。
            self._varlist = filter(lambda name: name not in self._classlist,  util.full_qualified(self._pkg, eval_foreign_vm_copy('(append (variables "" "LISP") (variables "" "{}"))'.format(self._pkg))))
            self._funclist =  util.full_qualified(self._pkg, eval_foreign_vm_copy('(append (functions "" "LISP") (functions "" "{}"))'.format(self._pkg)))

            self._vartable = {}
            self._functable = {}
            self._classtable = {}
            
            for var_name in self._varlist:
                # optionに従って変換した文字列を取得。変換後のqualifyされた文字列からはpkg(大文字表記)::が消え、許可されない記号が変更されている。
                changed_var_name = util.change_valid_name(self._pkg, var_name, self._option) 
                # python側のシンボルテーブルには変換後の文字列で登録される。値として対応するEuslispのシンボルの文字列表記をもつ。
                self._vartable[changed_var_name] = var_name    

            for class_name in self._classlist:
                changed_class_name = util.change_valid_name(self._pkg, class_name, self._option)
                # 内部でメソッド名をクラスのもつ辞書に登録するので、その際util.change_valid_name関数を呼ぶ必要がでてくる。それに必要な引数を渡す。
                self._classtable[changed_class_name] = _make_proxy_class(self._pkg, class_name, self._option)
            
            # set_params()メソッドで元の関数名を復元するときに使用していた(が今は使われていない)
            self._from_changed_to_default_func_name = {}
            
            # 関数の名前空間はシンボル(変数、クラス名)の名前空間とわかれている。名前がかぶっているものはfunc_をつけたもののみを、かぶっていないものは両方を定義する。関数はfunc.hogeという形でも明示的に呼べるようにする。
            self.func = FunctionNamespace(self._pkg)

            for func_name in self._funclist:
                changed_func_name = util.change_valid_name(self._pkg, func_name, self._option)
                self._from_changed_to_default_func_name[changed_func_name] = func_name
                wrapper_function = _make_wrapper_func(func_name)

                # 衝突していないもの
                if func_name not in self._varlist and func_name not in self._classlist:
                    self._functable[changed_func_name] = wrapper_function
                
                # 衝突しているものの対応その1
                self.func._inner_functable[changed_func_name] = wrapper_function

                # 衝突しているものの対応その2  
                changed_func_name = "func_" + changed_func_name
                self._functable[changed_func_name] = wrapper_function


    def __getattr__(self, name):
        """
        あるパッケージの変数や関数、クラスをユーザが使おうとif not hasattr(my_assert, 'count_iter'):するとEus_pkg.somethingという形でフィールドアクセスが来るが、そんなものは存在しないのでここに処理が飛ばされてくる。
        _vartable, _functable, _classtableにはキーにPython側でのシンボルであるsomethingなる文字列が、値にはEuslisp側でのシンボル名や適切なラッパー関数オブジェクトやラッパークラスオブジェクトが登録されている。
        このテーブルを引いて対応するものを返す。
        """
        if name in self._vartable:
            return eval_foreign_vm_proxy(self._vartable[name] + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースが必用
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
            class_name (str): Euslispでのクラス名。大文字表記パッケージ解決演算子が含まれている。(qualifyずみであることを仮定)
        Returns:
            プロキシクラスオブジェクト
        """
        assert '::' in class_name  or ':' in class_name
        changed_class_name = util.change_valid_name(self._pkg, class_name, self._option)

        return self._classtable[changed_class_name]
    
 

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



#########################################################
#### 明示的な型変換のためのコンストラクタ (風の関数) ####
#########################################################
def EusSym(pkg, name):
    pkg = util.pkg_uniform_upper(pkg)
    if not isinstance(name, str):
        raise TypeError("EusSym takes a string as an argument. got {}".format(name))
    if re.search(r"[\'\(\)\.\"\;]", name):
        raise SyntaxError("Symbol name cannot contain ', (, ), \" and ;. got {}".format(name))
    if name.isdigit():
        raise SyntaxError("Symbol name that only consists of digit is invalid. got {}".format(name))
    eus_prefix = "'"
    # パッケージ名を適宜つけないとシンボルアクセスできぬ
    python_literal =  util.full_qualified(pkg, [name])[0]
    # quoteを二重にしてやる必用はない。lispのreadはデータモードで読み込むので、quoteが１つあればEuslispでevalした際にシンボルとなる。
    proxy = eval_foreign_vm_proxy(eus_prefix + "{}".format(python_literal) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースが必用
    return proxy


def EusFuncSym(pkg, func_name):
    pkg = util.pkg_uniform_upper(pkg)
    if not isinstance(func_name, str):
        raise TypeError("EusFuncSym takes a string as an argument. got {}".format(func_name))
    if re.search(r"[\'\(\)\.\"\;]", func_name):
        raise SyntaxError("Function name cannot contain ', (, ), \" and ;. got {}".format(func_name))
    if func_name.isdigit():
        raise SyntaxError("Function name that only consists of digit is invalid. got {}".format(func_name))
    eus_prefix = "#'"
    python_literal = util.full_qualified(pkg, [func_name])[0]
    proxy = eval_foreign_vm_proxy(eus_prefix + "{}".format(python_literal) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースが必用
    return proxy


def EusStr(s=''):
    if not isinstance(s, str):
        raise TypeError("EusStr takes a string as an argument. got {}".format(s))
    eus_prefix = '"'
    eus_suffix = '"'
    python_literal = s
    proxy = eval_foreign_vm_proxy(eus_prefix + "{}".format(python_literal + eus_suffix) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusHash(d={}, test="equal"):
    # test-functionをeqからequalに変えていることに注意。Pythonで辞書のキーは文字列であるため。
    if not isinstance(d, dict):
        raise TypeError("EusHash takes a dictionary as an argument. got {}".format(d))
    if test not in ['eq', 'eql', 'equal']:
        raise TypeError("EusHash takes 'eq' or 'eql' or 'equal' as a test function. got {}".format(test))
    filling = ""
    func_string = "#'" + test
    for key, value in d.items():
        filling += '(setf (gethash "{}" hsh) {})'.format(key, _translate_tuple((value, ), unfreeze=True))
    command = "(let ((hsh (make-hash-table :test {}))) (progn {}) hsh)".format(func_string, filling)
    proxy = eval_foreign_vm_proxy(command)
    return proxy

# 以下のようなコンテナ型は、'({})でリテラル表現を作り送り込むと '(((lookup-registered-object ...) . 100))のように送られてしまう。
# proxyを取得するS式部分は評価させるために、適切に ` と , でunfreezeさせる必要がある
# _translateのところで(lookup-...)の前に毎回 , をぶち込むように指定して、これらのコンストラクタのeus_prefixは ' ではなく ` にする
#  ` は何もなかったら ' と同ななので完全に ` を採用してOK. unfreezeさせる , は ` とセットである必要がある。毎回unfreeze=Trueと指定して、_translate_tupleの方で臨機応変に , をつけるか判断するようにする
def EusCons(sequence=[]):
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusCons takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    eus_prefix = "`"
    python_literal = sequence
    if len(python_literal) == 0 or len(python_literal) == 1:
        proxy =  eval_foreign_vm_proxy('({})'.format(_translate_tuple(t=tuple(python_literal), unfreeze=True, recursive=True)))
    else:
        car = _translate_tuple(t=python_literal[:-1], unfreeze=True, recursive=True) 
        cdr = _translate_tuple(t=python_literal[-1:], unfreeze=True, recursive=True)    # tにリストがバインドされるのがポイント([-1]じゃだめ)
        proxy = eval_foreign_vm_proxy(eus_prefix + '({} . {})'.format(car, cdr) + " ")     # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusList(sequence=[]):
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusPlist takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    eus_prefix = "`"
    python_literal = sequence
    proxy = eval_foreign_vm_proxy(eus_prefix + '({})'.format(_translate_tuple(t=python_literal, unfreeze=True, recursive=True)) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusPlist(sequence=[[None, None]]):
    # '((indicator1 . 100) (indicator2 . 200))のようなものを作るにはEusSymを内部で使うようにする。
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusPList takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    if not all(map(lambda x: len(x) == 2, sequence)):
        raise SyntaxError("The shape of property list should be (n, 2). got {}".format(sequence))
    eus_prefix = "`"
    python_literal = sequence
    pairs = ''
    for pair in python_literal:
        car = _translate_tuple(t=pair[:1], unfreeze=True, recursive=True)    # スライスにより、tにリストがバインドされるのがポイント([0]じゃだめ)
        cdr = _translate_tuple(t=pair[1:], unfreeze=True, recursive=True)
        pairs += '({} . {})'.format(car, cdr)
    plist = eus_prefix + "({})".format(pairs)
    proxy = eval_foreign_vm_proxy("(make-instance propertied-object :plist {})".format(plist))
    return proxy


def EusArray(sequence=[]):
    # arrayは次元のそろったlistやvectorのコンテナとなる。#Aではなくmake-array関数を使用したい。が、EusListやEusIntVecといったproxyを引数にとってきたときに次元を数えるのが難しい。次元がないとdimの指定ができない。
    # #aスタイルだと次元が揃っていないとarrayが作れないが、make-array関数は適切にnilで要素を拡張する上にリストを適切に「無視する」( (make-array '(2 3) :initial-contents '((1 2 3) (4 (5) 6))) とかなら[1][1]は (5) となる。)
    # check-proper-dimention関数を用いて(貪欲に)生成可能な次元を特定する。
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusArray takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    if sequence == []:
        eus_prefix = "#A"
        proxy = eval_foreign_vm_proxy(eus_prefix + '()' + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    else:
        list_proxy = EusList(sequence)
        dim_list_proxy = eval_foreign_vm_proxy("(check-proper-dimention nil {})".format(_translate_tuple(t=(list_proxy, ), unfreeze=False, recursive=True)))
        proxy = eval_foreign_vm_proxy("(make-array {} :initial-contents {})".format(_translate_tuple(t=(dim_list_proxy, ), unfreeze=False, recursive=True), _translate_tuple(t=(list_proxy, ), unfreeze=False, recursive=True)))    
    return proxy


def EusVec(sequence=[]):
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    eus_prefix = "#"
    python_literal = sequence        
    proxy = eval_foreign_vm_proxy(eus_prefix + '({})'.format(_translate_tuple(t=python_literal, unfreeze=False, recursive=True)) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusIntVec(sequence=[]):
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusIntVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    if not all(map(lambda x: isinstance(x, (int, long)) or x is None, sequence)):
        raise SyntaxError("Integer expected as an element. got {}".format(sequence))
    eus_prefix = "#I"
    python_literal = sequence        
    proxy = eval_foreign_vm_proxy(eus_prefix + '({})'.format(_translate_tuple(t=python_literal, unfreeze=False, recursive=True)) + " ")     # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusFloatVec(sequence=[]):
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusFloatVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    if not all(map(lambda x: isinstance(x, float) or x is None, sequence)):
        raise SyntaxError("Float expected as an element. got {}".format(sequence))
    eus_prefix = "#F"
    python_literal = sequence        
    proxy = eval_foreign_vm_proxy(eus_prefix + '({})'.format(_translate_tuple(t=python_literal, unfreeze=False, recursive=True)) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusBitVec(sequence=[]):
    if not isinstance(sequence, (list, tuple)):
        raise TypeError("EusBitVec takes a sequence (list or tuple) as an argument. got {}".format(sequence))
    if not all(map(lambda x: x == 0 or x == 1 or x is None, sequence)):
        raise SyntaxError("0 or 1 expected as an element. got {}".format(sequence))
    eus_prefix = "#*"
    python_literal = sequence
    # bit-vectorだけリテラルが #*1010のような表記であることに留意    
    proxy = eval_foreign_vm_proxy(eus_prefix + '{}'.format("".join(map(lambda b: str(b), python_literal))) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


def EusPath(name='/'):
    if not isinstance(name, str):
        raise TypeError("EusPath takes a string as an argument. got {}".format(name))
    eus_prefix = "#P"
    python_literal = name
    proxy = eval_foreign_vm_proxy(eus_prefix + '"{}"'.format(python_literal) + " ")    # socketの向こう側では(read)で待機している。S式の塊がわかるようスペースをぶちこむ
    return proxy


######################################
#### 型変換を自動化するための関数 ####
######################################

# 許容される型コンストラクタの一覧。set_params関数で変なコンストラクタを指定されないようにするために使う。
valid_constructor = (EusSym, EusFuncSym, EusStr, EusHash, EusCons, EusList, EusPlist, EusArray, EusVec, EusIntVec, EusFloatVec, EusBitVec, EusPath)


def set_params(pkg_obj, changed_func_name, *arg_constructors, **kwarg_constructors):
    """
    changed_func_nameと結び付けられているEuslisp wraper関数を、内部で引数に対しconstructorを噛ましてから送り込むEuslisp wrapper関数へと上書きする。
    
    Args:
        pkg_obj (Eus_pkg): 型指定を行いたい関数の存在するパッケージ (eus_packなど)
        changed_func_name (str): 型指定を行いたい関数のPythonでの表記 ('some_func'など)
        arg_constructors (tuple): その関数の引数に対応したコンストラクタ関数のタプル
        kwarg_constructors (dict): その関数の引数に対応したコンストラクタ関数のディクショナリ
    
    Raises:
        TypeError: 適切なコンストラクタが渡されていないとき (package.EusListなどと表記する必要があることに注意)
        NameError: 指定された関数名をこのパッケージから発見できなかったとき
    """
    global valid_constructor
    if not all(map(lambda x: x in valid_constructor, arg_constructors)) or not all(map(lambda x: x in valid_constructor, kwarg_constructors.values())):
        raise TypeError("args shoud be one of the constructor in  {}. got {} {}".format(valid_constructor, arg_constructors, kwarg_constructors.values()))

    if changed_func_name in pkg_obj._functable:
        # func_name = pkg_obj._from_changed_to_default_func_name[changed_func_name]
        pkg_obj._functable[changed_func_name].arg_constructors = list(arg_constructors)    # tuple -> list
    elif changed_func_name in pkg_obj.func._inner_functable:
        # func_name = pkg_obj._from_changed_to_default_func_name[changed_func_name]
        pkg_obj.func._inner_functable[changed_func_name].kwarg_constructors = kwarg_constructors
    else:
        raise NameError("cannot find the function named {} in package {}.".format(changed_func_name, pkg_obj._pkg))



###############################################
#### Proxy関連 (主にEus_pkg内で使用される) ####
###############################################
class Eus_proxy(object):
    """
    全てのプロキシクラス共通の基底クラス
    """
    def __init__(self, key_num):
        self.key_num = key_num
    
    def __del__(self):
        if is_connected:    # まだ接続が保たれている時
            send_sexp("(send *ffi-symbols* :delete-symbol-pair {})".format(self.key_num), 'n')    # responseは必要ないので'n'モードで

    def to_python(self):
        """
        proxyクラスのインスタンスを、以降はpythonの純粋なオブジェクト(リストやハッシュ、文字列など)として使えるように変換する関数
        """
        return eval_foreign_vm_copy("(lookup-registered-object {})".format(self.key_num))


def _make_proxy_class(pkg, class_name, option):
    """
    make_eus_instance()から呼ばれるEus_pkgクラスコンストラクタにて、eusのクラス名のエントリからプロキシクラスを作成する際の補助関数。(コンテキストマネージャ内なのでstring-copy-modeであることが保証される)
    引数部の構築は_translate_args()を用いて行う。

    Args:
        pkg (str): Eus_pkgのインスタンス変数self._pkgがくる。大文字でパッケージ接頭語':'はつかない。'RCB4EUS'など。
        class_name (str): qualifyずみ。'RCB4LISP::someclass'など。
    Returns:
        クラスオブジェクト
    """
    field = {}
    field['class_name'] = class_name
    changed_class_name = util.change_valid_name(pkg, class_name, option)

    method_list = map(lambda s: s[1:], eval_foreign_vm_copy("(mapcar #'car (send {} :all-methods))".format(class_name)))    # [":prin1", ":super",...]のように:がメソッド名の先頭についている形でくる。python2ではmapはリストを返す
    for method_name in method_list:
        changed_method_name = util.change_valid_name(pkg, method_name, option)
        field[changed_method_name] = _make_wrapper_method(method_name)

    return type(changed_class_name, (Eus_proxy, ), field)    # type(クラス名, 親クラス, 辞書)でクラス生成    # changed_class_nameなるクラス名で登録してシンボルとして適当であるようにする。本当の名前が知りたくなったらself.class_nameに聞けばよい


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
        func_name (str): もとのEuslispでの関数名でpkg::を含む。なお、関数は内部シンボルと仮定している。(exportされてたら外部シンボルアクセス:(1個)を使わなければならない)。
    Returns:
        wrapper (function): 複数引数, キーワード引数を適切にEuslispで処理可能な形で変換しS式を送り込む処理を行う関数
    """
    def wrapper(*args, **kwargs):
        # TODO: checking phase here
    
        # mapping phase
        if len(args) == len(wrapper.arg_constructors):
            args = tuple([constructor(arg) if not isinstance(arg, Eus_proxy) and constructor is not None else arg for arg, constructor in zip(args, wrapper.arg_constructors)])    # wrapper.arg_constructorsの要素constructorがNoneであることもある。下記logging phaseを見よ
        if len(kwargs) == len(wrapper.kwarg_constructors):
            kwargs = {key:(wrapper.kwarg_constructors[key](kwargs[key]) if not isinstance(kwargs[key], Eus_proxy) else kwargs[key]) for key in wrapper.kwarg_constructors}
        
        # evaluation phase
        result = eval_foreign_vm_proxy('({}{})'.format(func_name, _translate_args(args, kwargs)))
        
        # logging phase (ここに来ている時点でevaluation phaseにてEusErrorは投げられていないことがわかる。今回の型は正当である)
        # logging時にargの要素やkwargのvalueがproxyでなかった場合(つまりデフォルトルールでの変換が行われた場合)、Noneをarg_constructorsに登録する
        # proxyのクラスからコンストラクタを特定する上で、nil-terminated cons vs. non-nil-terminated consの競合が発生する。使用頻度を考えてEusListの方であろうと決め打ちを行うことにする!
        # for ind, elm in enumerate(args):
        #     if isinstance(elm, Eus_proxy):
        #         pass
        #     else:
        #         pass
        # for ind, key in enumerate(kwargs):
        #     if isinstance(key, Eus_proxy):
        #         pass
        #     else:
        #         pass
        return result
    
    wrapper.arg_constructors = []
    wrapper.kwarg_constructors = {}
    wrapper.arg_possible_types = []
    wrapper.kwarg_possible_types = {}
    return wrapper


##################################################
#### 他言語関数引数部を作成するための補助関数 ####
##################################################

def _translate_args(t=tuple(), d=dict(), unfreeze=False):
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
    ' `(1 2) `(3 4)'

    >>> L2 = (((1,2),(3,4)),)
    >>> _translate_args(t=L2)
    ' `((1 2) (3 4))'

    >>> val1, val2 = 5, 10
    >>> _translate_args(d={"a":val1, "b":val2})
    ' :a 5 :b 10'

    >>> nested_list_dict = {"key":[1,[2,[3]]]}
    >>> _translate_args(d=nested_list_dict)
    ' :key `(1 (2 (3)))'

    >>> _translate_args(t=(1, 2), d={"x":3, "y":4})
    ' 1 2 :y 4 :x 3'
    
    >>> pi = 3.14
    >>> t, d = (("ho", "ge", pi), 1), {"x":3, "y":(4, 5)}
    >>> _translate_args(t, d)
    ' `("ho" "ge" 3.14) 1 :y `(4 5) :x 3'

    Args:
        最初の呼び出しは(args, kwargs)を想定している。argsは関数呼び出し時に*argsにバインドされたタプル, kwargsは関数呼び出し時に**kwargsにバインドされたディクショナリ。
        unfreeze=Trueのときはproxyに対応する (lookup-registered-object ...) なるS式の前に , がついてunfreezeされるようになる。(型コンストラクタの引数でさらにproxyが入ってきたときのための実装)
        t (tuple or list)
        d (dictionary)
        unfreeze (bool)
    Returns:
        str: 関数呼び出しを行うS式の引数部分に埋め込む文字列
    Raises:
        TypeError: 自動的な型変換に失敗した時 (ユーザー定義Pythonクラスインスタンスが引数としてとられていたときなど)
    """
    return '{}{}'.format(_translate_tuple(t, unfreeze), _translate_dict(d, unfreeze))


def _translate_tuple(t, unfreeze, recursive=False):
    """
    引数のタプルをよしなに変換する(シンボルは評価される)。再帰呼出しからは(quote )が付加される。
    _translate_dict関数同様、関数引数の通常引数を処理するためのものだが、リストの再帰構造を処理するのにも便利であるため転用している。

    >>> _translate_tuple(tuple(), False)
    ''

    # 空のリストを引数に関数呼び出しをするとき。tは空のリスト1要素を持つタプルなので以下のようになる。Euslispでは空リストとして評価される。
    >>> _translate_tuple(([], ), False)    
    ' `()'

    >>> _translate_tuple(("a", "b", "c"), False)
    ' "a" "b" "c"'

    >>> _translate_tuple((1, [2, 3], [4, [5, 6]]), False)
    ' 1 `(2 3) `(4 (5 6))'
    """
    s = ''
    for ind, elm in enumerate(t):
        if not recursive or ind != 0:
            s += ' '
        
        if isinstance(elm, int) or isinstance(elm, float):
            s += '{}'.format(elm)
        elif elm is None or elm is False:
            s += 'nil'
        elif elm is True:
            s += 't'
        elif isinstance(elm, str):
            s += '"{}"'.format(elm)
        # デフォルトマッピングでEuslsipのlistになるやつらのとき
        # EusListとかの内部でこの_translate_tupleを使用しているのでここを',(lookup-registered-object {})'.format(EusList(elm).key_num)とかに置き換えるのはNG
        elif isinstance(elm, (list, tuple, xrange)):
            if recursive:
                s += '({})'.format(_translate_tuple(t=elm, unfreeze=unfreeze, recursive=True))
            else:
                # s += '(quote ({}))'.format(_translate_tuple(t=elm, unfreeze=unfreeze, recursive=True))
                s += '`({})'.format(_translate_tuple(t=elm, unfreeze=True, recursive=True))    # (quote ({}))ではなくbackquote version. [EusCons, EusCons]みたいなやつらを変換する際にしっかりunfreezeしてやる
        # デフォルトマッピングでEuslispのhashtableになるやつらのとき
        elif isinstance(elm, dict):
            filling = ""
            for key, value in elm.items():
                filling += '(setf (gethash "{}" hsh) {})'.format(key, _translate_tuple((value, ), unfreeze))
            s += "(let ((hsh (make-hash-table :test #'equal))) (progn {}) hsh)".format(filling)    # Pythonでは辞書のキーは文字列であるため
        # Eus_proxyを継承したclass objectのとき
        elif type(elm) == type and "Eus_proxy" in map(lambda x: x.__name__, elm.__bases__):    # issubclass()はクラスがグローバルに登録されていないので使えない
            s += '{}'.format(elm.class_name)
        # Eus_proxyを継承したclassのinstanceのとき
        elif isinstance(elm, Eus_proxy):
            if unfreeze:
                s += ','    # 適宜, によりunfreezeするのが大切。そうでないとEusList(EusSym, EusSym)とかでこのS式が評価されぬまま残ってしまう
            s += '(lookup-registered-object {})'.format(elm.key_num)
        # 関数のとき
        elif callable(elm):
            global global_callback_key_count
            global global_callback_table
            global_callback_key_count += 1
            callback_key = "pyfunc" + str(global_callback_key_count)
            global_callback_table[callback_key] = elm
            send_sexp("(generate-callback-function {})".format(callback_key), 'n')    # マクロ呼び出し。Euslisp側に pyfunc1といった関数がdefunされる。responseは必要ないので'n'モードで。
            s += "#'{}".format(callback_key)
        else:
            raise TypeError("The type of the argument in the sequence cannot be converted automatically. got {} (type {})".format(elm, type(elm)))
    return s


def _translate_dict(d, unfreeze):
    """
    引数の辞書をよしなに変換する(シンボルは評価される)。
    これはLispにおける関数のkeyword引数であって、hashtableオブジェクトとはなんら関係がない。
    引数にハッシュテーブルのリストなどが来たからと言ってこの関数が呼ばれるわけではない。

    >>> _translate_dict(dict(), False)
    ''

    >>> _translate_dict({"a":1, "b":2}, False)
    ' :a 1 :b 2'

    >>> _translate_dict({"1": [1,2,3], "2":[4,5,6]}, False)
    ' :1 `(1 2 3) :2 `(4 5 6)'
    """
    s = ''
    for key, val in d.items():
        s += ' '
         
        if isinstance(val, int) or isinstance(val, float):
            s += ':{} {}'.format(key, val)
        elif val is None or val is False:
            s += ':{} nil'.format(key)
        elif val is True:
            s += ':{} t'.format(key)
        elif isinstance(val, str):
            s += ':{} "{}"'.format(key, val)
        # デフォルトマッピングでEuslispのlistになるやつらのとき
        elif isinstance(val, (list, tuple, xrange)):
            # s += ':{} (quote ({}))'.format(key, _translate_tuple(t=val, unfreeze=unfreeze, recursive=True))    # _translate_dictは再帰呼出しされることはないためrecursiveチェックは不要。
            s += ':{} `({})'.format(key, _translate_tuple(t=val, unfreeze=True, recursive=True))    # _translate_dictは再帰呼出しされることはないためrecursiveチェックは不要。 # backquote version
        # デフォルトマッピングでEuslispのhashtableになるやつらのとき
        elif isinstance(val, dict):
            filling = ""
            for py_key, py_value in val.items():
                filling += '(setf (gethash "{}" hsh) {})'.format(py_key, _translate_tuple((py_value, ), unfreeze))
            s += ":{} (let ((hsh (make-hash-table :test #'equal))) (progn {}) hsh)".format(key, filling)    # このkeyはキーワード引数のキー
        # Eus_proxyを継承したclass objectのとき
        elif type(val) == type and "Eus_proxy" in map(lambda x: x.__name__, val.__bases__):
            s += ':{} {}'.format(key, val.class_name)
        # Eus_proxyを継承したclassのinstanceのとき
        elif isinstance(val, Eus_proxy):
            unfreeze_option = ',' if unfreeze else ''
            s += ':{} {}(lookup-registered-object {})'.format(key, unfreeze_option, val.key_num)
        # 関数のとき
        elif callable(val):
            global global_callback_key_count
            global global_callback_table
            global_callback_key_count += 1
            callback_key = "pyfunc" + str(global_callback_key_count)
            global_callback_table[callback_key] = val
            send_sexp("(generate-callback-function {})".format(callback_key), 'n')    # マクロ呼び出し。Euslisp側に pyfunc1といった関数がdefunされる。responseは必要ないので'n'モードで。
            s += ":{} #'{}".format(key, callback_key)
        else:
            raise TypeError("The type of the value in the dictionary cannot be converted automatically. got {} (type {})".format(val, type(val)))
    return s



if __name__ == "__main__":
    import doctest
    doctest.testmod()
