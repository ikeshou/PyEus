#!/usr/bin/env python2
# coding: utf-8

"""
ENTRY POINT OF THE WHOLE PROGRAM
You can use all functions inside the library just to put 'import pyeus' at the beginning of your program
"""

import os, sys
import pyeus
from pyeus import EusSym, EusFuncSym, EusCons, EusList, EusPlist, EusArray, EusVec, EusIntVec, EusFloatVec, EusBitVec, EusPath
from pyeus import my_assert
from pyeus import close_all_sockets, kill_eus_process


def mainloop():
    print("--python code--\n")

    try:
        print("start integration test...!")


        #### :test package
        src = os.path.join(os.getcwd(), "functions_for_integration_test.l")
        pyeus.load_library(src=src, module_name=":test")

        # pyeus.load_library(src=src, module_name="'test")
        # pyeus.load_library(src=src, module_name="TEST")
        # pyeus.load_library(src=src, module_name="test")    # 混乱するので非推奨だが一応動くようにしてあげている

        
        eus_pack = pyeus.make_eus_instance(pkg=":test")    # option="underscore"
        # eus_pack = pyeus.make_eus_instance(pkg="'test")
        # eus_pack = pyeus.make_eus_instance(pkg="TEST")
        # eus_pack = pyeus.make_eus_instance(pkg="test")    # 混乱するので非推奨だが一応動くようにしてあげている
        
        # ----test0----
        print("# test0 -> instance variables of Eus_pkg")
        my_assert(eus_pack._pkg, "TEST")
        my_assert(eus_pack._option, "underscore")
        my_assert(pyeus.eus_package_table.has_key("TEST"), True)
        
        # for debug
        # print(eus_pack.__dict__)

        # ----test1----
        # グローバル変数にアクセス
        print("# test1 -> global variables in Euslisp")
        my_assert(eus_pack._my_global_str_.to_python(), 'this is my global variable')
        my_assert(eus_pack.const_pi, 3.1415)


        # ----test2----
        # ユーザー定義関数にアクセス
        print("# test2 -> user-defined functions in Euslisp")
        # 引数なし
        my_assert(eus_pack.no_arg_ret_nil(), None)
        my_assert(eus_pack.no_arg_ret_int(), 1)
        my_assert(eus_pack.no_arg_ret_float(), 1.0)
        my_assert(eus_pack.no_arg_ret_str().to_python(), 'returned string')
        my_assert(eus_pack.no_arg_access_global().to_python(), 'this is my global variable')
        my_assert(eus_pack.no_arg_ret_list().to_python(), [1,2,3,4])
        my_assert(eus_pack.ret_list_with_nil().to_python(), [1,[2,[3,None]]])
        my_assert(eus_pack.no_arg_ret_int_vec().to_python(), [1,2,3])
        my_assert(eus_pack.no_arg_ret_float_vec().to_python(), [1.0,2.0,3.0])
        my_assert(eus_pack.no_arg_ret_bit_vec().to_python(), [0,0,0,0])
        my_assert(eus_pack.no_arg_ret_mat().to_python(), [[1.0,2.0,3.0],[4.0,5.0,6.0]])
        my_assert(eus_pack.no_arg_ret_arr().to_python(), [[[1,2],["a","b"]],[[3,4],["c","d"]]])
        my_assert(eus_pack.no_arg_ret_empty_hash().to_python(), {})
        my_assert(eus_pack.no_arg_ret_a_1_hash().to_python(), {"a":1})
        
        # 1引数
        my_assert(eus_pack.double(5), 10)
        my_assert(eus_pack.double(5.0), 10.0)
        my_assert(eus_pack.list_arg_ret_doubled_list([1, 2, 3, 4, 5]).to_python(), [2,4,6,8,10])
        my_assert(eus_pack.list_arg_ret_doubled_list((1, 2, 3, 4, 5)).to_python(), [2,4,6,8,10])
        my_assert(eus_pack.original_nest([1,2,3]).to_python(), [1,[2,3]])
        my_assert(eus_pack.original_nest([1,2,[3]]).to_python(), [1,[2,[3]]])
        my_assert(eus_pack.str_arg_ret_0("some text here").to_python(), "s")

        # 複数引数
        my_assert(eus_pack.three_args_ret_sum(1, 2, 3), 6)
        a, b, c = 0, 1, 2
        my_assert(eus_pack.three_args_ret_sum(a, b, c), 3)

        my_assert(eus_pack.num_list_arg_ret_sum(1, [2, 3, 4, 5]), 15)
        my_assert(eus_pack.num_list_arg_ret_sum(1, (2, 3, 4, 5)), 15)
        n, l = 10, [20, 30]
        my_assert(eus_pack.num_list_arg_ret_sum(n, l), 60)
        a, b = 10, 20
        my_assert(eus_pack.num_list_arg_ret_sum(a, (b, 30)), 60)

        my_assert(eus_pack.two_list_arg_ret_append([a, b], (30, 40)).to_python(), [10,20,30,40])

        # キーワード引数
        my_assert(eus_pack.keyword_args_ret_int(a=1, b=2, c=3), 10)    # 3a+2b+c
        my_assert(eus_pack.keyword_args_ret_int(c=3, b=2, a=1), 10)
        x = 3
        my_assert(eus_pack.keyword_args_ret_int(a=1, b=2, c=x), 10)
        
        # 複数引数とキーワード引数の合わせ技
        my_assert(eus_pack.int_list_keyword_args_ret_int(1, [2, 3, 4], a=1, b=2, c=3), 20)    # 1 + 2 + 3 + 4 + 3a +2b +c


        # func_接頭語がついたものは常に登録されていることの確認
        my_assert(eus_pack.func_int_list_keyword_args_ret_int(1, [2,3,4], a=1, b=2, c=3), 20)



        # ----test3----
        # 組み込み関数にアクセス
        print("# test3 -> built-in functions in Euslisp")
        # 引数なし
        eus_system = pyeus.make_eus_instance(pkg="'system")
        my_assert(type(eus_system.list_all_classes().to_python()), list)     # systemパッケージに入っている

        # 1引数
        my_assert(eus_pack.reverse([1,2,3]).to_python(), [3,2,1])    # lispパッケージに入っている

        # 複数引数
        my_assert(eus_pack.eq(1, 1.0), None)

        # 複数引数とキーワード引数の合わせ技
        my_assert(eus_pack.position(3, (1,2,3,4,5,4,3,2,1), count=1) ,2)
        my_assert(eus_pack.position(3, (1,2,3,4,5,4,3,2,1), count=2) ,6)

        # func_接頭語がついたものは常に登録されていることの確認
        my_assert(eus_pack.func_reverse([1,2,3]).to_python(), [3,2,1])

        # 関数名とシンボル名がかぶっているものは、python側ではfunc_接頭語がついたものしか関数として登録されていないことの確認
        my_assert(eus_pack.func_integer_vector(10,20,30).to_python(), [10,20,30])
        try:
            eus_pack.integer_vector(10,20,30).to_python()
        except TypeError:    # func_がないとEus_proxyのinteger_vectorとして処理される。Eus_proxyのコンストラクタに複数引数を与えようとするのでtype errorとなる。(1つだとここではじかれないのがなんともいえぬ)
            pass

        # ----test4----
        # ユーザー定義クラスにアクセス
        print("# test4 -> user-defined class in Euslisp")
        # しっかりプロキシクラスが作成されているか
        my_assert(type(eus_pack.person), type)
        
        # インスタンスに対してメソッドコールが行えるか
        murabitoA = eus_pack.instantiate(eus_pack.person)
        murabitoA.set_name("ikezaki")
        murabitoA.set_age(20)
        my_assert(murabitoA.print_hi().to_python(), "Hi,ikezaki")
        my_assert(murabitoA.get_name().to_python(), "ikezaki")
        my_assert(murabitoA.get_age(), 20)
        my_assert(murabitoA.incf_age(), 21)
        my_assert(murabitoA.list_name_age().to_python(), ["ikezaki",21])

        murabitoB = eus_pack.make_instance(eus_pack.person, name="koko", age=10)
        my_assert(murabitoB.get_name().to_python(), 'koko')
        my_assert(murabitoB.get_age(), 10)


        # ----test5----
        # 組み込みクラスにアクセス
        print("# test5 -> built-in class in Euslisp")
        eus_pack.instantiate(eus_pack.integer_vector, 4)
        my_assert(eus_pack.instantiate(eus_pack.integer_vector, 4).to_python(), [0,0,0,0])
        my_assert(eus_pack.subclassp(eus_system.integer_vector, eus_system.vector), True)
        # インスタンスに対してアトリビュートアクセスが行えるか
        int_vec_instance = eus_pack.instantiate(eus_pack.integer_vector, 4)    # 関数の方はfunc_integer_vectorのみで登録しているのでいけるはず
        my_assert(int_vec_instance.slots().to_python(), [['length',4]])


        # ----test6----
        # 明示的な型変換が必要な関数アクセスのテスト
        # 引数coordinates, line, plane, surronding-box, streo-viewing? これらは普通に関数呼び出しで作成すればよい。euslispの方でもリテラル表記がないのでそれで充分。
        print("# test6 -> explicit type conversion")

        # 引数symbol (str to symbol)
        my_assert(eus_pack.access_symbol(EusSym("TEST::const-pi")), 3.1415)
        
        # 引数function symbol (str to symbol)
        my_assert(eus_pack.sort([-2,-5,11,7,3,-13], EusFuncSym("<"), key=EusFuncSym("abs")).to_python(), [-2,3,-5,7,11,-13])

        # 引数cons (list/tuple to cons)
        # 2.7ではrangeはlistを返す。xrangeがrangeオブジェクトを返すがこいつはスライスなどを受け付けず限定的な処理しか許されない。3.7からはサポートしても良いかも。
        my_assert(eus_pack.dot_cons_p(EusCons([1, 2, 3])), True)

        # 引数list (list/tuple to list)
        tmp = 1
        my_assert(eus_pack.list_arg_ret_doubled_list(EusList([tmp, 2 ,3])).to_python(), [2, 4, 6])

        # 引数plist (2*n-list/2*n-tuple to plist)
        p_object = eus_pack.instantiate(eus_pack.propertied_object)
        p_object.plist(EusPlist([[10, 100], [20, 200], [30, 300]]))
        my_assert(p_object.get(20), 200)

        # 引数array (list/tuple to array)
        my_assert(eus_pack._d_array_arg_ret_0_0(EusArray([[4, 3], [2, 1]])), 4)

        # 引数vector (list/tuple to array)
        my_assert(eus_pack.vector_arg_ret_0(EusVec([['a', 'b'], ['c', 'd']])).to_python(), ["a", "b"])

        # 引数int-vector (list/tuple to integer vector)
        my_assert(eus_pack.int_vec_arg_ret_0(EusIntVec([1, 2, 3, 4])), 1)
        my_assert(eus_pack.int_vec_arg_ret_0(EusFloatVec([1.0, 2.0, 3.0, 4.0])), None)

        # 引数float-vector (list/tuple to float vector)
        my_assert(eus_pack.float_vec_arg_ret_0(EusFloatVec([1.0, 2.0, 3.0, 4.0])), 1.0)
        my_assert(eus_pack.float_vec_arg_ret_0(EusIntVec([1, 2, 3, 4])), None)

        # 引数bit-vector (0-1-list/0-1-tuple to bit-vector)
        my_assert(eus_pack.bit_vec_arg_ret_0(EusBitVec([1, 0, 0, 0])), 1)

        # 引数pathname (str to pathname) 
        # 3.4からはpathlibにPathがあるのでそちらもサポートしてもよいかも
        my_assert(eus_pack.pathname_arg_ret_file_str(EusPath("/opt/ros/melodic/env.sh")).to_python(), "ENV")


    except Exception:
        err_value = sys.exc_info()[1]
        print >> sys.stderr, err_value    

    finally:
        print("\n")
        print("*" * 16)
        print("total passed: {}".format(my_assert.count_passed))
        print("total error: {}".format(my_assert.count_error))
        print("*" * 16)
        print("\n")
        print("--python code--")
        close_all_sockets()
        kill_eus_process()



if __name__ == "__main__":
    print("this is a mainloop.")
    mainloop()

