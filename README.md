<!-- ctrl+shift+v to see preview,  ctrl+shift+i to arrange tables in VS Code-->

# PyEus
Python library that builds Foreign Function Interface (what is called [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) between [Python](https://www.python.org/) and [Euslisp](https://github.com/euslisp/EusLisp).
<br>
<br>


## :black_nib: Author
***
Ikezaki Shoya <ikezaki@csg.ci.i.u-tokyo.ac.jp>

## :computer: Platform and  Languages
***
PyEus supports Linux only.  Though you can probably use PyEus library on your Mac OS X, it is not supported.

PyEus supports Python 2.7 and EusLisp 9.26 (1.2.1). (After Euslisp supports ROS2, Python 3.7 will also be supported.)

## :warning: Caution
***
Since the library is still an experimental one, please be sure that the whole architecture of the library might be changed. Moreover, it has not yet packaged :)

If you just want to see how it works or just want to try it, please move on to the next step! If you want to see the explanation of each file in the directory first, go to the [Description of each file](#description_of_each_file) section.
<br>
<br>


## Getting started
***
1. clone this package
```
git clone https://github.com/ikeshou/PyEus.git
```

2. place `pyeus.py`, `pyeus_util.py`, `eus_server.l` `bugfixed_hash.l`, `inference_eusfunc.l` and `builtin_type_data.l` inside the same directory that your main python program is placed.

3. You can call euslisp functions from your python program after `import pyeus` !
```python
# Python user code
import pyeus

# some codes here
```
<br>

## Tutorial
***
### 1. Using variables and functions  

Suppose that you want to use `originalVariable` and `my-awesome-eus-function` in the "ROBOP" package written in Euslisp from Python.  
The first thing you have to do is import pyeus library and load the euslisp file by using `load_library` function where `originalVariable` and `my-awesome-eus-function` is written.  
Though the module_name argument is optional, please use the argument since it works as "include guard".  
```python
# Python user code
import pyeus

pyeus.load_library(src="/Users/ikezakishoya/PyEus/eus_script.l", module_name=":robop")
```

The next step you should do is making the euslisp package object inside the python by using `make_eus_instance` function.
The keyword notation or quote notation or uppercase string notation is allowed as pkg argument.  
```python
# Python user code
robop = pyeus.make_eus_instance(pkg=":robop")
```
<br>

Now that you can use any Euslisp variables/functions/classes as you want! (Of course, you can use Euslisp built-in variables/functions/classes.) You can call Euslisp functions mostly as same as you use fucntions in a Python module.    
Before moving on to the precise explanation, please remember there are some rules:  

- Since using some symbols (`"+-*/@$%^&=<>~.;"`) is prohibited in Python, these characters in a package/variable/function/class name is automatically translated to underscore(`"_"`).  
- Because the namespace of the function is separated from other symbols in Euslisp, sometimes there are functions whose name is as same as the name of a variable or a class (e.g. `vector`). In that case, the prefix `"func_"` is automatically added to the name of the function in Python in order to tell one from the other. Conversely, you can access variables, functions and classes in Euslisp by completely the same name inside Python except for these cases.
- You can also access the functions of which name conflicts the name of a variable or a class in such way: `robop.func.vector`. 
- All of the keyword argument in Python is automaticaly translated to `&key` argument. 
- If the Euslisp function returns the object that is integer or float or null or t, the result is integer or float or None or True (Python built-in object)). (If you want to know other cases, see the next section!)  

```python
var = robop.originalVariable
result = robop.my_awesome_eus_function(*args, **kwargs)
```

- Importantly, symbols that is passed as an argument is evaluated as a **Python object**.  
For example,  
```lisp
;; suppose you want to use a ~~strange~~ Euslisp function below
(defun complicated~func (num1, num2, num3, &key param name)
  (if (string= s "py")
      (+ num1 num2 num3 param)))
```
```python
# Python user code
a, b = 1, 10    # of course, these are python onject
n = robop.complicated_func(a, b, 100, param=1000, name="py")    # a and b are evaluated in Python!
``` 
Running the code above is as same as running the following code in Euslisp.  
```lisp
(robop::complicated~func 1 10 100 :param 1000 :name "py")
```
As a result, the `n` will be `1111` (Python built-in object "integer").
<br>  
<br>

### 2. Using (more and more) data types 
<div id=default_rule>
What if you want to use functions of which argument is not an object explained above such as number or string but a list or vector or array?  
Basically, some Python data types are converted to the corresponding Euslisp data types automatically (the literal expression of Euslisp object are made and passed as arguments):  

(From Python to Euslisp)  

| Object (Python)     | Object (Euslisp)     | Description                                  |
| :------------------ | :------------------- | :------------------------------------------- |
| integer             | integer              |                                              |
| float               | float                |                                              |
| None, False         | null                 |                                              |
| True                | t                    |                                              |
| string              | string               |                                              |
| list, tuple, xrange | list                 |                                              |
| dictionary          | hash-table           |                                              |
| function            | *Python-like object* | see [4. Using callback functions](#callback) |
| any other object    | ×                    | TypeError                                    |
</div>
<br>

<div id=type_description>

If you want to pass an object as an argument that is not listed on Type(Euslisp) column of the tabale, use *explicit type description*:  

| Constructor (Pyhon) | Args (Python) | Object (python)                   | Object (Euslisp)  | Description                                                                |
| :------------------ | :------------ | :-------------------------------- | :---------------- | :------------------------------------------------------------------------- |
| EusSym              | string        | <class 'pyeus.symbol'>            | symbol            | if invalid characters are in argument, raises SyntaxError                  |
| EusFuncSym          | string        | <class 'pyeus.compiled_code'>     | function symbol   | if invalid characters are in argument, raises SyntaxError                  |
| EusStr              | string        | <class 'pyeus.string'>            | string            |                                                                            |
| EusCons             | list, tuple   | <class 'pyeus.cons'>              | (dotted) list     |                                                                            |
| EusPlist            | list, tuple   | <class 'pyeus.propertied_object'> | propertied object | if the shape of the argument is not (n, 2), raises SyntaxError             |
| EusList             | list, tuple   | <class 'pyeus.cons'>              | list              |                                                                            |
| EusIntVec           | list, tuple   | <class 'pyeus.integer_vector'>    | integer-vector    | if there is an element in argument that is not integer, raises SyntaxError |
| EusFloatVec         | list, tuple   | <class 'pyeus.float_vector'>      | float-vector      | if there is an element in argument that is not float, raises SyntaxError   |
| EusBitVec           | list, tuple   | <class 'pyeus.bit_vector'>        | bit-vector        | if there is an element in argument that is nor 0 and 1, raises SyntaxError |
| EusVec              | list, tuple   | <class 'pyeus.vector'>            | vector            |                                                                            |
| EusArray            | list, tuple   | <class 'pyeus.array'>             | array             |                                                                            |
| EusHash             | dictionary    | <class 'pyeus.hash_table'>        | hash-table        |                                                                            |
| EusPath             | string        | <class 'pyeus.pathname'>          | pathname object   |                                                                            |

By using the *Euslisp-like object constructor*, you can declare the type of an argument explicitly.  
```lisp
;; suppose you want to use an Euslisp function below
;; if a list is passed as an argument, Python raises EusError("irteus 0 error: array expected")
(defun zero-zero-elm-of-array (2d-arr)
  (if (= (length (array-dimensions 2d-arr)) 2)
      (aref 2d-arr 0 0)))
```
```python
# Python user code
result = robop.zero_zero_elm_of_array(EusArray([[1,2],[3,4]]))
print(result)    # 1
```
</div>
<br>

<div id=euslisp_to_python>
Then, let's take a look at what will happen when the Euslisp returns the object that is not integer or float or null or t.  

In short, it returns *Euslisp-like object* such as EusArray.  

(From Euslisp to Python)  

| Object (Euslisp)                | Object (Python)                   | after `to_python` conversion | Description                                                               |
| :------------------------------ | :-------------------------------- | :--------------------------- | :------------------------------------------------------------------------ |
| integer                         | integer                           | ×                            | AttributeError                                                            |
| float                           | float                             | ×                            | AttributeError                                                            |
| nil                             | None                              | ×                            | AttributeError                                                            |
| t                               | True                              | ×                            | AttributeError                                                            |
| symbol                          | <class 'pyeus.symbol'>            | string                       |                                                                           |
| function symbol                 | <class 'pyeus.compiled_code'>     | string                       | `to_python` returns `"#<compiled-code #...>"`, not a function-symbol-name |
| string                          | <class 'pyeus.string'>            | string                       |                                                                           |
| (dotted) list                   | <class 'pyeus.cons'>              | list                         |                                                                           |
| propertied-object               | <class 'pyeus.propertied_object'> | list                         |                                                                           |
| list                            | <class 'pyeus.cons'>              | list                         |                                                                           |
| integer-vector                  | <class 'pyeus.integer_vector'>    | list                         |                                                                           |
| float-vector                    | <class 'pyeus.float_vector'>      | list                         |                                                                           |
| bit-vector                      | <class 'pyeus.bit_vector'>        | list                         |                                                                           |
| vector                          | <class 'pyeus.vector'>            | list                         |                                                                           |
| array                           | <class 'pyeus.array'>             | list                         |                                                                           |
| hashtable                       | <class 'pyeus.hash_table'>        | dictionary                   |                                                                           |
| pathname object                 | <class 'pyeus.pathname'>          | string                       |                                                                           |
| any other instance of 'MyClass' | <class 'pyeus.MyClass'>           | string                       | `to_python` returns the string notation of the instance                   |
| class (not an instance)         | <class 'pyeus.metaclass'>         | string                       | `to_python` returns the string notation of the class                      |



Going back to the first example in section 1,  
```python
# Python user code
result = robop.my_awesome_eus_function(*args, **kwargs)
```
When the `my-awesome-eus-function` returns a list in Euslisp, the `result` is an EusList object. Since this itself is not a Python built-in object, you have to convert to the familiar data types by `to_python` method if you want to handle this object as an ordinary python object.  
```python
# Python user code
print(result)    # <class 'pyeus.cons'>, Python proxy object to Euslisp list
L = result.to_python()    # list, Python built-in object
isinstance(L, list)    # True
```


*Euslisp-like-object* behaves as if it were an Euslisp object.  
For example,  
```python
# Python user code
arr = robop.make_array([2,2])    # arr is an EusArr object
robop.setf(robop.aref(arr, 0, 0), 100)    # assign directly
print(arr.to_python)    # [[100, None], [None, None]] (the 0-0 elment is changed)
```
</div>
<br>

### 3. Using classes  

You can access the user-defined class and built-in class in the same way.

(accessing the built-in class)  
```python
iv = robop.instantiate(robop.integer_vector, 4)    # integer-vector is the built-in class in Euslisp
iv.to_python()    # [0,0,0,0]
```
<br>

(accessing the user-defined class)
```lisp
(defclass animal :super object :slots (name age))
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
```
If you want to use the Euslisp user-defined class like above in Python, the code is like following:
```Python
murabito = robop.make_instance(robop.animal, name="Tanukichi", age=46)
murabito.get_name().to_python()    # "Tanukichi"
murabito.incf_age()
murabito.get_age()    # 47
```

<br>
<br>

<div id=callback>

### 4. Using callback functions
***
You can also uses Euslisp functions that needs callback fuctions from Python! 

```Python
L = robop.mapcar(lambda x: robop.cadr(x), [[1,2,3], [4,5,6], [7,8,9]])
L.to_python()    # [2,5,8]
```
The precise behavior of the code above is following:
1. Since the `robop.mapcar` is *Euslisp-like object*, these argument function, list are converted to *Python-like object* (works as a pointer to the python function) and Euslisp list (see [From Python to Euslisp](#default_rule)).
2. In Euslisp, `mapcar` tries to apply *Python-like object* to each element of Euslisp list `((1 2 3) (4 5 6) (7 8 9))`, that is, `(1 2 3)` and `(4 5 6)` and `(7 8 9)`.
3. Since the callback function is *Python-like object* (pointer to the python functions), Euslisp orders Python to apply callback function with `(1 2 3)` and `(4 5 6)` and `(7 8 9`. These Euslisp list is also converted to \<class pyeus.cons\> (see [From Euslisp to Python](#euslisp_to_python)).
4. Python applies callback function `lambda x: robop.cadr(x)` with \<class pyeus.cons\> of `(1 2 3)` and `(4 5 6)` and `(7 8 9)`, then returns the result `2` and `5` and `8`. Since they are Python integers, they are converted to Euslisp integers (see [From Python to Euslisp](#default_rule)).
5. In Euslisp, `mapcar` tries to gather the result in one list, that is, `(2 5 8)`. Since this is the Euslisp list, the Python function `robop.mapcar` finally returns the \<class pyeus.cons\> of `(2 5 8)` (`L`).
6. to_python() translates `(2 5 8)` to `[2,5,8]` (see [From Euslisp to Python](#euslisp_to_python)).


<br>

Remember you **cannot** write the lambda function that does not handle *Euslisp-like object* if your higher-order function in Euslisp needs callback functions that takes Euslisp any object other than integer, float, nil and t. 

For example, 
```Python
# allowed
robop.mapcar(lambda x: x+1, [1,2,3])
```
is allowed. However, 
```Python
# * NOT ALLOWED *
robop.mapcar(lambda x: x[1], [[1,2,3], [4,5,6], [7,8,9]])
```
is not allowed since the Python tries to apply callback function `lambda x: x[1]` with \<class pyeus.cons\> of `(1 2 3)` and `(4 5 6)` and `(7 8 9)`. 

<br>

Of cource, you can pass an `<class pyeus.compiled_code>` object directly to the functions that needs callback functions. Following code is as same as the first code.
```Python
proxy_func_object = robop.cadr    # <class pyeus.compiled_code>
robop.mapcar(proxy_func_object, [[1,2,3], [4,5,6], [7,8,9]])
```
<br>
<br>

<div id=type_description_interface>

### 5. (Cool) Type description interface
***
If you use the foreign functions that only needs [default type mappping](#default_rule) like `list-concatenate` function below, you can easily call the foreign functions with python literal as arguments.
```lisp
(in-package "TEST")

(defun list-concatenate (&rest list-of-list)
  (mapcan #'append list-of-list))
```

However, if you use the foreign functions that needs [explicit type description])(#type_description) like `sum-of-0-0-element` function below, you have to write `EusArray` constructor many times. (Moreover, it often occurres since dymamic typed languages such as Python and Euslisp have various types.)
```Python
TEST.list_concatenate([[1,2,3],[4,5,6],[7,8,9]]).to_python()
# [1,2,3,4,5,6,7,8,9]

TEST.list_concatenate([['a','b','c'],['d','e','f']]).to_python()
# ['a','b','c','d','e','f']
```

```lisp
(in-package "TEST")

(defun sum-of-0-0-element (arr1 arr2)
  (+ (aref arr1 0 0) (aref arr2 0 0)))
```

```Python
TEST.sum_of_0_0_element(EusArray([[1,2],[3,4]]), EusArray([[5,6],[7,8]]))    # 6
TEST.sum_of_0_0_element(EusArray([[9,10],[11,12]]), EusArray([[13,14],[15,16]]))    # 22
TEST.sum_of_0_0_element(EusArray([[17,18],[19,20]]), EusArray([[21,22],[23,24]]))    # 38
```

Are you getting tired of writing the [explicit type description](#type_description)? 
PyEus library provides three distict systems that save your cost writing the explicit type descriptions:
- set_params() helper function
- logging system
- type inference system 

<br>

#### 5.1. set_params() helper function
Explicit type discription such as `EusVec` or `EusArray` is the type description to the value. Contrary to this, `set_params()` is the type description to the foreign function. After you register the type information like below, you can easily call the foreign function with python literal as arguments. 

```Python
set_params("sum-of-0-0-element", EusArray, EusArray)

TEST.sum_of_0_0_element([[1,2],[3,4]], [[5,6],[7,8]])    # 6
TEST.sum_of_0_0_element([[9,10],[11,12]], [[13,14],[15,16]])    # 22
TEST.sum_of_0_0_element([[17,18],[19,20]], [[21,22],[23,24]])    # 38
```

#### 5.2. logging system

(Now writing)

<div id=type_inference_system>

#### 5.3. type inference system

(Now writing)

<br>
<br>



<div id=description_of_each_file>

## Description of each file
***
- `README.md`: here
- `pyeus.py`: core program of the Python/Euslisp FFI
- `eus_server.l`: core program of the Python/Euslisp FFI
- `pyeus_util.py`: helper functions for `pyeus.py`
- `bugfixed_hash.l`: bug fixed hash-table for Euslisp
- `inference_eusfunc.l`: type inference system for Euslisp functions (See [Type description interface](#type_description_interface) and [type inference system](#type_inference_system).)

The followings are the unit test files. (For Python codes, doctest is written in `pyeus.py` and `pyeus_util.py`.)

- `test_eus_server.l`: unit test for `eus_server.l`
- `test_inference_eusfunc.l`: unit test for `inference_eusfunc.l` 

The following is the integration files.
- `integration_test.py`

<br>


## Notes
***
This folder is migrated from ikeshou/CSG_research repository (private, for research use).
Do not worry about the small number of commitment!
<br>
Now implementing: 
- Type inference system in type description inteface
- Specific debugger for Python and Euslisp FFI

It's gonna be awesome :wink:
