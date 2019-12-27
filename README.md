# PyEus
Python library that builds Foreign Function Interface between [Python](https://www.python.org/) and [Euslisp](https://github.com/euslisp/EusLisp).
<br>
<br>


## :black_nib: Author
Ikezaki Shoya <ikezaki@csg.ci.i.u-tokyo.ac.jp>
<br>

## :computer: Platform
PyEus supports Linux only.  
Though you can probably use PyEus library on your Mac OS X, it is not supported.
<br>

## :warning: Caution
Since the library of this version is an experimental one, please be sure that the whole architecture of the library might be changed.  
Moreover, the way of bootstrapping has not yet refined :)  
If you just want to see how it works or just want to try my library, please move on to the next step!
<br>  
<br>


## Getting started
***
1. clone this package
```
git clone https://github.com/ikeshou/PyEus.git
```

2. place `pyeus.py` and `eus_server.l` inside the same directory that your main python program is placed.

3. You can call euslisp functions from your python program after `import pyeus` !
```python
# Python user code
import pyeus

# some codes here
```
  
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

- Since using some symbols("+-*/@$%^&=<>~.;") is prohibited in Python, these characters in a package/variable/function/class name is automatically translated to underscore("_").  
- Because the namespace of the function is separated from other symbols in Euslisp, sometimes there are functions whose name is as same as the name of a variable or a class. In that case, the prefix "func_" is automatically added to the name of the function in Python in order to tell one from the other. Conversely, you can access variables, functions and classes in Euslisp by completely the same name inside Python except for these cases.  
- If the Euslisp function returns the object that is integer or float or null or t, the result is integer or float or None or True (Python built-in object)). (If you want to know other cases, see the next section!)  

```python
var = robop.originalVariable
result = robop.my_awesome_eus_function(*args, **kwargs)
```

- Importantly, symbols that is passed as an argument is evaluated as a **Python object**.  
For example,  
```lisp
;; suppose you want to use a ~~strange~~ Euslisp function below
(defun complicated~func (num1, num2, num3, &key param s)
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
(robop::func 1 10 100 :param 1000 :name "py")
```
As a result, the `n` will be 1111 (Python built-in object "integer").
<br>  
<br>

### 2. Using (more and more) data types 

What if you want to use functions of which argument is not an object explained above such as number or string but a list or vector or array?  
Basically, some Python data types are converted to the corresponding Euslisp data types automatically (the literal expression of Euslisp object are made and passed as arguments):  
(From Python to Euslisp)  

| Object (Python)     | Object (Euslisp) | Description                                             |
| :------------------ | :--------------- | :------------------------------------------------------ |
| integer             | integer          |                                                         |
| float               | float            |                                                         |
| None, False         | null             |                                                         |
| True                | t                |                                                         |
| string              | string           |                                                         |
| list, tuple, xrange | list             |                                                         |
| dictionary          | ×                | TypeError (there is no literal of hashtable in Euslisp) |
| any other object    | ×                | TypeError                                               |

If you want to pass an object as an argument that is not listed on Type(Euslisp) column of the tabale, use *explicit type declaration*:  

| Constructor (Pyhon) | Args (Python) | Object (python)                | Object (Euslisp) | Description                                                                |
| :------------------ | :------------ | :----------------------------- | :--------------- | :------------------------------------------------------------------------- |
| EusSym              | string        | <class 'pyeus.symbol'>         | symbol           | if invalid characters are in argument, raises SyntaxError                  |
| EusFuncSym          | string        | <class 'pyeus.compiled_code'>  | function symbol  | if invalid characters are in argument, raises SyntaxError                  |
| EusStr              | string        | <class 'pyeus.string'>         | string           |                                                                            |
| EusCons             | list, tuple   | <class 'pyeus.cons'>           | (dotted) list    |                                                                            |
| EusPlist            | list, tuple   | <class 'pyeus.cons'>           | property list    | if the shape of the argument is not (n, 2), raises SyntaxError             |
| EusList             | list, tuple   | <class 'pyeus.cons'>           | list             |                                                                            |
| EusIntVec           | list, tuple   | <class 'pyeus.integer_vector'> | integer-vector   | if there is an element in argument that is not integer, raises SyntaxError |
| EusFloatVec         | list, tuple   | <class 'pyeus.float_vector'>   | float-vector     | if there is an element in argument that is not float, raises SyntaxError   |
| EusBitVec           | list, tuple   | <class 'pyeus.bit_vector'>     | bit-vector       | if there is an element in argument that is nor 0 and 1, raises SyntaxError |
| EusVec              | list, tuple   | <class 'pyeus.vector'>         | vector           |                                                                            |
| EusArray            | list, tuple   | <class 'pyeus.array'>          | array            |                                                                            |
| EusHash             | dictionary    | <class 'pyeus.hash_table'>     | hashtable        |                                                                            |
| EusPath             | string        | <class 'pyeus.pathname'>       | pathname object  |                                                                            |

By using the *Euslisp-like object constructor*, you can declare the type of an argument explicitly.  
```lisp
;; suppose you want to use an Euslisp function below
;; if a list is passed as an argument, it causes an error ("irteus 0 error: array expected")
(defun zero-zero-elm-of-array (2d-arr)
  (if (= (length (array-dimensions 2d-arr)) 2)
      (aref 2d-arr 0 0)))
```
```python
# Python user code
result = robop.zero_zero_elm_of_array(EusArray([[1,2],[3,4]]))
print(result)    # 1
```
<br>


Then, let's take a look at what will happen when the Euslisp returns the object that is not integer or float or null or t.  

In short, it returns *Euslisp-like object* such as EusArray.  

(From Euslisp to Python)  

| Object (Euslisp)                | Object (Python)                | after `to_python` conversion | Description                                                 |
| :------------------------------ | :----------------------------- | :--------------------------- | :---------------------------------------------------------- |
| integer                         | integer                        | ×                            | AttributeError                                              |
| float                           | float                          | ×                            | AttributeError                                              |
| nil                             | None                           | ×                            | AttributeError                                              |
| t                               | True                           | ×                            | AttributeError                                              |
| symbol                          | <class 'pyeus.symbol'>         | string                       |                                                             |
| function symbol                 | <class 'pyeus.compiled_code'>  | string                       | returns "#<compiled-code #...>", not a function-symbol-name |
| string                          | <class 'pyeus.string'>         | string                       |                                                             |
| (dotted) list                   | <class 'pyeus.cons'>           | list                         |                                                             |
| property list                   | <class 'pyeus.cons'>           | list                         |                                                             |
| list                            | <class 'pyeus.cons'>           | list                         |                                                             |
| integer-vector                  | <class 'pyeus.integer_vector'> | list                         |                                                             |
| float-vector                    | <class 'pyeus.float_vector'>   | list                         |                                                             |
| bit-vector                      | <class 'pyeus.bit_vector'>     | list                         |                                                             |
| vector                          | <class 'pyeus.vector'>         | list                         |                                                             |
| array                           | <class 'pyeus.array'>          | list                         |                                                             |
| hashtable                       | <class 'pyeus.hash_table'>     | dictionary                   |                                                             |
| pathname object                 | <class 'pyeus.pathname'>       | string                       |                                                             |
| any other instance of 'MyClass' | <class 'pyeus.MyClass'>        | string                       |                                                             |
| class (not an instance)         | <class 'pyeus.metaclass'>      | string                       |                                                             |



Going back to the first example in section 1,  
```python
# Python user code
result = robop.my_awesome_eus_function(*args, **kwargs)
```
When the `my-awesome-eus-function` returns a list in Euslisp, the `result` is an EusList object. Since this itself is not a Python built-in object, you have to convert to the familiar data types by `to_python` method if you want to handle this object as an ordinary python object.  
```python
# Python user code
L = result.to_python()
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
<br>

### 3. Using classes  
You can access the user-defined class and built-in class in the same way.  
```python
iv = robop.instantiate(robop.integer_vector, 4)    # integer-vector is the built-in class in Euslisp
iv.to_python()    # [0,0,0,0]
```
<br>
<br>


## 4. Using callback functions
(now writing)
<br>
<br>


## 5. (Cool) type description interface
(now writing)
<br>
<br>


## API Documentation  
***
(now writing)
<br>
<br>


## Notes
***
This folder is migrated from ikeshou/CSG_research repository (private, for resarch use).
Do not worry about the small number of commitment!
<br>
Now implementing: 
- Type inference system in type description inteface
- Specific debugger for Python and Euslisp FFI

It's gonna be awesome :wink:
