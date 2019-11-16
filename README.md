# PyEus
Python library that builds FFI between Python and Euslisp.


## Author
Ikezaki Shoya <ikezaki@csg.ci.i.u-tokyo.ac.jp>

## Platform
PyEus supports Linux only.  
Though you can probably use PyEus library on your Mac OS X, it is not supported.

## Caution
Since the library of this version is an experimental one, please be sure that the whole architecture of the library might be changed.  
Moreover, the way of bootstrapping has not yet refined :)  
If you just want to see how it works or just want to try my library, please move on to the next step!


## Getting started
1. clone this package
```
git clone https://github.com/ikeshou/PyEus.git
```

2. place pyeus.py and eus_server.l inside the same directory that your main python program is placed.

3. You can call euslisp functions from your python program after `import pyeus` library!
```
# inside your python program

import pyeus
# some codes here
```


## Tutorial
1. Using variables and functions  

Suppose that you want to use `originalVariable` and `my-awesome-eus-function` in the "ROBOP" package written in Euslisp from Python.  
The first thing you have to do is import pyeus library and load the euslisp file by using `load_library` function where `originalVariable` and `my-awesome-eus-function` is written.  
Though the module_name argument is optional, please use the argument since it works as "include gard".  
```
# Python user code
import pyeus

pyeus.load_library(src="/Users/ikezakishoya/PyEus/eus_script.l", module_name=":robop")
```

The next step you should do is making the euslisp package object inside the python by using `make_eus_instance` function.
The keyword notation or quote notation or uppercase string notation is allowed as pkg argument.  
```
# Python user code
robop = pyeus.make_eus_instance(pkg=":robop")
```

Now that you can use any Euslisp variables/functions/classes as you want! (Of course, you can use Euslisp built-in variables/functions/classes.) You can call Euslisp functions mostly as same as you use fucntions in a Python module.    
Before moving on to the precise explanation, please remember there are some rules:  
- Since using some symbols("+-*/@$%^&=<>~.;") is prohibited in Python, these characters in a package/variable/function/class name is automatically translated to underscore("_").  
- Because the namespace of the function is separated from other symbols in Euslisp, sometimes there are functions whose name is as same as the name of a variable or a class. In that case, the prefix "func_" is automatically added to the name of the function in Python in order to tell one from the other. Conversely, you can access variables, functions and classes in Euslisp by completely the same name inside Python except for these cases.  
- If the Euslisp function returns the object that is integer or float or null or t, the result is integer or float or None or True (Python built-in object)). (If you want to know other cases, see the next section!)  
```
var = robop.originalVariable
result = robop.my_awesome_eus_function(*args, **kwargs)
```

- Importantly, symbols that is passed as an argument is evaluated as a **Python object**.  
For example,  
```
;; suppose you want to use a　~~strange~~ Euslisp function below
(defun complicated~func (num1, num2, num3, &key param s)
  (if (string= s "py")
      (+ num1 num2 num3 param)))
```
```
# Python user code
a, b = 1, 10    #　of course, this is python onject
n = robop.complicated_func(a, b, 100, param=1000, name="py")    #　a and b are evaluated in Python!
``` 
Running the code above is as same as running the following code in Euslisp.  
```
(robop::func 1 10 100 :param 1000 :name "py")
```
As a result, the `n` will be 1111 (Python built-in object "integer").


2. Using (more and more) data types 

What if you want to use functions of which argument is not an object explained above such as number or string but a list or vector or array?  
Basically, some Python data types are converted to corresponding Euslisp data types automatically:  
(From Python to Euslisp)  

|Type(Python)|Type(Euslisp)|Description|
|:---|:---|:---|
|integer|integer||
|float|float||
|None, False|null|(partially not implemented)|
|True|t||
|string|string||
|list|list||
|dicttionary|hashtable|(not implemented)|
|any other object|×|TypeError(not implemented)|

If you want to pass an object as an argument that is not listed on Type(Euslisp) column of the tabale, use *explicit type declaration*:  

| Type(Python) | Type(Euslisp) | Description |
| :--- | :--- | :--- |
| EusSym | symbol |  |
| EusFuncSym | function symbol |  |
| EusStr | string |  |
| EusCons | (dotted)list |  |
| EusPlist | property list |  |
| EusList | list |  |
| EusIntVec | integer-vector |  |
| EusFloatVec | float-vector |  |
| EusBitVec | bit-vector |  |
| EusVec | vector |  |
| EusArray | array |  |
| EusHash | hashtable | (not implemented) |
| EusPath | pathname object |  |

By using the *Euslisp-like object constructor*, you can declare the type of an argument explicitly.  
```
;; suppose you want to use an Euslisp function below
;; if a list is passed as an argument, it causes an error ("irteus 0 error: array expected")
(defun zero-zero-elm-of-array (2d-arr)
  (if (= (length (array-dimensions 2d-arr)) 2)
      (aref 2d-arr 0 0)))
```
```
# Python user code
result = robop.zero_zero_elm_of_array(EusArray([[1,2],[3,4]]))
print(result)    # 1
```


Then, let's take a look at what will happen when the Euslisp returns the object that is not integer or float or null or t. 
In short, it returns *Euslisp-like object* such as EusArray.  

(From Euslisp to Python)  

|Type(Euslisp)|Type(Python)|after `to_python` conversion|Description|
|:---|:---|:---|
|integer|integer|×||
|float|float|×||
|nil|None|×||
|t|True|×||
|symbol|EusSym|string|(not implemented)|
|function symbol|EusFuncSym|(not implemented)|
|string|EusStr|string||
|(dotted)list|EusCons|list||
|list|EusList|list||
|integer-vector|EusIntVec|list||
|float-vector|EusFloatVec|list||
|bit-vector|EusBitVec|list||
|vector|EusVec|list||
|array|EusArray|list||
|hashtable|EusHash|dictionary||
|pathname object|EusPath||
|any other object|EusProxy|TypeError(not implemented)|


Going back to the first example in section 1,  
```
# Python user code
result = robop.my_awesome_eus_function(*args, **kwargs)
```
When the `my-awesome-eus-function` returns a list in Euslisp, the `result` is an EusList object. Since this itself is not a Python built-in object, you have to convert to the familiar data types by `to_python` method if you want to handle this object as an ordinary python object.  
```
# Python user code
L = result.to_python()
isinstance(L, list)    # True
```


*Euslisp-like-object* behaves as if it were an Euslisp object.  
For example,  
```
# Python user code
arr = robop.make_array([2,2])    # arr is an EusArr object
robop.setf(robop.aref(arr, 0, 0), 100)    # assign directly
print(arr.to_python)    # [[100, None], [None, None]] (the 0-0 elment is changed)
```


3. Using classes  

You can access the user-defined class and built-in class in the same way.  
```
iv = robop.instantiate(robop.integer_vector, 4)    # integer-vector is the built-in class in Euslisp
iv.to_python()    # [0,0,0,0]
```


## Specific Debugging Tool  
(not implemented)



## API Documentation  
(not implemented)


| Command | Description |
| --- | --- |
| `git status` | List all *new or modified* files |
| `git diff` | Show file differences that **haven't been** staged |
