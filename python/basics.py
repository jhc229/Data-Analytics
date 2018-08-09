##### Basics

# Run one line Ctrl+r or Command+r
# Run selected lines Ctrl+shift+r or Command+shift+r

###
2+2  # will not print
5/6
5.0/6
float(5)/6
import math
math.sqrt(36)
from math import sqrt
sqrt(36)
vol=3 # type integer
print vol
"hello world!"
'hello world!'
x='hello'
x
print x
x,y=2,3 #multiple assignment
y,x
x=y=z=10
x,y,z
x=1; y=2
x, y
print 'my volume is', vol
phrase='hello '+'world'
print phrase

### Lists (they are constructed by bracket)
cities=['NYC','LA','SF']
cities
cities[0] #known as slicing (like subsetting in R)
cities[-1]
cities.append('CHI')
cities
range(10) #similar to sequence() in R
range(3,8) #upto 7
range(0,20,2)
new_list=['hello',2,'all',39.5] # don't need to have the same data type, but not recommended
new_list
len(new_list)

my_list=[1,2,3,4]
my_list2=my_list #dependent copy
my_list2
my_list[0]=10
my_list2
my_list3=my_list[:] #independent copy
my_list[1]=20
my_list3

# some operators
3 in my_list
[1,2,3]+[4,5,6]
[1,2,3]*3

# some methods
my_list=[1,2]
my_list.append(3)
my_list
my_list.insert(0,5) # where, what
my_list
my_list.extend([100,101])
my_list
my_list.index(5) # returns index of the argument

ages=[12,34,1,3]
ages.sort()
ages

### tuple (they are constructed by parantheses)
my_tuple=(1,2,3) 
my_tuple.append(5) # tuples are immutable, they are useful to group pieces of data together
my_tuple=(23,14,22)
my_tuple[2]  # tuples are faster than lists

my_list=list(my_tuple) #switch btw. list and tuple
my_list
my_tuple2=tuple(my_list)
my_tuple2

### Sets (defined using curly brackets)
my_set={1,1,1,2,3,3}
my_set
len(my_set)
3 in my_set  
4 not in my_set    
my_set1=my_set.copy() #independent copy
my_set2={3,3,3,4,5,5,6}
set.intersection(my_set1,my_set2)
set.union(my_set1,my_set2)
                          
### dictionaries (they are constructed by curly brackets) -- key:value
# keys are immutable, values of any type
my_dict={'sam':33,'john':22,'edward':20}
my_dict
my_dict['sam']
len(my_dict)
my_dict2=my_dict.copy()  #independent copy
my_dict2
my_dict.has_key('john')
my_dict.clear()
my_dict

#dictionar methods
my_dict={"foo":1,"bar":2}
my_dict.keys()
my_dict.values()
my_dict.items()

### miscellaneous
z=True #type boolean
z
type(z)

y=None #type is NoneType
y
type(y)

import os
os.system('cls') # clear console

del y, z # to remove variable(s)
%reset # to remove all variables

### logical operators (and, or, not)
x=1
y=3
if x==1 and y==3:
    print 'first condition satisfied'
elif x==1 or y==3:
    print 'second condition satisfied'
else:
    DoesNotExist()

###  some loops
cities=['NYC','LA','SF','CHI']
for city in cities:
    print city
    
for i in range(1,10):
    print 'The inverse of ', i, 'is', 1.0/i
    
for letter in 'Hello':
    print letter

for city in cities:
    if city=='NYC':
            print 'Cool'
    elif city == 'SF': #notice the indentations
            print 'still cool'
    else:   
            print 'fine'
a = 10
while a > 0:
    print a
    a -= 1

num=2
for i in [1,2,3,4]:
    if i > num:
	break
    else:
	print i

###  functions  (are variables are local unless sepcified as global)
def adder(num1,num2):
    ### this function adds num1 to num2 ###
    ans=num1+num2
    return ans

adder(3,4)

