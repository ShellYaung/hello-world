# -*- coding: utf-8 -*-
"""
Created on Sat Mar 23 17:06:36 2019

@author: Shell Young
"""

import numpy as np
def judgeprime(number):
    divisor = 2 
    while divisor <= number/2:
        if number%divisor == 0:
            return False
        divisor += 1
    return True

table = []
for a in range(10):
    a = str(a)
    for b in range(10):
        b = str(b)
        number = int(a+b)
        reverse = int(b+a)
        if judgeprime(number) and judgeprime(reverse) and number != reverse:
            table.append(number)
for a in range(10):
    a=str(a)
    for b in range(10):
        b=str(b)           
        for c in range(10):
            c = str(c)
            number = int(a+b+c)
            reverse = int(c+b+a)
            if judgeprime(number) and judgeprime(reverse) and number != reverse:
                table.append(number)
for a in range(10):
    a=str(a)
    for b in range(10):
        b=str(b)           
        for c in range(10):
            c = str(c)
            for d in range(10):
                d = str(d)
                number = int(a+b+c+d)
                reverse = int(d+c+b+a)
                if judgeprime(number) and judgeprime(reverse) and number != reverse:
                    table.append(number)
x = np.array(table)
x.resize((10,10))
print(x[:10,:])
            
#        for c in range(10):
 #           c = str(c)
  #          d = a+b+c
   #         if d
