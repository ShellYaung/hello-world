# -*- coding: utf-8 -*-
"""
Created on Tue Feb 19 19:21:42 2019

@author: Shell Young
"""
X1, Y1, W1, H1 = eval(input("Enter r1's center x-, y-coordinates, width, and height: "))  
X2, Y2, W2, H2 = eval(input("Enter r2's center x-, y-coordinates, width, and height: ")) 

X1X2 = abs(X1 - X2)
Y1Y2 = abs(Y1 - Y2)

wSum = (W1/2) + (W2/2) 
hSum = (H1/2) + (H2/2) 
wDiff = (W1 - W2)/2
hDiff = (H1 - H2)/2

if X1X2 <= wDiff and Y1Y2 <= hDiff:
    print ("r2 is inside r1")
elif X1X2 < wSum and Y1Y2  < hSum:
    print("r2 overlaps r1 ")
else:
    print("r2 does not overlap r1 ")
