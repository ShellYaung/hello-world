# -*- coding: utf-8 -*-
"""
Created on Sat Mar 23 18:02:56 2019

@author: Shell Young
"""


def main():   
    enter = input("Please enter your credit card number: ")
    listNum = list(enter)
    intNum = eval(enter)
    number = []
    for n in listNum:
        n = int(n)
        number.append(n)
    
    def isValid(a,b):
        return getSize(a) >= 13 and getSize(a) <= 16 and \
        (prefixMatched(a, 4) or prefixMatched(a, 5) or \
        prefixMatched(a, 6) or prefixMatched(a, 37)) and  \
       (SumOfDoubleEvenPlace(b) + SumofOddplace(b)) % 10 == 0
    
    if isValid(intNum, number):
        print(intNum, 'is valid')
    else: print(intNum, 'is not valid')
    

def SumOfDoubleEvenPlace(number):
    even = number[-2::-2]
    evensum = 0
    for e in even:
        if 2 * e >= 10:
            a = list(str(2*e))
            e = int(a[0])+int(a[1])
            evensum += e
        else: evensum += 2*e
    return evensum
    
def SumofOddplace(number):
    odd = number[-1::-2]
    oddsum = 0
    for o in odd:
        oddsum += o
    return oddsum

def getPrefix(number, k):
    result = number
    
    for i in range(getSize(number) - k):
        result //= 10
    
    return result

def prefixMatched(Num, d):
    return getPrefix(Num, getSize(d)) == d

def getSize(d): 
    numberOfDigits = 0
    
    while d != 0:
        numberOfDigits += 1
        d = d // 10
    
    return numberOfDigits


main()
