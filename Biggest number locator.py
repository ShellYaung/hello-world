# -*- coding: utf-8 -*-
"""
Created on Tue Apr 23 16:32:34 2019

@author: Shell Young
"""

class location:
    def __init__(self, row=0, data=[]):
        self.__data = data
        self.__row = row
    def getrow(self):
        return self.__row
    def getrowbig(self):
        bign = []
        p=0
        for i in range(self.getrow()):
            bign.append(max(self.__data[p]))
            p += 1
        return bign
    
    def getallbig(self):
        biggest = max(self.getrowbig())
        return biggest
    def getrownum(self):
        return self.getrowbig().index(self.getallbig()) + 1

    
def main():
    data1 = []
    rowcolumnNum = eval(input('Enter the number of rows and columns in the list: '))
    RClist = list(rowcolumnNum)
    a = RClist[0]
    for i in range(a):
        rowNum = 0
        asd = str(rowNum)
        row = eval(input('enter row'+asd+' :'))
        rowNum += 1
        data1.append(row)
    g=location(a,data1)
    u=data1[g.getrownum()-1]
    col=0
    for i in u:
        col+=1
        if i == g.getallbig():
            break
    print('The location of the largest element is: ',g.getallbig(),',','at ('
          ,g.getrownum(),',',col,')')
main()


        