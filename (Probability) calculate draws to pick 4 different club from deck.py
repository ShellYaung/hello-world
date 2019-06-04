# -*- coding: utf-8 -*-
"""
Created on Sat Mar 16 15:55:19 2019

@author: Shell Young
"""
#create the suits and ranks
import random 
suits = ["Spades", "Hearts", "Diamonds", "Clubs"]
ranks = ["Ace", "2", "3", "4", "5", "6", "7", "8", "9","10", "Jack", "Queen", "King"]
#create the list for drew cards
check = 4*[False]
draw = 0
count = 0
#check if the picked card was has the same rank or suit as the previous one
while count < 4:
      draw += 1
      index = random.randint(0, 51)
#create a loop that stops when check list are all true
      if check[index//13]==False:
#replace False when the rank was unique and mark the card
          check[index // 13] = True
          count += 1
          suit = suits[index // 13]
          rank = ranks[index % 13]
          print(rank, "of", suit)
#display the result
print("Number of picks:", draw)