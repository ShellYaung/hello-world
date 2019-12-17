#!/usr/bin/env python
# coding: utf-8

# In[9]:


from sklearn import datasets
import math
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import pandas as pd
from minisom import MiniSom


# In[4]:


digit = datasets.load_digits()
print(digit.data.shape)


# In[5]:


#this dataset has 1797 samples with 64 features for each item
x = digit.data
y = digit.target
x_train, x_test, y_train, y_test = train_test_split(x,y,test_size=0.4,random_state=0)


# In[11]:


n = x_train.shape[0]
m = x_train.shape[1]
size = math.ceil(np.sqrt(n)*5)
print('training sample size:{}; tesing sample size:{}'.format(n,x_test.shape[0]))
print('optimal output gridsize',size)


# In[18]:


MaxIter = 200
som = MiniSom(size, size, m, sigma = 3, learning_rate=0.5, neighborhood_function = 'bubble')
som.random_weights_init(x_train)
som.train_batch(x_train,MaxIter,verbose=False)
winmap = som.labels_map(x_train,y_train)
def classify(som,data,winmap):
    from numpy import sum as npsum
    default_class = npsum(list(winmap.values())).most_common()[0][0]
    result = []
    for d in data:
        win_position = som.winner(d)
        if win_position in winmap:
            result.append(winmap[win_position].most_common()[0][0])
        else:
            result.append(default_class)
    return result
prediction = classify(som,x_test,winmap)
print(classification_report(y_test,np.array(prediction)))


# In[ ]:




