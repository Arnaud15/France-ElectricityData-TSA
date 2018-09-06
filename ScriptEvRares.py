#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 19 22:30:56 2018

@author: arnaudautef
"""

import numpy as np
import matplotlib.pyplot as plt

X = np.genfromtxt('SimulationConso1Fev.txt')

X_sorted = np.sort(X)

alpha_estimes = []
t = range(10,5000)
for k in t:
    seuil = np.log(X_sorted[k+1])
    alph = 1 / np.mean(np.log(X_sorted[k+1:]) - seuil)
    alpha_estimes.append(alph)

plt.plot(t,alpha_estimes)

plt.hist(X, 50, normed=1, facecolor='green', alpha=0.75)

