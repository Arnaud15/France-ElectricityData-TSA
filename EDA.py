#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 18 12:04:34 2018

@author: arnaudautef
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.formula.api as smf
import statsmodels.tsa.api as smt
import statsmodels.api as sm
import scipy.stats as scs
from sklearn import linear_model


stations_meteo = pd.read_csv('postesSynop.csv', sep=';')

def chargement(donnee, region='.', type_data='t',station='.'):
    
    if (donnee=='electricite_nationale'):
        data_conso = pd.read_csv('conso.csv', parse_dates={'date' : [2, 3]})
        data_conso.dropna(axis=0,subset=['Consommation'],inplace=True)
        data_conso = data_conso.set_index(pd.to_datetime(data_conso['date'].values))
        serieConsommation = pd.Series(data_conso['Consommation'])
        
        return serieConsommation
        
    elif(donnee=='electricite_regionale'):
        data_region = pd.read_csv(region + '.csv', parse_dates={'date' : [2, 3]})
        data_region.dropna(axis=0,subset=['Consommation'],inplace=True)
        data_region = data_region.set_index(pd.to_datetime(data_region['date'].values))
        serieConsommation = pd.Series(data_region['Consommation'])
        
        return serieConsommation
    
    else:
        stationID = stations_meteo[stations_meteo.Nom == station].iloc[0,0]
        serie_meteo = pd.Series()
        for i in range(1,13):
            string = '0' + str(i)
            if (i>=10):
                string = str(i)
            meteo = pd.read_csv('synop.2017' + string + '.csv', sep=';', parse_dates=[1], na_values='mq')
            meteo.fillna(method='pad',inplace=True)
            meteo = meteo.set_index(pd.to_datetime(meteo['date'].values))
            serie = pd.to_numeric(pd.Series(meteo[meteo.numer_sta == stationID][type_data]))
            serie_meteo = serie_meteo.append(serie)
        if (type_data =='t'):
            serie_meteo = serie_meteo - 273.15
            
        return serie_meteo
        

def affichage(serie, type_data, debut, fin):
    plt.figure()
    plt.plot(serie[debut:fin], label=type_data)
    plt.legend()
    plt.show()

def Trend_model(serie, o_trend):
    X = np.arange(0,len(serie))
    y = serie.values
    coeffs = np.polyfit(X, y, o_trend)
    print('Coefficients: %s' % coeffs)
    trend = list()
    for i in range(len(X)):
        	value = coeffs[-1]
        	for d in range(o_trend):
        		value += X[i]**(o_trend-d) * coeffs[d]
        	trend.append(value)
    plt.plot(serie.values)
    plt.plot(trend, color='red', linewidth=3)
    plt.show()
    return trend

def Season_model(serie, o_season, span):
    X = np.array([i%span for i in range(len(serie))])
    y = serie.values
    coeffs = np.polyfit(X, y, o_season)
    print('Coefficients: %s' % coeffs)
    season = list()
    for i in range(len(X)):
        	value = coeffs[-1]
        	for d in range(o_season):
        		value += X[i]**(o_season-d) * coeffs[d]
        	season.append(value)
    plt.plot(serie.values)
    plt.plot(season, color='red', linewidth=3)
    plt.show()
    return season

def Buys_Ballot(serie, o_trend, o_season):
    rgr = linear_model.LinearRegression()
    tf = len(serie)
    y = serie.values
    X = []
    for t in range(tf):
        n = t%o_season
        U = np.array([t**i for i in range(o_trend+1)])
        #print U
        V = np.array([int(n==j) - int(n==0) for j in range(1,o_season+1)])
        #print V
        X.append(np.concatenate((U,V)))
    rgr.fit(X,y)
    print('Coefficients: \n', rgr.coef_)
    sortie = rgr.predict(X)
    return sortie
    
def analyse_stationnaire(y, lags=30, figsize=(10, 8), style='bmh'):
    with plt.style.context(style):    
        fig = plt.figure(figsize=figsize)
        layout = (3, 2)
        ts_ax = plt.subplot2grid(layout, (0, 0), colspan=2)
        acf_ax = plt.subplot2grid(layout, (1, 0))
        pacf_ax = plt.subplot2grid(layout, (1, 1))
        qq_ax = plt.subplot2grid(layout, (2, 0))
        pp_ax = plt.subplot2grid(layout, (2, 1))
        y.plot(ax=ts_ax)
        ts_ax.set_title('Time Series Analysis Plots')
        smt.graphics.plot_acf(y, lags=lags, ax=acf_ax, alpha=0.5)
        smt.graphics.plot_pacf(y, lags=lags, ax=pacf_ax, alpha=0.5)
        sm.qqplot(y, line='s', ax=qq_ax)
        qq_ax.set_title('QQ Plot')        
        scs.probplot(y, sparams=(y.mean(), y.std()), plot=pp_ax)
        plt.tight_layout()
    return 

Region = 'Occitanie'
station = 'PERPIGNAN'

frequence = 'D'
start_date = '2017-01-01 00:00'
end_date = '2017-01-03 23:30'

serie = chargement('electricite_nationale')
serie = serie.resample(frequence).sum()
serie.fillna(method='pad',inplace=True)

deterministe = Buys_Ballot(serie[start_date:end_date], 1, 48)
plt.plot(deterministe)
affichage(serie, 'conso_electricite_max_30Min', start_date, end_date)


residu = serie[start_date:end_date] - deterministe
analyse_stationnaire(residu)


