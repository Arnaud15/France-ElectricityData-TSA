#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 11 16:39:51 2018

@author: arnaudautef
"""

#script Conso Elec Nationale
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.formula.api as smf
import statsmodels.tsa.api as smt
import statsmodels.api as sm
import scipy.stats as scs
import statsmodels.tsa.stattools as sts
from sklearn import linear_model


stations_meteo = pd.read_csv('postesSynop.csv', sep=';')

def chargement(donnee, region='.', type_data='t',station='.'):
    
    if (donnee=='electricite_nationale17'):
        data_conso = pd.read_csv('conso.csv', parse_dates={'date' : [2, 3]})
        data_conso.dropna(axis=0,subset=['Consommation'],inplace=True)
        data_conso = data_conso.set_index(pd.to_datetime(data_conso['date'].values))
        serieConsommation = pd.Series(data_conso['Consommation'])
        
        return serieConsommation
    elif (donnee=='electricite_nationale16'):
        data_conso = pd.read_csv('conso2.csv', parse_dates={'date' : [2, 3]})
        data_conso.dropna(axis=0,subset=['Consommation'],inplace=True)
        data_conso = data_conso.set_index(pd.to_datetime(data_conso['date'].values))
        serieConsommation = pd.Series(data_conso['Consommation'])
        
        return serieConsommation
    elif (donnee=='electricite_nationale15'):
        data_conso = pd.read_csv('conso3.csv', parse_dates={'date' : [2, 3]})
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

def Buys_Ballot(serie, o_trend, o_season, name):
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
    print np.shape(X)
    rgr.fit(X,y)
    (a,b) =(rgr.coef_[:o_trend+1],rgr.coef_[o_trend+1:])
    s = serie.reset_index(drop=True)
    tr = []
    sais = []
    for t in range(tf):
        n = t%o_season
        U = np.array([c * (t**i) for i,c in enumerate(a)])
        V = np.array([c * (int(n==j) - int(n==0)) for j,c in enumerate(b)])
        tr.append(np.sum(U))
        sais.append(np.sum(V))
    res = pd.Series(s.values-rgr.predict(X))
    tr = pd.Series(tr)
    sais = pd.Series(sais)
    style = 'bmh'
    with plt.style.context(style):    
        fig = plt.figure(figsize=(10, 8))
        layout = (4, 1)
        serie_ax = plt.subplot2grid(layout, (0, 0))
        serie_ax.set_title('Serie Temporelle')
        tendance_ax = plt.subplot2grid(layout, (1, 0))
        tendance_ax.set_title('Tendance')
        saisonnalite_ax = plt.subplot2grid(layout, (2, 0))
        saisonnalite_ax.set_title('Saisonnalite')
        residu_ax = plt.subplot2grid(layout, (3, 0))
        residu_ax.set_title('Residu')
        s.plot(ax=serie_ax)
        tr.plot(ax=tendance_ax)
        sais.plot(ax=saisonnalite_ax)
        res.plot(ax=residu_ax)
        plt.tight_layout()
        plt.savefig(name +'.png')
    
    return (rgr)
    
def analyse_stationnaire(y, lags=30, figsize=(10, 8), style='bmh', name='lol'):
    with plt.style.context(style):    
        fig = plt.figure(figsize=figsize)
        layout = (3, 2)
        ts_ax = plt.subplot2grid(layout, (0, 0), colspan=2)
        acf_ax = plt.subplot2grid(layout, (1, 0))
        pacf_ax = plt.subplot2grid(layout, (1, 1))
        qq_ax = plt.subplot2grid(layout, (2, 0))
        pp_ax = plt.subplot2grid(layout, (2, 1))
        y.plot(ax=ts_ax)
        ts_ax.set_title('Analyse de stationnarite')
        smt.graphics.plot_acf(y, lags=lags, ax=acf_ax, alpha=0.5)
        smt.graphics.plot_pacf(y, lags=lags, ax=pacf_ax, alpha=0.5)
        sm.qqplot(y, line='s', ax=qq_ax)
        qq_ax.set_title('QQ Plot')
        scs.probplot(y, sparams=(y.mean(), y.std()), plot=pp_ax)
        plt.tight_layout()
        plt.savefig(name +'.png')
    return 
#On charge la conso élec journaliere en France sur 2015, 2016, 2017


frequence = 'W'
serie1 = chargement('electricite_nationale15') / 10000.
serie2 = chargement('electricite_nationale16') / 10000.
serie3 = chargement('electricite_nationale17') / 10000.
serie4 = serie3.copy()
serie3 = serie3['2017-01-01 00:00':'2017-12-30 23:00']
serie = serie1.append(serie2)
serie = serie.append(serie3)
serie = serie.resample(frequence).sum()
serie.fillna(method='pad',inplace=True)

with open('ConsommationElectriqueWeek20152017.txt','w') as f:
    for val in serie.values:
        f.write(str(val)+'\n')


#OSEF DE LA SUITE LA
'''
o_season = 53
o_trend = 1
tf = len(serie.values)

model = Buys_Ballot(serie, o_trend, o_season, 'Consommation electrique hebdomadaire 2015-2017')

Y =[]
for t in range(tf):
    n = t%o_season
    U = np.array([t**i for i in range(o_trend+1)])
    #print U
    V = np.array([int(n==j) - int(n==0) for j in range(1,o_season+1)])
    #print V
    Y.append(np.concatenate((U,V)))

'''


'''
plt.figure(1)
plt.plot(serie.values)
plt.plot(model.predict(Y))
plt.show()
plt.close()
residu = pd.Series(serie.values - model.predict(Y))

analyse_stationnaire(residu, name='Residu Buys-Ballot Conso electrique hebdomadaire 2015-2017')
'''
pred = model.predict(Y)
data = pd.read_csv('Olivier.txt', sep=" ", header=None)
serieRes = np.array(data.values[0])
serieRes = serieRes + pred


print(len(serieRes))
print(len(serie))

plt.figure(15, figsize=(10,8))
plt.plot(serie.values,color='blue',label='Consommation Reelle')
plt.plot(serieRes, color='red', label='Consommation simulee')
plt.title('Simulation de la consommation par Trend + Saisonnalite  (Buys-Ballot) et Residu ARIMA(4,0,2)')
plt.legend()
plt.savefig('Simulation du modele obtenu.png')

plt.close()
''' 
with open('Consommation electrique hebdomadaire 2015-2017.txt','w') as f:
    for val in serie.values:
        f.write(str(val)+'\n')
with open('Residu Buys-Ballot Conso electrique hebdomadaire 2015-2017.txt','w') as f:
    for val in residu.values:
        f.write(str(val)+'\n')
        
with open('Test Dickey-Fuller augmente pour residu.txt','w') as f:
    f.write(str(sts.adfuller(residu)[1])+'\n')
'''
    
    
    
'''
#On cherche à savoir si la série des résidus est stationnaire
analyse_stationnaire(residu, lags = 10)

#Non, donc on analyse la série différenciée
residu1 = residu.diff()

residu1.fillna(method='backfill',inplace=True)

analyse_stationnaire(residu1)
#tjrs pas bon, on continue
residu2 = residu1.diff()

residu2.fillna(method='backfill',inplace=True)

analyse_stationnaire(residu2)

residu3 = residu2.diff()

residu3.fillna(method='backfill',inplace=True)

analyse_stationnaire(residu3)

#
'''
