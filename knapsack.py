# -*- coding: utf-8 -*-
"""
Created on Sat Dec 26 12:24:01 2015

@author: Vojta
"""
import random
from math import exp
from time import clock

import timeit
import itertools
import sys
import math
import matplotlib.pyplot as plt
import plotly.plotly as py
import numpy as np
import time

from time import clock
from operator import itemgetter
from subprocess import check_output


_TEMPERATURE = 100
#_STATE = 
_STEPS = 1000
_COOLFACTOR = 0.5
_FINALTEMP = 1

class Item:
    def __init__(self, index, weight, price):
        self.index = int(index)
        self.weight = int(weight)
        self.price = int(price)
        
class State:
    def __init__(self,n):
        self.binarnik = []
        for i in range(0,n,1):
            self.binarnik.append(0)
        
        self.items = []
        self.sumPrice = 0
        self.sumWeight = 0
        
    def calculateSums(self):
        for item in self.items:
            self.sumWeight = self.sumWeight + item.weight
            self.sumPrice = self.sumPrice + item.price
            
    def addItem(self, newItem):
        self.items.append(newItem)
        self.sumPrice = self.sumPrice + newItem.price 
        self.sumWeight = self.sumWeight + newItem.weight
        self.binarnik[newItem.index] = 1 
        
    def addItemObj(self, ceny, vahy, index):
        item = Item(index,vahy[index],ceny[index])
        self.addItem(item)
        
        
    def removeItem(self,newItem):
        self.binarnik[newItem.index] = 0
        self.sumPrice = self.sumPrice - newItem.price
        self.sumWeight = self.sumWeight - newItem.weight   
        #del self.items[newItem.index]
        for x in self.items:
            if x.index == newItem.index:
                del x
                break
    
        
def better(newState,bestState):
       
    if (newState.sumPrice > bestState.sumPrice):
        return newState
    else:
        return bestState
        
        
def nextState(state, temperature):
    newItemIndex = random.randint(0, n-1)
    newItem = Item(newItemIndex,vahy[newItemIndex],ceny[newItemIndex])
        
    if(state.binarnik[newItem.index] == 1):
        #uz tam je, takho dyztak seberem
        newPrice = state.sumPrice - newItem.price
        delta = newPrice - state.sumPrice
        
        if (random.random() < exp(delta/temperature)):
            state.removeItem(newItem)

    else:
        #neni tam, toz pridame
        if ((state.sumWeight + int(vahy[newItem.index])) <= M):
            state.addItem(newItem)
    return state   
            
        

def cool(temperature):
    global _COOLFACTOR
    temperature *= _COOLFACTOR
    return temperature
    
def frozen(temperature):
    return (temperature < _FINALTEMP)
    
def equilibrium():
    global steps
    steps = steps - 1
    if (steps < 0):
        return True
    else:
        return False
    
    
def simulatedAnnealing(initState):
    temperature = _TEMPERATURE
    state = initState
    best = state
    global steps
    
    while True:
        steps = _STEPS
        while True:
            state = nextState(state,temperature)
            best = better(best,state)
            if equilibrium():
                break
        
        temperature = cool(temperature)
        if frozen(temperature):
            break   
    return best
    
def annealingRunAdapter(parceny,parvahy, vysledek,bordel):   
    global ceny
    global vahy
    global n
    
    ceny = parceny
    vahy = parvahy
    
    n = len(parceny)
    _STATE = State(n)
    
    best = simulatedAnnealing(_STATE)
    
    del vysledek[:]
    indexy = [x.index for x in best.items]
    vysledek.extend(indexy)
    return best.sumPrice
    
    
def prumRelChyba(moje,dobre):
    chyby = []
    for i in range(0,len(moje)):
        chyby.append(abs(dobre[i] - moje[i])/float(dobre[i]))
    return sum(chyby)/float(len(chyby))
    
def maxRelChyba(moje,dobre):
    chyby = []
    for i in range(0,len(moje)):
        chyby.append(abs(dobre[i] - moje[i])/float(dobre[i]))
    return max(chyby)    
    
def runInstance(myFunction,*myArgs):
    avgCasy = []
    avgChyby = []
    maxChyby = []
    
    global _TEMPERATURE
    global _COOLFACTOR
    global _FINALTEMP
    global _STEPS
    global nagenerovano
    
    instanci = 40
    _TEMPERATURE = 110
    _STEPS = 70
    _COOLFACTOR = 0.9
    _FINALTEMP = 3
    
    nagenerovano = range(1,40,2)     
    
    #for instanci in N_INST:
    for instanci in N_INST:
        #soubor = open(sys.argv[1],'r')
        soubor=open('C:\Users\Vojta\Downloads\inst\knap_' + str(instanci) + '.inst.dat','r')
        souborSol=open('C:\Users\Vojta\Downloads\sol\knap_' + str(instanci) + '.sol.dat','r')
        spravneCeny = []
        celkCeny = []
        casy = []
        global M
        global indexy
        global bbHigh
        
        for line in souborSol:
            nasekany = line.split(" ")
            spravneCeny.append(nasekany[2])
        spravneCeny = map(int, spravneCeny)
        #print spravneCeny
        pocitadlo = 0
        
        for line in soubor:
            start = clock()
        
            nasekany = line.split(" ")
            id = nasekany[0]
            n = int(nasekany[1])
            M = int(nasekany[2])
            vahy = nasekany[3::2]
            ceny = nasekany[4::2]
            bbHigh = 0
            bbKombo = []
            vysledek = []
            
            '''
            if TEST and int(id) !=  9094:
                pocitadlo += 1
                continue
            '''
    
            indexy = range(0,len(vahy))
            celkCena = myFunction(ceny,vahy,vysledek,myArgs)
          
            end = clock()    
            casy.append(round(end-start,4))
            celkCeny.append(celkCena)
            pocitadlo = pocitadlo + 1
            
        prumCas = round(sum(casy)/float(len(casy)),4)
        prumChyba = round(prumRelChyba(celkCeny, spravneCeny)*100,2)
        avgCasy.append(prumCas)
        avgChyby.append(prumChyba)
        maxChyby.append(maxRelChyba(celkCeny, spravneCeny)*100)
        print 'prumerny cas pro n =',n,':',prumCas,'s'
        print 'prumerna relativni chyba: ',prumChyba,'%'
    return avgCasy,avgChyby,maxChyby
    
    
def runInstance2(myFunction,*myArgs):
    avgCasy = []
    avgChyby = []
    maxChyby = []
    global N_INST
    global NEGENERUJ
    
    del N_INST[:]
    
    ADRESA = 'C:\\cygwin\\home\\Vojta\\knapgen\\'
    p_n = 20
    p_N = 50
    p_m = 0.7
    p_C = 100
    p_d = 0
    p_k = 1
    p_W = 100
    
    #varList = np.linspace(0.1,1.2,num=40)
    varList = range(50,1000,50)
    for var in varList:
        p_W = var         
        PARAMS = '-n ' + str(p_n) + ' -N ' + str(p_N) + ' -m '+ str(p_m) + ' -C '+ str(p_C)+ ' -d '+ str(p_d) + ' -k ' +str(p_k) + ' -W ' + str(p_W)
        FNAME = 'n' + str(p_n) + 'N' + str(p_N) + 'm'+ str(p_m) + 'C'+ str(p_C) + 'd'+ str(p_d) + 'k' + str(p_k) + 'W' + str(p_W) + '.txt'

        print FNAME
        if(NEGENERUJ != True):        
            check_output(ADRESA + 'knapgen.exe ' + PARAMS + ' > ' + FNAME ,shell=True)
        N_INST.append(FNAME)

    print N_INST
    time.sleep(3)
    
    for instanci in N_INST:
        #soubor = open(sys.argv[1],'r')
        #soubor=open('C:\Users\Vojta\Downloads\inst\knap_' + str(instanci) + '.inst.dat','r')
        #souborSol=open('C:\Users\Vojta\Downloads\sol\knap_' + str(instanci) + '.sol.dat','r')
        soubor=open(instanci,'r')
        spravneCeny = []
        celkCeny = []
        casy = []
        global M
        global indexy
        global bbHigh
        
        '''
        for line in souborSol:
            nasekany = line.split(" ")
            spravneCeny.append(nasekany[2])
        spravneCeny = map(int, spravneCeny)
        #print spravneCeny
        '''
        pocitadlo = 0
        
        for line in soubor:
            start = clock()
        
            nasekany = line.split(" ")
            id = nasekany[0]
            n = int(nasekany[1])
            M = int(nasekany[2])
            vahy = nasekany[3::2]
            ceny = nasekany[4::2]
            bbHigh = 0
            bbKombo = []
            vysledek = []
            
    
            indexy = range(0,len(vahy))
            celkCena = myFunction(ceny,vahy,vysledek,myArgs)
            goodCena = fakeBaB(ceny,vahy,[],myArgs)
          
            end = clock()    
            casy.append(round(end-start,4))
            celkCeny.append(celkCena)
            spravneCeny.append(goodCena)
            #print id,n,celkCena,casy[pocitadlo], binarnik(vysledek, n),spravneCeny[pocitadlo],round(((spravneCeny[pocitadlo] - celkCena)/float(spravneCeny[pocitadlo]))*100,2)
            print casy[pocitadlo]            
            pocitadlo = pocitadlo + 1
            
        prumCas = round(sum(casy)/float(len(casy)),4)
        prumChyba = round(prumRelChyba(celkCeny, spravneCeny)*100,2)
        avgCasy.append(prumCas)
        avgChyby.append(prumChyba)
        #maxChyby.append(maxRelChyba(celkCeny, spravneCeny))
        #print 'prumerny cas pro n =',n,':',prumCas,'s'
        #print 'prumerna relativni chyba: ',prumChyba,'%'
        
    del N_INST[:]
    for neco in varList:
        N_INST.append(neco)
        #N_INST = varList
        
    return avgCasy,avgChyby#,maxChyby
    
T1 = True  
N_INST = []
del N_INST[:]
global nagenerovano
 
if(T1):
    N_INST.append(30)
    N_INST.append(32)
    N_INST.append(35)
    N_INST.append(37)
    N_INST.append(40)
    N_INST = sorted(N_INST)
    casy,chyby,maxChyby = runInstance(annealingRunAdapter)
    
    figura = plt.figure(1)
    plt.plot(N_INST,casy,'rx',label='Casy (s)')
    plt.plot(N_INST,chyby,'b+',label='Chyba (%)')
    plt.plot(N_INST,maxChyby,'g.',label='MaxChyba (%)')
    
    py.sign_in('sadlomasloskvarky','ndm0afp5zl')
    py.plot_mpl(figura)
            
            