#3/27/2019
import numpy as np
from scipy.stats import norm
from cvxpy import *
import matplotlib.pyplot as plt
#we have 3 stocks in this portfolio
#given information
n = 3
mu = np.array([[0.3],[0.2],[0.1]])
sigma = np.array([[0.2,0,0],[0,0.15,0],[0,0,0.1]])
r = np.array([[1,-0.3,-0.5],[-0.3,1,-0.6],[-0.5,-0.6,1]])

cov =  np.dot(np.dot(sigma,r),sigma)

#get 200 different portfolio
SAMPLE = 400
rets = np.zeros(SAMPLE)
risks = np.zeros(SAMPLE)
weights = np.zeros([SAMPLE,n])
for i in range(SAMPLE):
    sets = np.abs(np.random.randn(3))
    total = np.sum(sets)
    for j in range(n):
        weights[i][j] = sets[j]/total
    rets[i] = np.dot(np.transpose(weights[i]) , mu)
    risks[i] = np.sqrt(np.dot(np.dot(weights[i],cov), np.transpose(weights[i])))

#get the curve

#set variables for the optimization
weight = Variable(n)#via cvxpy
ret = mu.T * weight
#StandardDeviation
risk = quad_form(weight,cov)

#This sample means the possible points on the curve
sample = 50
risk_data = np.zeros(sample)
ret_data = np.zeros(sample)
for i in range(sample):
    delta = (0.3 - 0.1)/sample
    prob = Problem(Minimize(risk),
                   [sum(weight) == 1,
                    ret == 0.1 + delta * i,
                    weight >= 0])
    prob.solve()
    risk_data[i] = np.sqrt(risk.value)
    ret_data[i] = ret.value
#find the min risk in the portfolio
prob = Problem(Minimize(risk),
               [sum(weight) == 1,
                weight >= 0])
prob.solve()
minRisk = np.sqrt(risk.value)
retInMinR = ret.value
print("The minimize risk is ",minRisk)
print("The return in minize risk is ",retInMinR[0])

plt.plot(risk_data,ret_data)
plt.scatter(risks,rets)
plt.show()