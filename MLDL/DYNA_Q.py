#!/usr/bin/env python
# coding: utf-8

# In[145]:


import numpy as np
import pandas as pd
import scipy.stats as stats
import sys
import matplotlib
import matplotlib.pyplot as plt
from collections import defaultdict
from Table_Q import QLearning
import math
import seaborn as sns
from datetime import datetime
import random

class Env:
    def __init__(self, demand_data, n_state, n_action, c, h, p,
                 time_horizon=10, order_limit=10, capacity = 1000000, order_batch = 1, IL_0=0.0,  std=1, random_demand=False, random_LT=False):
        '''if random_demand:
            self.shape = np.zeros(len(demand_data.columns))
            self.scale = np.zeros(len(demand_data.columns))
            self.demand_data = np.zeros_like(demand_data.to_numpy())
            self.index = demand_data.index
            self.columns = demand_data.columns
            for i in range(len(self.columns)):
                shape, loc, scale = stats.lognorm.fit(demand_data.iloc[:, i].to_numpy(), floc=0)
                self.shape[i] = shape
                self.scale[i] = scale
                self.demand_data[:, i] = np.round(
                    stats.lognorm.rvs(shape, loc=0, scale=scale, size=time_horizon))  # stochastic demand scenario
            
            self.demand_data = pd.DataFrame(self.demand_data, index=self.index, columns=self.columns)
           ''' 
        self.index = demand_data.index
        self.columns = demand_data.columns
        if random_demand:
            self.demand_data_ori = demand_data.to_numpy()
            self.demand_data = np.zeros_like(demand_data.to_numpy())
            self.noise = stats.norm.rvs(loc=0, scale=std, size=time_horizon * len(self.columns))
            self.noise = self.noise.reshape((time_horizon, len(self.columns)))
            for i in range(len(self.columns)):
                self.demand_data[:, i] = np.maximum(np.round(self.demand_data_ori[:, i] + self.noise[:, i]),np.zeros(len(self.demand_data_ori))) 
            self.demand_data = pd.DataFrame(self.demand_data, index=self.index, columns=self.columns)
            
        else:
            self.demand_data = demand_data  # deterministic demand scenario
        self.n_state = n_state
        self.n_action = n_action
        self.index = demand_data.index
        self.columns = demand_data.columns
        self.time_horizon = time_horizon
        self.order_limit = order_limit
        self.capacity = capacity #JUN
        self.c = np.array(c)  # ordering cost per unit
        self.h = np.array(h)  # holding cost per unit
        self.p = np.array(p)  # stockout cost per unit
        self.IL_0 = np.float(IL_0)  # initial inventory level
        
        self.std=std
        self.random_demand = random_demand
        self.random_LT = random_LT
        self.order_batch = order_batch

        self.time_period = 0  # current time step
        self.state = np.array([IL_0 for i in range(n_state)])  # state: inventory level
        self.arrival = pd.DataFrame(np.zeros_like(self.demand_data.to_numpy()), index=self.index, columns=self.columns)

    def step(self, action):
        if self.random_LT:
            LT = np.random.randint(1, 4, size=len(action))
            for i, lead_time in enumerate(LT):
                if self.time_period + lead_time < self.time_horizon:
                    self.arrival.loc[self.time_period + lead_time, i] += (action[i]*self.order_batch) #JUN
        else:
            self.arrival.loc[self.time_period] = action[0]*self.order_batch # JUN

        cost = 0  # total cost

        # order comes
        self.state += self.arrival.loc[self.time_period].to_numpy()  # inventory level after order
        # print("order comes",self.state)
        # full until capacity
        self.state = np.where(self.state > self.capacity, self.capacity, self.state) # JUN for capacity
        # print("full",self.state)
        # demand is realized
        self.state -= self.demand_data.iloc[self.time_period].to_numpy()  # inventory level after demand realization
        # print("realization",self.state)

        OH = np.where(self.state > 0, self.state, 0)  # on-hand inventory
        BO = np.where(self.state < 0, - self.state, 0)  # backorder


        self.state = np.where(self.state<0,0,self.state) # JUN for lost sale


        cost += np.sum(self.c * self.order_batch * np.array(action))  # ordering cost
        cost += np.sum(self.h * OH)  # inventory holding cost
        cost += np.sum(self.p * BO)  # penalty

        self.time_period += 1

        if self.time_horizon == self.time_period:
            done = True
        else:
            done = False
        return self.state.copy(), - cost, done

    def reset(self):
        self.time_period = 0
        self.state = np.array([self.IL_0 for i in range(self.n_state)])

        if self.random_demand:
            self.demand_data = np.zeros_like(self.demand_data.to_numpy())
            self.noise = stats.norm.rvs(loc=0, scale=self.std, size=time_horizon * len(self.columns))
            self.noise = self.noise.reshape((time_horizon, len(self.columns)))
            for i in range(len(self.columns)):
                self.demand_data[:, i] = np.maximum(np.round(self.demand_data_ori[:, i] + self.noise[:, i]),np.zeros(len(self.demand_data_ori)))
            self.demand_data = pd.DataFrame(self.demand_data, index=self.index, columns=self.columns)
        if self.random_LT:
            self.arrival = pd.DataFrame(np.zeros_like(self.demand_data.to_numpy()),
                                        index=self.index, columns=self.columns)

        return self.state.copy()


# In[146]:


class QLearning:
    def __init__(self, n_state, n_action, IL_0=0 , order_limit = 5, capacity = 300):
        self.discount_factor = 0.99
        self.learning_rate = 0.95
        self.num = 0
        self.sum_totalreward = 0
        self.Q = np.zeros([capacity+1, order_limit +1])
        self.Model_R=np.zeros([capacity+1,order_limit+1])
        self.Model_S_prime=np.zeros([capacity+1,order_limit+1])
        # self.Q1 = defaultdict(lambda: np.zeros(n_action))
        # self.Q2 = defaultdict(lambda: np.zeros(n_action))
        self.n_state = n_state
        self.n_action = n_action
        self.epsilon = 0.5

    def choose_action(self,state,episode):
        action_probability = self.e_greedy_policy(state)
        self.epsilon = self.epsilon-0.000001
        self.epsilon = max(self.epsilon , 0.000001)
        aa = []
        a = np.random.choice(np.arange(len(action_probability)), p =action_probability)
        aa.append(a)
        return aa

    def e_greedy_policy(self,state):
        state = int(state[0])
        nA = self.n_action
        A = np.ones(nA, dtype=float)*self.epsilon/nA
        best_action = np.argmax(self.Q[state])
        A[best_action] += 1.0 - self.epsilon
        return A
    
    def update_Q(self,s, s_prime, a, r, done):
        s = int(s[0])
        s_prime = int(s_prime[0])
        update_decide = random.random()
        if done == False:
            td_target = r + self.discount_factor * np.max(self.Q[s_prime])
        else:
            td_target = r
        td_delta = td_target - self.Q[s][a]
        self.Q[s][a] += self.learning_rate * td_delta
        
    def model_learning(self,s,a,r,s_prime):
        s = int(s[0])
        s_prime = int(s_prime[0])
        self.Model_R[s][a]=r
        self.Model_S_prime[s][a]=s_prime
        
    def planning(self, done, n=10):
        #random selection of visited states
        
        for loop in range(n):
            
            s=np.random.choice(np.flatnonzero(self.Q.sum(1) != 0))
            a=np.random.choice(np.where(self.Q[s]!=0)[0])

            r=self.Model_R[s][a]
            s_prime=self.Model_S_prime[s][a]
            s_prime = int(s_prime)
            if done == False:
                td_target = r + self.discount_factor * np.max(self.Q[s_prime])
            else:
                td_target = r
            td_delta = td_target - self.Q[s][a]
            self.Q[s][a] += self.learning_rate * td_delta
            


# In[104]:


df_timeline = pd.read_csv("timeline.csv")
df_timeline = df_timeline.drop(labels=["Unnamed: 0"], axis=1)
df_timeline = pd.DataFrame(df_timeline.sum(axis=1))
df_timeline = df_timeline.reset_index(drop=True)
print("AVG",df_timeline.mean(axis=0))


# In[156]:


if __name__ == "__main__":
    df_timeline = pd.read_csv("timeline.csv")
    df_timeline = df_timeline.drop(labels=["Unnamed: 0"], axis=1)
    df_timeline = pd.DataFrame(df_timeline.sum(axis=1))
    df_timeline = df_timeline.reset_index(drop=True)
    print("AVG",df_timeline.mean(axis=0))
    # print(df_timeline)
    # shape, loc, scale = stats.lognorm.fit(df_timeline, floc=0)
    # fig, ax = plt.subplots(1, 1)
    # x = np.linspace(stats.lognorm.ppf(0.01, shape, loc, scale), stats.lognorm.ppf(0.99, shape, loc, scale), 100)
    # ax.plot(x, stats.lognorm.pdf(x, shape, loc, scale), 'r-', lw=2, alpha=0.6, label='beta pdf')
    # binwidth = 50
    # ax.hist(df_timeline.to_numpy(), density=True, histtype='stepfilled', alpha=0.2,
    #         bins=np.arange(min(df_timeline.to_numpy()), max(df_timeline.to_numpy()) + binwidth, binwidth))
    # plt.show()
    order_limit = 5
    capacity = 500
    order_batch = 100
    std=5
    print("order_limit",order_limit)
    print("capacity",capacity)
    num_of_products = len(df_timeline.columns)
    time_horizon = len(df_timeline)

    c = [1 for i in range(num_of_products)]  # ordering cost per unit
    h = [1 for i in range(num_of_products)]  # holding cost per unit
    p = [5 for i in range(num_of_products)]  # stockout cost per unit

    n_state = num_of_products
    n_action = num_of_products * (order_limit + 1)

    env = Env(df_timeline, n_state, n_action, c, h, p,
              time_horizon=time_horizon, order_limit=order_limit, capacity=capacity, order_batch=order_batch, std=std,random_demand=True, random_LT=True)
    ql = QLearning(n_state, n_action, order_limit=order_limit, capacity = capacity)

    num_episode = 5000
    totalreward = []
    for episode in range(num_episode):
        step = 0
        reward_sum = 0
        # cost_sum = 0
        done = False
        state = env.reset()
        while not done:
            step += 1
            # Qlearning
            # action = [0 for i in range(num_of_products)]
            action = ql.choose_action(state,episode)
            # action = [200]
            next_state, reward, done = env.step(action)
            ql.update_Q(state, next_state, action, reward, done)
            if episode>=1:
                ql.model_learning(state,action,reward,next_state)
                ql.planning(done)

            # policy
            # action = [order_limit for i in range(num_of_products)]
            # next_state, reward, done = env.step(action)

            state = next_state
            reward_sum += reward
            if done:
                break
        if episode % 1 == 0:
            print("episode: ",episode," reward sum: ", reward_sum)
        totalreward.append(reward_sum)

    avg = np.array(totalreward)
    avg = np.mean(avg)
    print("Avg",avg)
    slot = 50
    N = len(totalreward)
    totalreward = np.array(totalreward)
    running_avg = np.empty(N)
    for t in range(N):
        running_avg[t] = totalreward[max(0, t - slot):(t + 1)].mean()
    plt.plot(running_avg, lw=1.5, alpha=0.8, color='#1f77b4')
    plt.title("Q learning ")
    plt.savefig("Q learning_dyna_Q.png")
    plt.close()


# In[ ]:




