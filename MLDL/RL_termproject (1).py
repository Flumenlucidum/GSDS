import numpy as np
import pandas as pd
from Table_Q import QLearning
import matplotlib.pyplot as plt
from collections import defaultdict

import sys


class Env:
    def __init__(self, demand_data, n_state, n_action, h, p, time_horizon=10, order_limit=10, IL_0=0):
        self.demand_data = demand_data  # demand scenario
        self.n_state = n_state
        self.n_action = n_action

        self.time_horizon = time_horizon
        self.order_limit = order_limit
        self.h = np.array(h)  # holding cost per unit
        self.p = np.array(p)  # stockout cost per unit
        self.IL_0 = IL_0  # initial inventory level

        self.time_period = 0  # current time step
        self.state = np.array([IL_0 for i in range(n_state)])  # state: inventory level for each product

    def step(self, action):
        cost = 0  # total cost

        self.state += action  # inventory level after` order
        # print(self.demand_data.iloc[self.time_period])
        # sys.exit(1)
        self.state = self.state - np.array([self.demand_data.iloc[self.time_period,0]])  # inventory level after demand realization
        OH = np.where(self.state > 0, self.state, 0)  # on-hand inventory
        BO = np.where(self.state < 0, - self.state, 0)  # backorder
        cost += np.sum(self.h * OH)  # inventory holding cost
        cost += np.sum(self.p * BO)  # penalty

        self.time_period += 1

        if self.time_horizon == self.time_period:
            done = True
        else:
            done = False

        return self.state, - cost, done

    def reset(self):
        self.time_period = 0
        self.state = np.array([self.IL_0 for i in range(self.n_state)])

        return self.state


if __name__ == "__main__":
    df_timeline = pd.read_csv("data/timeline.csv")
    df_timeline = df_timeline.set_index("Unnamed: 0", drop=True)
    df_timeline = pd.DataFrame(df_timeline.sum(axis=1))
    order_limit = 200
    num_of_products = len(df_timeline.columns)
    time_horizon = len(df_timeline)

    h = [1 for i in range(num_of_products)]  # holding cost per unit
    p = [5 for i in range(num_of_products)]  # stockout cost per unit

    n_state = num_of_products #32951
    n_action = num_of_products * (order_limit + 1)

    env = Env(df_timeline, n_state, n_action, h, p, time_horizon, order_limit)
    ql = QLearning(n_state, n_action)
    num_epsiodes = 100000
    totalreward = []
    for episode in range(num_epsiodes):
        step = 0
        reward_sum = 0
        done = False
        state = env.reset()
        while not done:
            step += 1
            if step==1:
                action = [100 for i in range(num_of_products)]
            else:
                action = ql.choose_action(state)
            next_state, reward, done = env.step(action)
            ql.update_Q(state, next_state, action, reward, done)
            state = next_state
            reward_sum += reward
            if done:
                break
        if episode % 100 == 0:
            print("episode: ",episode," reward sum: ", reward_sum)
        totalreward.append(reward_sum)

        # draw the learning curve
    slot = 50
    N = len(totalreward)
    totalreward = np.array(totalreward)
    running_avg = np.empty(N)
    for t in range(N):
        running_avg[t] = totalreward[max(0, t - slot):(t + 1)].mean()
    plt.plot(running_avg, lw=1.5, alpha=0.8, color='#1f77b4')
    plt.title("Q learning learning curve")
    plt.savefig("Q learning.png")
    plt.close()


