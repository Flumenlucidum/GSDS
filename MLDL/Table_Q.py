import numpy as np
import sys
import math
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
from collections import defaultdict
import random
import pandas as pd

class QLearning:
    def __init__(self, n_state, n_action, IL_0=0):
        self.discount_factor = 0.99
        self.learning_rate = 0.95
        self.num = 0
        self.sum_totalreward = 0
        self.Q = defaultdict(lambda: np.zeros(n_action))
        self.n_state = n_state
        self.n_action = n_action
        self.epsilon = 0.01

    def choose_action(self,state):
        action_probability = self.e_greedy_policy(state)
        a = np.random.choice(np.arange(len(action_probability)), p =action_probability)
        return a

    def e_greedy_policy(self,state):
        state = tuple(state)
        nA = self.n_action
        A = np.ones(nA, dtype=float)*self.epsilon/nA
        best_action = np.argmax(self.Q[state])
        A[best_action] += 1.0 - self.epsilon
        return A

    def update_Q(self,s, s_prime, a, r, done):
        s = tuple(s)
        s_prime = tuple(s_prime)
        if done == False:
            td_target = r + self.discount_factor*np.max(self.Q[s_prime])
        else:
            td_target = r
        td_delta = td_target - self.Q[s][a]
        self.Q[s][a] += self.learning_rate * td_delta