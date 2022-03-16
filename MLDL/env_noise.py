import numpy as np
import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt


class Env:
    def __init__(self, demand_data, n_state, n_action, c, h, p,
                 time_horizon=10, order_limit=10, IL_0=0.0, std=1, random_demand=False, random_LT=False):
        self.index = demand_data.index
        self.columns = demand_data.columns

        if random_demand:
            self.demand_data_ori = demand_data.to_numpy()
            self.demand_data = np.zeros_like(demand_data.to_numpy())
            self.noise = stats.norm.rvs(loc=0, scale=std, size=time_horizon * len(self.columns))
            self.noise = self.noise.reshape((time_horizon, len(self.columns)))
            for i in range(len(self.columns)):
                self.demand_data[:, i] = np.round(self.demand_data_ori[:, i] + self.noise[:, i])
            self.demand_data = pd.DataFrame(self.demand_data, index=self.index, columns=self.columns)
        else:
            self.demand_data = demand_data  # deterministic demand scenario
        self.n_state = n_state
        self.n_action = n_action

        self.time_horizon = time_horizon
        self.order_limit = order_limit
        self.c = np.array(c)  # ordering cost per unit
        self.h = np.array(h)  # holding cost per unit
        self.p = np.array(p)  # stockout cost per unit
        self.IL_0 = np.float(IL_0)  # initial inventory level

        self.std = std
        self.random_demand = random_demand
        self.random_LT = random_LT

        self.time_period = 0  # current time step
        self.state = np.array([IL_0 for i in range(n_state)])  # state: inventory level
        self.arrival = pd.DataFrame(np.zeros_like(self.demand_data.to_numpy()),
                                    index=self.index, columns=self.columns)

    def step(self, action):
        if self.random_LT:
            LT = np.random.randint(1, 4, size=len(action))
            for i, lead_time in enumerate(LT):
                if self.time_period + lead_time < self.time_horizon:
                    self.arrival.loc[self.time_period + lead_time, i] += action[i]
        else:
            self.arrival.loc[self.time_period] = action

        cost = 0  # total cost

        self.state += self.arrival.loc[self.time_period].to_numpy()  # inventory level after order
        self.state -= self.demand_data.iloc[self.time_period]  # inventory level after demand realization

        OH = np.where(self.state > 0, self.state, 0)  # on-hand inventory
        BO = np.where(self.state < 0, - self.state, 0)  # backorder

        cost += np.sum(self.c * np.array(action))  # ordering cost
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
                self.demand_data[:, i] = np.round(self.demand_data_ori[:, i] + self.noise[:, i])
            self.demand_data = pd.DataFrame(self.demand_data, index=self.index, columns=self.columns)

        if self.random_LT:
            self.arrival = pd.DataFrame(np.zeros_like(self.demand_data.to_numpy()),
                                        index=self.index, columns=self.columns)

        return self.state.copy()


if __name__ == "__main__":
    df_timeline = pd.read_csv("data/timeline.csv")
    df_timeline = df_timeline.drop(labels=["Unnamed: 0"], axis=1)
    df_timeline = pd.DataFrame(df_timeline.sum(axis=1))
    df_timeline = df_timeline.reset_index(drop=True)

    # shape, loc, scale = stats.lognorm.fit(df_timeline, floc=0)
    # fig, ax = plt.subplots(1, 1)
    # x = np.linspace(stats.lognorm.ppf(0.01, shape, loc, scale), stats.lognorm.ppf(0.99, shape, loc, scale), 100)
    # ax.plot(x, stats.lognorm.pdf(x, shape, loc, scale), 'r-', lw=2, alpha=0.6, label='beta pdf')
    # binwidth = 50
    # ax.hist(df_timeline.to_numpy(), density=True, histtype='stepfilled', alpha=0.2,
    #         bins=np.arange(min(df_timeline.to_numpy()), max(df_timeline.to_numpy()) + binwidth, binwidth))
    # plt.show()

    order_limit = 20
    num_of_products = len(df_timeline.columns)
    time_horizon = len(df_timeline)

    c = [1 for i in range(num_of_products)]  # ordering cost per unit
    h = [1 for i in range(num_of_products)]  # holding cost per unit
    p = [5 for i in range(num_of_products)]  # stockout cost per unit

    n_state = num_of_products
    n_action = num_of_products * (order_limit + 1)

    env = Env(df_timeline, n_state, n_action, c, h, p,
              time_horizon=time_horizon, order_limit=order_limit, random_demand=True, random_LT=True)

    step = 0
    reward_cum = 0
    done = False

    state = env.reset()

    while not done:
        step += 1
        action = [10 for i in range(num_of_products)]
        next_state, reward, done = env.step(action)
        reward_cum += reward

        state = next_state

    print(reward_cum)