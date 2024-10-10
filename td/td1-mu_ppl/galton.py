import pandas as pd
import numpy as np
from typing import Tuple
from mu_ppl import *
import matplotlib.pyplot as plt


raw = pd.read_csv("galton.csv")
data = raw.loc[:,["parent","child"]]
x_obs = data["parent"]
y_obs =  data["child"]

ax=plt.axes()
ax.set_xlim(50, 80)
ax.set_ylim(50,80)
plt.scatter(x_obs, y_obs, color='red', zorder=1)
plt.show()
def model(data):
    a = sample(Gaussian(1, 1), name="a")
    b = sample(Gaussian(1, 5), name="b")
    f = lambda x: a*x +b
    for _, p in data.iterrows():
        observe(Gaussian(f(p["parent"]), 0.5), p["child"])
    return (a, b)

with ImportanceSampling(num_particles=1000):
    dist: Categorical[Tuple[float,float]] = infer(model, data)
x = np.linspace(55, 80, 2)
for i in range(10):   
    a, b = dist.sample()
    print(a, b)
    plt.plot(x, a * x + b, color='blue', alpha=0.1, zorder=0)


plt.scatter(x_obs, y_obs, color='red', zorder=1)
plt.show()