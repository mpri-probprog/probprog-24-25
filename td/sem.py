#!/usr/bin/env python3

def stop(p:float):
    n = 0
    while sample(Bernoulli(p)):
        n = n+1
    return n

with ImportanceSampling(num_particles=1000):
    for i in range(1,10):
        dist = infer(stop, i/10)
        viz(dist)

def flip(p):
    x = sample(Bernoulli(p), name='x')
    y = sample(Bernoulli(p), name='y')
    assume(x!=y)
    return x

with Enumeration():
    for i in range(1, 10):
        dist: Categorical = infer(flip, i/10)
        print("bias: ", i/10," mean: ", dist.stats()[0])

def coin(obs: list[int]) -> float:
    p = sample(Uniform(0, 1))
    for o in obs:
        observe(Bernoulli(p), o)
    return p

with ImportanceSampling(num_particles=10000):
    dist: Categorical = infer(coin, [0, 0, 0, 0, 0, 0, 0, 0, 1, 1])
    print(dist.stats())
    viz(dist)

def flipRec(p, s=' '):
    x = sample(Bernoulli(p), name='x')
    y = sample(Bernoulli(p), name='y')
    return x if (x!=y) else flipRec(p, s+'_')

with Enumeration():
    for i in range(1, 10):
        dist: Categorical = infer(flip_rec, i/10)
        print("bias: ", i/10," mean: ", dist.stats()[0])
