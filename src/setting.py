from yaml import load, Loader
from addict import Dict


with open("src/config.yaml") as f:
    setting = Dict(load(f, Loader=Loader))
