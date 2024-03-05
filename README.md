# ShapleyBO

## Explaining Bayesian Optimization by Shapley Values Facilitates Human-AI Collaboration



### Introduction, TOC
This repository contains ShapleyBO, a modular framework for explaining Bayesian optimization by Shapley Values, as introduced in the paper "Explaining Bayesian Optimization by Shapley Values Facilitates Human-AI Collaboration"

* [R](R) contains implementation of ShapleyBO
* [main-experiments-R](main-experiments-R) provides code to run experiments (section 5) with adjustable settings
* [data](data) contains exosuit personalization data used in experiments
* visualization of results via
    - [automatic visualization](viz-results-auto.R)
    - [custom visualization](viz-results.R)
    


### Tested with

- R 4.3.2
- R 4.2.0
- R 4.1.6
- R 4.0.3

on
- Linux Ubuntu 20.04
- Linux Debian 10
- Windows 11 Pro Build 22H2 


### Setup

In order to reproduce the papers' key results (and visualizations thereof) follow these steps:

* First and foremost, clone this repository and install all dependencies
* Then download the implementations of ShapleyBO from folder named "R"
* Now run [main-experiments-R](main-experiments-R)


* 

Eventually, download [benchmarks/experiments_simulated_data.R](benchmarks/experiments_simulated_data.R) and run from benchmarks/ (estimated runtime: 30 CPU hours)

Important: Create empty folders [results](results) and [plots](plots) where experimental results will be stored automatically. In addition, you can access them as object after completion of the experiments.


### Further experiments

Additional experimental setups can now easily be created by modifying [benchmarks/experiments_simulated_data.R](benchmarks/experiments_simulated_data.R)


### Data

Find data and files to read in data in folder [data](data). 



