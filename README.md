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
      - Note that anonymous.4open.science only allows for individual file downloads, which can be quite tedious 
      - To circumvent manual cloning, please use the zip-folder of this repo that we attached to our submission     
* Then download the implementations of ShapleyBO from folder named "R"
* Now run [main-experiments-R](main-experiments-R) (defaults to settings reported in paper, estimated runtime: 6 CPU hours

Important: Experimental results will be stored automatically as RDS files in home directory. In addition, you can access them as object after completion of the experiments.

### Visualization

Running experiments triggers [automatic visualization](viz-results-auto.R) of results. For customized visualization, please
* read in the saved RDS files and name them according to [viz-results.R](viz-results.R)
* then run [viz-results.R](viz-results.R) with customzed settings

### Further experiments

Additional experimental setups can now easily be created by modifying the setup in [main-experiments-R](main-experiments-R)


### Data

Find data and files to read in data in folder [data](data). In order to preserve anonymity, we do not include a reference to the study for which the data was collected. Details and data source will be made available after the double-blind reviewing process. 




