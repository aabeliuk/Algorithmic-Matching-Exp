# Algorithmic-Matching-Exp
Data and code for the Paper "Price of Anarchy in Algorithmic Matching of Romantic Partners"


## Code for plotting Figures
We have tested the code with R version 3.4.1 (2017-06-30) and Python 2.7.14 :: Anaconda custom (64-bit) in Mac OSX Sierra (on MacBook Pro).

## R 

### Dependencies

```
install.packages('Hmisc')
install.packages('ggplot2')
install.packages('wesanderson')
```

### Reproduction Instruction
To reproduce all the figures in the manuscript (except, example 2), once dependencies are installed, type:
```
setwd("<path-to-repo>/Code/")
source('plot.R')
```
This will generate all images in the Figs folder.
Note that `<path-to-repo>` must be replaced with the path to your local copy of the repo.

## Python


### Dependencies
Requires Numpy, Scipy and Matplotlib. These dependencies can be installed via Anaconda distribution of Python.

Requires the following two optimization solvers: Pulp(https://pythonhosted.org/PuLP/) and pyOpt (http://www.pyopt.org/)

## Installation
```
pip install pulp
conda install -c mutirri pyopt
```

### Reproduction Instructions
To reproduce the results from example 2 (in Supplementary Material), once dependencies are installed, run:
```
python dating_competition.py 
```
This will generate an image "example2.png" in the Figs folder.

