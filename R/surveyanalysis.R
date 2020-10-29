#### Dependencies ####
library(tidyverse) 
library(survey)

##### Read in data ####
econ <- read.csv("Datasets/econ3.csv", header =T, na.strings = "")

data(api)

#make column for total # water systems (finite population count)
#FPC = ((N - n)/(N - 1)) ^ 0.5
# N = population size
# n = sample size

df <- df %>% 
  mutate(fpc = ((2389 - 400)/(2389 - 1))^0.5)

#### Calculate Survey Weights ####
#survey weight is relative probability of being sampled. Need to calculate for our dataset
#make table that summarizes population proportion, sample proportion and population proportion/sample proportion by each tag. 
econ

wts <- df %>% 
  mutate(fpc1 = 2389) %>% 
  group_by(tag) %>% 
  summarize(PopProportion = count()/2389, #total number of water systems
            SamProportion = count(sample)/300, #total number of samples
            pw = PopProportion / SamProportion) #calculate weight

##### Specify sampling design ####
#(stratified)
srv_design <- svydesign(data = df, 
                        weights = NULL, #if weighted. Don't forget tilde ~
                        fpc = ~fpc, #finite population correction factor
                        fpcstype = "fraction",
                        strata = ~tag, #bins
                        id = ~1) #number of stages of sampling- denote first one (if multiple). Also specify if clustering (we did not)
#print summary of design
summary(srv_design)
