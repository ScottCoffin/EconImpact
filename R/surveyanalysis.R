#### Dependencies ####
library(tidyverse) 
library(survey)

##### Read in data ####
econ <- read.csv("Datasets/econ3.csv", header =T, na.strings = "")

#make column for total # water systems (finite population count)
#FPC = ((N - n)/(N - 1)) ^ 0.5
# N = population size
# n = sample size

econ <- econ %>% 
  mutate(fpc = ((n_distinct(Water.System.Name) - 300)/(n_distinct(Water.System.Name) - 1))^0.5)

#### Calculate Survey Weights ####
#survey weight is relative probability of being sampled. Need to calculate for our dataset
#make table that summarizes population proportion, sample proportion and population proportion/sample proportion by each tag. 
wts <- econ %>% 
  group_by(tag) %>% 
  summarize(PopProportion = count()/2416, #total number of water systems
            SamProportion = count(sample)/300, #total number of samples
            pw = PopProportion / SamProportion) #calculate weight

##### Specify sampling design ####
#(stratified)
srv_design <- svydesign(data = econ, 
                        weights = ___, #if weighted. Don't forget tilde ~
                        fpc = fpc, #finite population correction factor
                        strata = ~tag, #bins
                        id = ~1) #number of stages of sampling- denote first one (if multiple). Also specify if clustering (we did not)
#print summary of design
summar(srv_design)