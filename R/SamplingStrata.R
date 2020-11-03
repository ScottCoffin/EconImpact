# Determine the atomic strata (Cartesian product of the auxiliary information - i.e. service connections)
## usually too difficult to do. Another way is to explosre the space of partitions with the genetic algorithm using r Package SamplingStrata

##### Dependencies #####
library(SamplingStrata)
library(tidyverse) #load dependencies
library(BAMMtools) #jenks
library(grid) #to make grobs
library(gridExtra) # to make multiple plots

##### Resources: ####
# https://www.istat.it/it/files/2014/06/SamplingStrata-An-R-Package-for-the-Optimization-of-Stratified-Sampling.pdf


#### Load in Data ####
econ1 <- read.csv("Datasets/CWS.csv", header =T, na.strings = "")
data("swissmunicipalities", package = "sampling") #practice dataset

# First, we define the identifier of the frame:
frame <- NULL
frame$id <- swissmunicipalities$Nom

#Let us suppose to plan a survey whose target estimates are the totals of the population by
#age class in each Swiss region. In this case, the Y ’s variables will be:
#Y1: number of men and women aged between 0 and 19,
# Y2: number of men and women aged between 20 and 39,
# Y3: number of men and women aged between 40 and 64,
# Y4: number of men and women aged 65 and over.
# Consequently, the following statements are executed:
frame$Y1 <- swissmunicipalities$Pop020
frame$Y2 <- swissmunicipalities$Pop2040
frame$Y3 <- swissmunicipalities$Pop4065
frame$Y4 <- swissmunicipalities$Pop65P

# Convert continuous variables into categorical using k-means clustering (function = var.bin)
library("SamplingStrata")
set.seed(1508)
frame$X1 <- var.bin(swissmunicipalities$POPTOT, bins = 18)
frame$X2 <- var.bin(swissmunicipalities$Surfacesbois, bins = 3)
frame$X3 <- var.bin(swissmunicipalities$Surfacescult, bins = 3)
frame$X4 <- var.bin(swissmunicipalities$Alp, bins = 3)
frame$X5 <- var.bin(swissmunicipalities$Airbat, bins = 3)
frame$X6 <- var.bin(swissmunicipalities$Airind, bins = 3)


# set the values of the domainvalue variable (in this case, estimates for the seven regions $REG)
frame$domainvalue <- swissmunicipalities$REG
frame <- as.data.frame(frame)
head(frame)

##### Filter Data ####
#Count each fee code category
econ1 %>% 
  group_by(Fee.Code) %>% 
  summarize(n())

## filter data ##
econ3 <- econ1 %>% 
  filter(Service.Connections <10000) %>% #filter above 2 because there are many
  filter(Fee.Code !="WH") %>%  #filter out wholesalers 
  filter(Fee.Code !="N1") %>% #transient non-community water systems
  filter(Fee.Code !="N2") %>%  #transient non-community water systems (handwash exemption)
  filter(Fee.Code != "SP") #nonTransient-NonCommunity water system


#re-count groups
econ3 %>% 
  group_by(Fee.Code) %>% 
  summarize(n())

#determine mean and stdev
econ3 %>% 
  summarize(mean = mean(Service.Connections),
            var = var(Service.Connections))

#### Natural Breaks ####
#Determine natural breaks and assign
breaks <- getJenksBreaks(econ3$Service.Connections, 6)

#alternative method with stats, very slow
#plotJenks(econ3$Service.Connections, n=4) 

#specify bin labels
tags <- c("Bin A", "Bin B", "Bin C", "Bin D")

#bucket values into bins
bins <- cut(econ3$Service.Connections,
            breaks = breaks,
            include.lowest = TRUE,
            right = FALSE,
            labels = tags)

#inspect bins
summary(bins)

breaks
#Store group as new column
econ3 <-as_tibble(econ3) %>% 
  mutate(tag = case_when(
    Service.Connections >= breaks[1] & Service.Connections < breaks[2] ~tags[1],
    Service.Connections >= breaks[2] & Service.Connections < breaks[3] ~tags[2],
    Service.Connections >= breaks[3] & Service.Connections <= breaks[4] ~tags[3],
    Service.Connections >= breaks[4] & Service.Connections <= breaks[5] ~tags[4],
  )) %>% 
  mutate(logService.Connections = log10(Service.Connections))

#tag is character vector, so convert to factor
econ3$tag <- factor(econ3$tag,
                    levels = tags,
                    ordered = FALSE)
##save data table as csv
#write.csv(econ3, "Datasets/econ3.csv")



##### Construction of atomic strata #####
# 1. the identifier of the stratum (named STRATO), concatenation of the values of the variables
# X’s;
# 2. the values of the M auxiliary variables (named X1 to XM ) corresponding to those in the
# frame;
# 3. the total number of units in the population belonging to the stratum (named N);
# 4. a flag (named CENS) indicating if the stratum is to be censused (= 1) or sampled (= 0);
# 5. a variable indicating the cost of interviewing a single unit in the stratum (named COST);
# 6. for each target variable Y , its estimated mean and standard deviation (named respectively Mi and Si);
# 7. the value of the domain of interest to which the stratum belongs (named DOM1 and corresponding to the variable domainvalue in the frame data frame).

#if all values are present, buildStrataDF can auto generate strata
strata <- buildStrataDF(frame)
str(strata)

# It is worth noting that the total number of different atomic strata is lower than the expected
# dimension of the Cartesian product of the X’s (which is 4,374): this is due to the fact that
# not all combinations of the value of the auxiliary variables are present in the sampling frame.
# Variables COST and CENS are initialized to 1 and 0, respectively, for all strata. It is possible
# to give them different values:
#   1. for variable COST, it is possible to differentiate the cost of interviewing per unit by
# assigning real values;
# 2. for variable CENS, it is possible to set it equal to 1 for all strata that are of the ‘take-all’
# type (i.e., all units in those strata must be selected).
# On the contrary, if there is no information in the frame regarding the target variables, it is
# necessary to build the strata data frame starting from other sources, for instance a previous
# round of the same survey, or from other surveys.



####### Choice of precision constraints for each target estimate ######
cv <- data.frame(DOM = "DOM1", CV1 = 0.05, CV2 = 0.05, CV3 = 0.05, CV4 = 0.05, domainvalue = 1:7)
cv

# In this way, we have set a maximum of 5% to the coefficients of variation expected for variables
# Y1, Y2, Y3 and Y4, in each of the 7 different domains (Swiss regions) in domain level DOM1.
# Of course we could differentiate the precision constraints region by region. It is important to
# underline that the values of domainvalue are the same than those in the frame data frame,
# and correspond to the values of variable DOM1 in the strata data frame

# If we want to determine the total size of the sample required to satisfy these precision constraints, considering the current stratification of the frame (the 641 atomic strata), we can do
# this by simply using the function bethel (it is worth noting that the format of the constraints
#data frame for the bethel function is different from the one accepted by the optimizeStrata
#function, as in bethel it is not possible to differentiate precision levels in the various subdomains) :

errors <- cv[1, 1:5]
allocation <- bethel(strata, errors)
length(allocation)  

sum(allocation)

# This is the required amount of units to be sampled when the frame stratification is most
# detailed. In general, after the optimization, this number is greatly reduced.

##### Optimization of frame stratification#####
# 1. cv: the (mandatory) data frame containing the precision levels expressed in terms
# of maximum acceptable coefficients of variation that refer to the estimates on target
# variables Y ’s of the survey;
# 2. strata: the (mandatory) data frame containing the information related to atomic strata;
# 3. initialStrata: the initial upper limit on the number of strata for each solution. Default value is nrow(strata), i.e., the number of atomic strata;
# 4. minnumstr: the minimum number of units that must be allocated in each stratum.
# Default is 2, that is the minimum value necessary to calculate sampling variance;
# 5. iter: the number of iterations (= generations) to be performed by the algorithm.
# Default is 20;
# 6. pops: the dimension of each generation in terms of individuals. Default is 50;
# 7. mut_chance (mutation chance): for each new individual, the probability that the value
# of a given chromosome (i.e., one bit in the solution vector), is changed. Default is 0.05;
# 8. elitism_rate: this parameter indicates the rate of fittest solutions that must be transferred from one generation to another. Default is 0.2.

solution <- optimizeStrata(errors = cv, strata = strata, cens = NULL, 
                           strcens = FALSE, addStrataFactor = 0.00,
                           minnumstr = 2, iter = 400, pops = 20, mut_chance = 0.005, 
                           elitism_rate = 0.2, highvalue = 1e+08, suggestions = NULL, 
                           realAllocation = TRUE, writeFiles = TRUE) #initialStrata = nrow(strata) -default, not working for some reason

## examine number of optimized strata 
sum(ceiling(solution$aggr_strata$SOLUZ))


##### 3.5. Analysis of results #####
#In order to analyze how tomic strata have been aggregated, it is possible to apply the function updateStrata, that
# assigns the labels of the new strata to the initial ones in the data frame strata, and produces:
# 1. a new file named ‘newstrata.txt’ containing all the information in the strata data frame
# related to atomic strata, plus a label indicating to which new stratum a given atomic
#stratum belongs;
#2. a table, contained in the file ‘strata aggregation.txt’, showing in which way the auxiliary variables X determine the new strata.

newstrata <- updateStrata(strata, solution, writeFiles = TRUE)
head(newstrata)

# Now, the atomic strata are associated with the aggregate strata defined in the optimal solution, by means of the variable LABEL. If we want to analyze in detail the new structure of the
# stratification, we can look at the ‘strata_aggregation.txt’ file:

strata_aggregation <- read.delim("strata_aggregation.txt")
head(strata_aggregation)

#In this structure, for each aggregate stratum the values of the X’s variables in each contributing atomic stratum are reported. It is then possible to understand the meaning of each
# aggregate stratum produced by the optimization.

##### 3.6. Updating the frame and selecting the sample #####

# Once the optimal stratification has been obtained, to be operational we need to accomplish
# the following two steps:
# 1. to update the frame units with new stratum labels (combination of the new values of
#                                                         the auxiliary variables X);
# 2. to select the sample from the frame stratified accordingly to the solution found.
# To do the first, we execute the following command:

framenew <- updateFrame(frame, newstrata, writeFiles = TRUE)

# The function updateFrame receives, as arguments, the indication of the data frame in which
# the frame information is saved, and of the data frame produced by the execution of the
# updateStrata function. The execution of this function produces a data frame (framenew),
# and also a file (named ‘framenew.txt’) containing, for each unit, the label indicating to which
# aggregated stratum the unit belongs. The allocation of units is contained in the SOLUZ variable
# in the data frame solution$aggr_strata. It is now possible to select the sample from this
# new version of the frame:

sample <- selectSample(framenew, solution$aggr_strata, writeFiles = TRUE)

# The function selectSample produces two datasets:
# 1. ‘sample.csv’ containing the units of the frame that have been selected, together with the weights that have been calculated for each one of them;
# 2. ‘sample.chk.csv’ containing information on the selection: for each stratum, the number of units in the population, the planned sample, the number of selected units, the sum of their weights (that must equalise the number of units in the population).

###### 3.7. Evaluation of the found solution #####

evalSolution(framenew2, solution2$aggr_strata, nsampl = 1000, writeFiles = TRUE)

###### Goal: maximize precision levels under given budget constraints (400 samples) ####


##### Outline of Steps Required #####
# 1. analysis of the frame data: identification of available auxiliary information;
# 2. manipulation of auxiliary information: in case auxiliary variables are of continuous type,
# they have to be transformed into categorical variables;
# 3. construction of atomic strata: on the basis of the categorical auxiliary variables available
# in the sampling frame, the set of atomic strata can be obtained by cross-classifying the
# units by using all the values of all the auxiliary variables;
# 4. characterization of each atomic stratum with the information related to the target variables (mean and standard deviation for each Y , estimated by using available information: by census, previous surveys or proxy variables data);
# 5. choice of the precision constraints for each target estimate, possibly differentiated by
# domain;
# 6. optimization of stratification and determination of required sample size and allocation;
# 7. analysis of the resulting optimized strata;
# 8. association of new labels to sampling frame units, each of them indicating the new strata
# resulting from the optimal aggregation of the atomic strata;
# 9. selection of units from the sampling frame with a stratified random sample selection
# scheme;
# 10. analysis of the solution found.





