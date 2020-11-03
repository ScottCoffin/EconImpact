#### Dependencies ####
library(tidyverse) 
library(survey)

#split data
bin1 <- read.csv("Datasets/Bin1_sample.csv", header = T, na.strings = "")
bin2 <- read.csv("Datasets/Bin2_sample.csv", header = T, na.strings = "")
bin3 <- read.csv("Datasets/Bin3_sample.csv", header = T, na.strings = "")
bin4 <- read.csv("Datasets/Bin4_sample.csv", header = T, na.strings = "")
#combine data to single df
df <- rbind(bin1, bin2, bin3, bin4)

#examine proportions
SummaryRealSampleList <- df %>% 
  group_by(tag) %>% 
  summarize(minServConns = min(SC), maxServConns = max(SC), count = n())
SummaryRealSampleList

#make subsets of data from population set
tag1<- econ3 %>% 
  filter(tag == "Bin A") 

tag2 <-econ3 %>% 
  filter(tag == "Bin B")

tag3 <- econ3 %>% 
  filter(tag== "Bin C")

tag4 <- econ3 %>% 
  filter(tag == "Bin D")

#### Modeling using fake data ####
set.seed(2000)

#pop totals
# mean       sd
# 791.8585 1717.339
target1 <- data.frame(Ref = c("target", "80%", "60%"), vals = c(228, (0.8*228), (0.6*228)))
target2 <- data.frame(Ref = c("target", "80%", "60%"), vals = c(133, (0.8*133), (0.6*133)))
target3 <- data.frame(Ref = c("target", "80%", "60%"), vals = c(84, (0.8*84), (0.6*84)))
target4 <- data.frame(Ref = c("target", "80%", "60%"), vals = c(65, (0.8*65), (0.6*65)))
target <- data.frame(Ref = c("target", "80%", "60%"), vals = c(510, (0.8*510), (0.6*510)))

##model the population using 2389 uniformly drawn integers
#population <- ceiling(rnorm(2389, mean = 792, sd = 1717))
population <- econ3$Service.Connections
samp <- list()
samples <- list()
sem <- numeric()
conf <- list()

#function to compute the SEM
calc_sem <- function(x) {
  sd(x)/sqrt(length(x))
}

#Calculate SEM for samples ranging in size from 1 to 550 observations
for (i in 1:2200) {
  samp[[i]]<-sample(population, i)
  sem[[i]]<-calc_sem(samp[[i]])
}

#creating a DF and adding a col for the sample size
sem_df<-data.frame(sem)
sem_df["size"]<-c(1:2200)

#plot the SEM
total <- ggplot(sem_df,aes(x = size, y = sem))+
  geom_point()+
  geom_smooth(color = "orange")+
  xlab("Sample Size")+
  ylab("Standard Error of the Mean")+
  geom_vline(data = target, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target, mapping = aes(x = vals,
                          y = 50,
                          label = Ref,
                          hjust = +1,
                          vjust = -1))+
  labs(title = "Total")+
  scale_x_continuous(limits = c(100, 600))+
  scale_y_continuous(limits = c(0,110))+
  theme_classic()
total

#### Tag 1 ####
population <- tag1$Service.Connections
samp <- list()
samples <- list()
sem <- numeric()
conf <- list()

#function to compute the SEM
calc_sem <- function(x) {
  sd(x)/sqrt(length(x))
}

#Calculate SEM for samples ranging in size from 1 to 550 observations
for (i in 1:1944) {
  samp[[i]]<-sample(population, i)
  sem[[i]]<-calc_sem(samp[[i]])
}

#creating a DF and adding a col for the sample size
sem_df<-data.frame(sem)
sem_df["size"]<-c(1:1944)

#plot the SEM
plottag1 <- ggplot(sem_df,aes(x = size, y = sem))+
  geom_point()+
  geom_smooth(color = "orange")+
  xlab("Sample Size")+
  ylab("Standard Error of the Mean")+
  geom_vline(data = target1, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target1, mapping = aes(x = vals,
                                         y = 0,
                                         label = Ref,
                                         hjust = +1,
                                         vjust = -1))+
  labs(title = "Bin 1")+
  scale_x_continuous(limits = c(0, 300))+
  scale_y_continuous(limits = c(0,110))+
  theme_classic()

plottag1


#### Tag 1 CI ####
#calculate the 95% CI for a sample of size n=400 drawn from our population
ci_test<-t.test(sample(population, 228),conf.level=0.95)

#take 500 repeated samples of size n=400
for (i in 1:500) {
  samples[[i]]<-sample(population, 228)
}

#calculate the means for each sample
sample_means <- data.frame(sapply(samples, function(x){mean(x)}))
sample_means$in_CI <- NA

#loop through each mean and check if it lands in our 95% CI. Flag as 1 if so, 0 if it falls outside
for (i in 1:500) {
  if ((sample_means[i,1]<ci_test$conf.int[[2]]) & (sample_means[i,1]>ci_test$conf.int[[1]])) {
    sample_means[i,2] <- 1
  }
  else {
    sample_means[i,2] <- 0
  }
}

#Calculate CI
sum(sample_means$in_CI)/length(sample_means$in_CI)


#Calculate CI for samples ranging in size from 100 to 500 observations
for (i in 10:1944) {
  conf[[i-9]]<-t.test(samp[[i]],conf.level=0.95)
}

#create a vector of the size of each interval using sapply then convert to a dataframe for graphing
intervals<-sapply(conf, function(x){x$conf.int[2]-x$conf.int[1]})
intervals_df<-data.frame(intervals)
intervals_df["size"]<-c(10:1944)

#plot the interval lengths
CI1 <- ggplot(intervals_df,
              aes(x = size, y = intervals))+
  geom_point()+
  geom_smooth()+
  xlab("Sample Size")+
  ylab("Confidence Interval Length") +
  scale_x_continuous(limits = c(1,500))+
  #scale_y_continuous(limits = c(0,200))+
  labs(title = "Bin 1 Confidence Intervals")+
  geom_vline(data = target1, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target1, mapping = aes(x = vals,
                                          y = 50,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  theme_classic()
CI1


##### Tag 2 ####
population <- tag2$Service.Connections
samp <- list()
samples <- list()
sem <- numeric()
conf <- list()

#function to compute the SEM
calc_sem <- function(x) {
  sd(x)/sqrt(length(x))
}

#Calculate SEM for samples ranging in size from 1 to 550 observations
for (i in 1:265) {
  samp[[i]]<-sample(population, i)
  sem[[i]]<-calc_sem(samp[[i]])
}

#creating a DF and adding a col for the sample size
sem_df<-data.frame(sem)
sem_df["size"]<-c(1:265)

#plot the SEM
plottag2 <- ggplot(sem_df,aes(x = size, y = sem))+
  geom_point()+
  geom_smooth(color = "orange")+
  xlab("Sample Size")+
  ylab("Standard Error of the Mean")+
  geom_vline(data = target2, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target2, mapping = aes(x = vals,
                                          y = 0,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  labs(title = "Bin 2")+
  scale_x_continuous(limits = c(0, 150))+
  scale_y_continuous(limits = c(0,110))+
  theme_classic()
plottag2

#### Tag 2 CI ####
#calculate the 95% CI for a sample of size n=400 drawn from our population
ci_test<-t.test(sample(population, 133),conf.level=0.95)

#take 500 repeated samples of size n=400
for (i in 1:500) {
  samples[[i]]<-sample(population, 133)
}

#calculate the means for each sample
sample_means <- data.frame(sapply(samples, function(x){mean(x)}))
sample_means$in_CI <- NA

#loop through each mean and check if it lands in our 95% CI. Flag as 1 if so, 0 if it falls outside
for (i in 1:500) {
  if ((sample_means[i,1]<ci_test$conf.int[[2]]) & (sample_means[i,1]>ci_test$conf.int[[1]])) {
    sample_means[i,2] <- 1
  }
  else {
    sample_means[i,2] <- 0
  }
}

#Calculate CI
sum(sample_means$in_CI)/length(sample_means$in_CI)


#Calculate CI for samples ranging in size from 100 to 500 observations
for (i in 10:265) {
  conf[[i-9]]<-t.test(samp[[i]],conf.level=0.95)
}

#create a vector of the size of each interval using sapply then convert to a dataframe for graphing
intervals<-sapply(conf, function(x){x$conf.int[2]-x$conf.int[1]})
intervals_df<-data.frame(intervals)
intervals_df["size"]<-c(10:265)

#plot the interval lengths
CI2 <- ggplot(intervals_df,
              aes(x = size, y = intervals))+
  geom_point()+
  geom_smooth()+
  xlab("Sample Size")+
  ylab("Confidence Interval Length") +
  #scale_x_continuous(limits = c(1000,2200))+
  #scale_y_continuous(limits = c(0,200))+
  labs(title = "Bin 2 Confidence Intervals")+
  geom_vline(data = target2, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target2, mapping = aes(x = vals,
                                          y = 50,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  theme_classic()
CI2



##### Tag 3 #####
population <- tag3$Service.Connections
samp <- list()
samples <- list()
sem <- numeric()
conf <- list()

#function to compute the SEM
calc_sem <- function(x) {
  sd(x)/sqrt(length(x))
}

#Calculate SEM for samples ranging in size from 1 to 550 observations
for (i in 1:108) {
  samp[[i]]<-sample(population, i)
  sem[[i]]<-calc_sem(samp[[i]])
}

#creating a DF and adding a col for the sample size
sem_df<-data.frame(sem)
sem_df["size"]<-c(1:108)

#plot the SEM
plottag3 <- ggplot(sem_df,aes(x = size, y = sem))+
  geom_point()+
  geom_smooth(color = "orange")+
  xlab("Sample Size")+
  ylab("Standard Error of the Mean")+
  geom_vline(data = target3, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target3, mapping = aes(x = vals,
                                          y = 0,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  labs(title = "Bin 3")+
  scale_x_continuous(limits = c(0, 108))+
  scale_y_continuous(limits = c(0,300))+
  theme_classic()
plottag3

#### Tag 3 CI ####
#calculate the 95% CI for a sample of size n=400 drawn from our population
ci_test<-t.test(sample(population, 84),conf.level=0.95)

#take 500 repeated samples of size n=400
for (i in 1:500) {
  samples[[i]]<-sample(population, 84)
}

#calculate the means for each sample
sample_means <- data.frame(sapply(samples, function(x){mean(x)}))
sample_means$in_CI <- NA

#loop through each mean and check if it lands in our 95% CI. Flag as 1 if so, 0 if it falls outside
for (i in 1:500) {
  if ((sample_means[i,1]<ci_test$conf.int[[2]]) & (sample_means[i,1]>ci_test$conf.int[[1]])) {
    sample_means[i,2] <- 1
  }
  else {
    sample_means[i,2] <- 0
  }
}

#Calculate CI
sum(sample_means$in_CI)/length(sample_means$in_CI)


#Calculate CI for samples ranging in size from 100 to 500 observations
for (i in 10:108) {
  conf[[i-9]]<-t.test(samp[[i]],conf.level=0.95)
}

#create a vector of the size of each interval using sapply then convert to a dataframe for graphing
intervals<-sapply(conf, function(x){x$conf.int[2]-x$conf.int[1]})
intervals_df<-data.frame(intervals)
intervals_df["size"]<-c(10:108)

#plot the interval lengths
CI3 <- ggplot(intervals_df,
              aes(x = size, y = intervals))+
  geom_point()+
  geom_smooth()+
  xlab("Sample Size")+
  ylab("Confidence Interval Length") +
  #scale_x_continuous(limits = c(1000,2200))+
  #scale_y_continuous(limits = c(0,200))+
  labs(title = "Bin 3 Confidence Intervals")+
  geom_vline(data = target3, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target3, mapping = aes(x = vals,
                                          y = 50,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  theme_classic()
CI3



##### Tag 4 ####
population <- tag4$Service.Connections
samp <- list()
samples <- list()
sem <- numeric()
conf <- list()

#function to compute the SEM
calc_sem <- function(x) {
  sd(x)/sqrt(length(x))
}

#Calculate SEM for samples ranging in size from 1 to 550 observations
for (i in 1:72) {
  samp[[i]]<-sample(population, i)
  sem[[i]]<-calc_sem(samp[[i]])
}

#creating a DF and adding a col for the sample size
sem_df<-data.frame(sem)
sem_df["size"]<-c(1:72)

#plot the SEM
plottag4 <- ggplot(sem_df,aes(x = size, y = sem))+
  geom_point()+
  geom_smooth(color = "orange")+
  xlab("Sample Size")+
  ylab("Standard Error of the Mean")+
  geom_vline(data = target4, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target4, mapping = aes(x = vals,
                                          y = 0,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  labs(title = "Bin 4")+
  scale_x_continuous(limits = c(0, 72))+
  scale_y_continuous(limits = c(0,300))+
  theme_classic()
plottag4


#### Tag 4 CI ####
#calculate the 95% CI for a sample of size n=400 drawn from our population
ci_test<-t.test(sample(population, 65),conf.level=0.95)

#take 500 repeated samples of size n=400
for (i in 1:500) {
  samples[[i]]<-sample(population, 65)
}

#calculate the means for each sample
sample_means <- data.frame(sapply(samples, function(x){mean(x)}))
sample_means$in_CI <- NA

#loop through each mean and check if it lands in our 95% CI. Flag as 1 if so, 0 if it falls outside
for (i in 1:500) {
  if ((sample_means[i,1]<ci_test$conf.int[[2]]) & (sample_means[i,1]>ci_test$conf.int[[1]])) {
    sample_means[i,2] <- 1
  }
  else {
    sample_means[i,2] <- 0
  }
}

#Calculate CI
sum(sample_means$in_CI)/length(sample_means$in_CI)


#Calculate CI for samples ranging in size from 100 to 500 observations
for (i in 10:72) {
  conf[[i-9]]<-t.test(samp[[i]],conf.level=0.95)
}

#create a vector of the size of each interval using sapply then convert to a dataframe for graphing
intervals<-sapply(conf, function(x){x$conf.int[2]-x$conf.int[1]})
intervals_df<-data.frame(intervals)
intervals_df["size"]<-c(10:72)

#plot the interval lengths
CI4 <- ggplot(intervals_df,
              aes(x = size, y = intervals))+
  geom_point()+
  geom_smooth()+
  xlab("Sample Size")+
  ylab("Confidence Interval Length") +
  #scale_x_continuous(limits = c(1000,2200))+
  #scale_y_continuous(limits = c(0,200))+
  labs(title = "Bin 4 Confidence Intervals")+
  geom_vline(data = target4, mapping = aes(xintercept = vals, color = Ref))+
  geom_text(data = target4, mapping = aes(x = vals,
                                          y = 50,
                                          label = Ref,
                                          hjust = +1,
                                          vjust = -1))+
  theme_classic()
CI4



##### plot all together ####
SEMplot <- grid.arrange(plottag1,plottag2,plottag3,plottag4, total)
CIplot <- grid.arrange(CI1, CI2, CI3, CI4)

ggsave(path = "plots",
       filename = "SEMPlot.png",
       SEMplot,
       width = 10,
       scale = 2,
       dpi = 500)

ggsave(path = "plots",
       filename = "CIplot.png",
       CIplot,
       width = 10,
       scale = 2,
       dpi = 500)







#using (non-parametric) solvin's formula to calculate confidence level
#note that this does not calculate statistical power

## n = N / (1 +Ne^2)
# n = num samples
# N = total population
# e = error tolerance level

## e = sqrt((N - n)/(N * n)) 

## Total population
N <- 2389 #total pop
n <- 510 #sample size
e.total = 100*(1 - sqrt((N - n)/(N * n))) 

## 80% Response rate
N <- 2389 #total pop
n <- (0.8*510) #sample size
e.80 = 100*(1 - sqrt((N - n)/(N * n))) 

Bin.A.pop <- 1944
Bin.B.pop <- 265
Bin.C.pop <- 107
Bin.D.pop <- 73


#Bins
bins <- df %>% 
  group_by(tag) %>% 
  summarize(PopProportion = n()) #total number of water systems
            #SamProportion = count(sample)/510, #total number of samples
            #pw = PopProportion / SamProportion) #calculate weight
bins

## Bin Response rate
N <- Bin.A.pop #total pop
n <- 228 #sample size
e.BinA = 100*(1 - sqrt((N - n)/(N * n))) 

N <- Bin.B.pop #total pop
n <- 133 #sample size
e.BinB = 100*(1 - sqrt((N - n)/(N * n))) 

N <- Bin.C.pop #total pop
n <- 84 #sample size
e.BinC = 100*(1 - sqrt((N - n)/(N * n))) 

N <- Bin.D.pop #total pop
n <- 65 #sample size
e.BinD = 100*(1 - sqrt((N - n)/(N * n))) 
