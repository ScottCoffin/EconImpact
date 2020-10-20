#### Dependencies #####
library(tidyverse) #load dependencies
library(BAMMtools) #jenks
library(GmAMisc) #alt Jenks
library(cowplot) #plotting multiple graphs
library(rstatix) #pipe-friendly R fnx for stats
library(ggpubr) #easy plots
library(grid) #to make grobs
library(gridExtra) # to make multiple plots

#### Load in Data ####
econ3 <- read.csv("CWS.csv", header =T, na.strings = "")


#### Filter Data ####
#filter out non-community and transient connections
#filter out LARGE water systems like LA and San Diego (cap at 10,000 service)
#Code in SDWIS for fees. If they're getting disadvantaged fees, they are flagged. Voluntary request though, so likely not representative.
#don't need to filter because Bethany already did

#Count each fee code category
econ3 %>% 
  group_by(Fee.Code) %>% 
  summarize(n())

## filter data ##
econ3<- econ3 %>% 
 filter(Service.Connections <10000) %>% #filter above 2 because there are many
  filter(Fee.Code !="WH")  #filter out wholesalers 

#re-count groups
econ3 %>% 
  group_by(Fee.Code) %>% 
  summarize(n())

#### Natural Breaks ####
#Determine natural breaks and assign
breaks <- getJenksBreaks(econ3$Service.Connections, 5)

#alternative method with stats, very slow
#plotJenks(econ3$Service.Connections, n=4) 

##### Breaks from Bethany ####
# SCs from 0 to 1009: 2,398 systems
# SCs from 1009 to 3090: 232 systems
# SCs from 3090 to 5868: 89 systems
# SCs from 5868 to 9890: 69 systems
# assign breaks manually (not applicable if using above)
#breaks <- c(0,1009,3090,5868,9890) #enter based on above

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
    Service.Connections >= 1 & Service.Connections < 1045 ~tags[1],
    Service.Connections >= 1045 & Service.Connections < 3339 ~tags[2],
    Service.Connections >= 3339 & Service.Connections <= 6360 ~tags[3],
    Service.Connections >= 6360 & Service.Connections <= 9944 ~tags[4],
  ))

#tag is character vector, so convert to factor
econ3$tag <- factor(econ3$tag,
                    levels = tags,
                    ordered = FALSE)



#### Normality Check ####
# Density plot
econA <- econ3 %>% 
  filter(tag == "Bin A")

econB <- econ3 %>% 
  filter(tag == "Bin B")

econC <- econ3 %>% 
  filter(tag == "Bin C")

econD <- econ3 %>% 
  filter(tag == "Bin D")

#SMALL
ggdensity(econA$Service.Connections, fill = "red")
qplotA <- ggqqplot(econA$Service.Connections, fill = "red") +
  labs(title = "Bin A",
       subtitle = "n = 1971, p = 4.87e-52")

#MEDIUM
ggdensity(econB$Service.Connections, fill = "red")
qplotB<- ggqqplot(econB$Service.Connections, fill = "seagreen4") +
  labs(title = "Bin B",
       subtitle = "n = 265, p = 6.82e-9")
#Large
qplotC<- ggqqplot(econC$Service.Connections, fill = "blue") +
  labs(title = "Bin C",
       subtitle = "n = 107, p = 1.70e-4")
#verylarge
qplotD<- ggqqplot(econD$Service.Connections, fill = "yellow") +
  labs(title = "Bin D",
       subtitle = "n = 73, p =1.03e-3")

#### Combine qplots ####
plot_grid(qplotA, qplotB, qplotC, qplotD,
          ncol =2,
          nrow= 3)


#Shapiro test for normality
econ3 %>% 
  group_by(tag) %>% 
  shapiro_test(Service.Connections)

### Data Visualization ####
#Plot as histograms
combined <- ggplot(data = econ3, mapping = aes(x=log10(Service.Connections),
                                               color = tag,
                                               fill = tag)) + 
  geom_histogram(aes(y=..density..),alpha=0.7) + 
  geom_density(alpha =0.2) +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Water Systems Binned by Jenks Natural Breaks",
       caption = "Data from SDWIS")
theme_minimal()

##plot each group individually ##
#Small Plot
BinA <- filter(econ3, tag =="Bin A") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections))) + 
  geom_histogram(aes(y=..density..),alpha=0.7,color = "red", fill = "red") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Bin A",
       subtitle = "n = 1971")

#medium plot
BinB <- filter(econ3, tag =="Bin B") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections))) + 
  geom_histogram(aes(y=..density..),alpha=0.7, color = "seagreen4", fill = "seagreen4") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Bin B",
       subtitle = "n = 265")

#large plot
BinC <- filter(econ3, tag =="Bin C") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections)))+
  geom_histogram(aes(y=..density..),alpha=0.7, color = "blue", fill = "blue") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Bin C",
       subtitle = "n = 107")

#very large plot
BinD <- filter(econ3, tag =="Bin D") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections)))+
  geom_histogram(aes(y=..density..),alpha=0.7, color = "purple", fill = "purple") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Bin D",
       subtitle = "n = 73",
       caption = "Data from SDWIS")

#Plot all together

plot_grid(BinA, BinB, BinC, BinD,combined,
          ncol =2,
          nrow= 3)


#### Plot with ;linear x-scale and manual bins ####

#combined
combined <- ggplot(data = econ3, mapping = aes(x=Service.Connections,
                                               color = tag,
                                               fill = tag)) + 
  geom_histogram(aes(y=..density..),alpha=0.7) + 
  geom_density(alpha =0.2) +
  geom_rug() +
  labs(x='Service Connections',
       y = "Density",
       title = "Water Systems Binned by Jenks Natural Breaks",
       caption = "Data from SDWIS")
theme_minimal()

# Bin A
BinA <- filter(econ3, tag =="Bin A") %>% 
  ggplot(mapping = aes(x=Service.Connections)) + 
  geom_histogram(binwidth = 10,alpha=0.7,color = "red", fill = "red") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='Service Connections',
       y = "Density",
       title = "Bin A",
       subtitle = "n = 1971")

#medium plot
BinB <-filter(econ3, tag =="Bin B") %>% 
  ggplot(mapping = aes(x=Service.Connections)) + 
  geom_histogram(binwidth =60,alpha=0.7, color = "seagreen4", fill = "seagreen4") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='Service Connections',
       y = "Density",
       title = "Bin B",
       subtitle = "n = 265")

#large plot
BinC <- filter(econ3, tag =="Bin C") %>% 
  ggplot(mapping = aes(x=Service.Connections))+
  geom_histogram(binwidth = 100,alpha=0.7, color = "blue", fill = "blue") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='Service Connections',
       y = "Density",
       title = "Bin C",
       subtitle = "n = 107")

#very large plot
BinD <- filter(econ3, tag =="Bin D") %>% 
  ggplot(mapping = aes(Service.Connections))+
  geom_histogram(binwidth = 120,alpha=0.7, color = "purple", fill = "purple") + 
  geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='Service Connections',
       y = "Density",
       title = "Bin D",
       subtitle = "n = 73",
       caption = "Data from SDWIS")

#Plot all together

grid.arrange(BinA, BinB, BinC, BinD,combined,
             ncol =2,nrow= 3,
             top = textGrob("Linear Histograms", gp=gpar(fontsize = 30, font=4)))

ggsave(path = "plots",
  filename = "linearplot.png",
  width = 5,
  scale = 2,
    dpi = 500)

#### Analysis of Bias ####

#summarize in table
biasSumm <- econ3 %>% 
  group_by(Fee.Code, tag) %>% 
  summarize(count_by_feecodeTag = n(),
            mean_serv.conn. = mean(Service.Connections, na.rm = TRUE),
            med_serv.conn = median(Service.Connections, na.rm = TRUE),
            sd_serv.conn. = sd(Service.Connections, na.rm = TRUE))

# plot percentage stacked
percentstacked <- ggplot(data = biasSumm, aes(fill = Fee.Code, x= tag, y = count_by_feecodeTag)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x='Bin',
       y = "Proportion of PWS'in Each bin",
       title = "Water Systems Binned by Jenks Natural Breaks",
       subtitle = "Stacked Percentage Chart",
       caption = "Data from SDWIS")

# plot percentage stacked
stacked <- ggplot(data = biasSumm, aes(fill = Fee.Code, x= tag, y = count_by_feecodeTag)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x='Bin',
       y = "PWS'in Each bin",
       title = "Water Systems Binned by Jenks Natural Breaks",
       subtitle = "Stacked Percentage Chart",
       caption = "Data from SDWIS")

# plot grouped
grouped <- ggplot(data = biasSumm, aes(fill = Fee.Code, x= tag, y = count_by_feecodeTag)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x='Bin',
       y = "PWS'in Each bin",
       title = "Water Systems Binned by Jenks Natural Breaks",
       subtitle = "Group Bar Chart",
       caption = "Data from SDWIS")


#all together
BinFeeCode<- grid.arrange(percentstacked, grouped,
             ncol =2,nrow= 1,
             top = textGrob("Water Systems Bins By Fee Code", gp=gpar(fontsize = 30, font=4)))

#Save
ggsave(path = "plots",
  filename = "WaterSystemsGroupedByFeeCodes.png",
  BinFeeCode,
  width = 10,
  scale = 2,
  dpi = 500)
