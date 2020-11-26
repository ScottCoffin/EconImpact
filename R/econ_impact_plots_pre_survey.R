#### Dependencies #####
library(tidyverse) #load dependencies
library(BAMMtools) #jenks
library(GmAMisc) #alt Jenks
library(cowplot) #plotting multiple graphs
library(rstatix) #pipe-friendly R fnx for stats
library(ggpubr) #easy plots
library(grid) #to make grobs
library(gridExtra) # to make multiple plots
library(calecopal) #colors

#### Load in Data ####
econ1 <- read.csv("Datasets/CWS.csv", header =T, na.strings = "")


#### Filter Data ####
#filter out non-community and transient connections
#filter out LARGE water systems like LA and San Diego (cap at 10,000 service)
#Code in SDWIS for fees. If they're getting disadvantaged fees, they are flagged. Voluntary request though, so likely not representative.
#don't need to filter because Bethany already did

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

#### Normality Check ####
#Shapiro test for normality
econ3 %>% 
  group_by(tag) %>% 
  shapiro_test(logService.Connections)


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
       subtitle = "n = 1944, p = 6.76e-17")

#MEDIUM
ggdensity(econB$Service.Connections, fill = "red")
qplotB<- ggqqplot(econB$Service.Connections, fill = "seagreen4") +
  labs(title = "Bin B",
       subtitle = "n = 265, p = 3.16e-7")
#Large
qplotC<- ggqqplot(econC$Service.Connections, fill = "blue") +
  labs(title = "Bin C",
       subtitle = "n = 107, p = 5.68e-4")
#verylarge
qplotD<- ggqqplot(econD$Service.Connections, fill = "yellow") +
  labs(title = "Bin D",
       subtitle = "n = 73, p =1.34e-3")

#### Combine qplots ####
plot_grid(qplotA, qplotB, qplotC, qplotD,
          ncol =2,
          nrow= 3)



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
  ggplot(mapping = aes(x=log10(Service.Connections),
                       color = Fee.Code,
                       fill = Fee.Code)) + 
  geom_histogram(alpha=0.7) + #color = "red", fill = "red"  
 # geom_density(alpha = 0.2, color = "black", position = "stack") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Frequency",
       title = "Bin A",
       subtitle = "n = 1944")

#medium plot
BinB <- filter(econ3, tag =="Bin B") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections),
                       color = Fee.Code,
                       fill = Fee.Code)) + 
  geom_histogram(alpha=0.7) + # color = "seagreen4", fill = "seagreen4") + 
 # geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Frequency",
       title = "Bin B",
       subtitle = "n = 265")

#Bin C plot
BinC <- filter(econ3, tag =="Bin C") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections),
                       color = Fee.Code,
                       fill = Fee.Code))+
  geom_histogram(alpha=0.7) +  #,color = "blue", fill = "blue") + 
 # geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Frequency",
       title = "Bin C",
       subtitle = "n = 107")

#very large plot
BinD <- filter(econ3, tag =="Bin D") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections),
                       color = Fee.Code,
                       fill = Fee.Code))+
  geom_histogram(alpha=0.7) + #, color = "purple", fill = "purple") + 
  #geom_density(alpha = 0.2, color = "black") +
  geom_rug() +
  labs(x='LOG10 Service Connections',
       y = "Frequency",
       title = "Bin D",
       subtitle = "n = 73",
       caption = "Data from SDWIS")

#Plot all together

logplots<- plot_grid(BinA, BinB, BinC, BinD,combined,
          ncol =2,
          nrow= 3)

ggsave(path = "plots",
       filename = "logplot.png",
       logplots,
       width = 5,
       scale = 2,
       dpi = 500)


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
  ggplot(mapping = aes(x=Service.Connections, color = Fee.Code, fill = Fee.Code)) + 
  geom_histogram(binwidth = 10,alpha=0.7) + 
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

linearplots<- grid.arrange(BinA, BinB, BinC, BinD,combined,
             ncol =2,nrow= 3,
             top = textGrob("Linear Histograms", gp=gpar(fontsize = 30, font=4)))

ggsave(path = "plots",
  filename = "linearplot.png",
  linearplots,
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
       caption = "Breaks: 1, 1045,3339, 6360, 9944")

# plot percentage stacked
stacked <- ggplot(data = biasSumm, aes(fill = Fee.Code, x= tag, y = count_by_feecodeTag)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x='Bin',
       y = "PWS'in Each bin",
       title = "Water Systems Binned by Jenks Natural Breaks",
       subtitle = "Stacked Percentage Chart",
       caption = "Breaks: 1, 1045,3339, 6360, 9944")

# plot grouped
grouped <- ggplot(data = biasSumm, aes(fill = Fee.Code, x= tag, y = count_by_feecodeTag)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x='Bin',
       y = "PWS'in Each bin",
       title = "Water Systems Binned by Jenks Natural Breaks",
       subtitle = "Group Bar Chart",
       caption = "Breaks: 1, 1045,3339, 6360, 9944")


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


#### Interesting Plots ####
filter(econ3, tag =="Bin A") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections), fill = Fee.Code)) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  geom_density(alpha=0.4)


filter(econ3, Fee.Code =="DAVCS") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections))) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  geom_density(alpha = 0.4) +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Small Community Water Systems (Fee Code = SC)",
       subtitle = "n = 1700")

#plot by fee code
DAVCL<- filter(econ3, Fee.Code =="DAVCL") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections), fill = tag)) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  geom_density(alpha = 0.4) +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Disadvantaged Large Community Water Systems (Fee Code = DAVCL)",
       subtitle = "n = 124")

DAVCS<- filter(econ3, Fee.Code =="DAVCS") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections)), fill = tag) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  geom_density(alpha = 0.4) +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Disadvantaged Community Water Systems (Fee Code = DAVCS)",
       subtitle = "n = 238")

C1<- filter(econ3, Fee.Code =="C1") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections), fill = tag)) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  geom_density(alpha = 0.4) +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Large Community Water Systems (Fee Code = C1)",
       subtitle = "n = 327")

SC<- filter(econ3, Fee.Code =="SC") %>% 
  ggplot(mapping = aes(x=log10(Service.Connections), fill = tag)) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  geom_density(alpha = 0.4) +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Small Community Water Systems (Fee Code = SC)",
       subtitle = "n = 1700")

#all together
FeeCodeBin<- grid.arrange(DAVCL, DAVCS, C1, SC,
                          ncol =2,nrow= 2,
                          top = textGrob("Water Systems Fee Codes By Bin", gp=gpar(fontsize = 30, font=4)))

#Save
ggsave(path = "plots",
       filename = "FeeCodeBin.png",
       FeeCodeBin,
       width = 10,
       scale = 2,
       dpi = 500)

tagbycode<- econ3 %>% 
  ggplot(mapping = aes(x=log10(Service.Connections), fill = tag)) + 
  geom_histogram(aes(y=..density..),alpha=0.7)+
  facet_grid(~Fee.Code) +
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Fee Codes by Bins")+
  theme_bw()


## Kernel density by bin

econ3 %>% 
  ggplot(mapping = aes(x=log10(Service.Connections))) + 
  geom_density(aes(fill = Fee.Code),alpha=0.3)+
  geom_histogram(aes(y=..density..,fill = tag, alpha = 0.7))+
  labs(x='LOG10 Service Connections',
       y = "Density",
       title = "Fee Codes by Bins")

#make data table from breaks
binsum <- econ3 %>% 
  group_by(tag) %>% 
  summarise(count = n(), low = min(log10(Service.Connections)), max = max(log10(Service.Connections)))
binsum

binsumlinear <- econ3 %>% 
  group_by(tag) %>% 
  summarise(count = n(),low = min(Service.Connections), max = max(Service.Connections))
binsumlinear

log10breaks <- log10(breaks)
breaksdf <-as.data.frame(log10breaks) %>% 
  slice(1:4)

breaksdf <- cbind(breaksdf, binsum) 
breaksdf <- rename(breaksdf, 
                   Breaks = log10breaks) 
                   
breaksdf

##### Final Plots #####
#map fee code distributions showing Jenk's breaks
# LOG10
FeeCodeDistributions <- ggplot(econ3, aes(x=Service.Connections, fill = Fee.Code)) + 
  geom_histogram(position = "identity",alpha=0.6, bins = 55, color = "gray") +
  #geom_density(alpha=0.2)+
 # geom_rug() +
  labs(x='LOG10 Service Connections',y = "Count",
       title = "Histogram of Water Sytems Considered For Survey Sampling", subtitle = "Sampling strata breaks shown in dotted lines")+
  scale_fill_manual(name = "Fee Code", 
                    labels = c("Large Water System", "Disadvantaged Large Community Water System", "Disadvantaged Small Community Water System", "Small Community"),
                    values = cal_palette("superbloom3"))+
 # scale_color_manual(name = "Fee Code", 
  #                   labels = c("Large Water System", "Disadvantaged Large Community Water System", "Disadvantaged Small Community Water System", "Small Community"),
   #                  values = cal_palette("superbloom3")) +
  #geom_vline(data = breaksdf, mapping = aes(xintercept= log10(breaks)), linetype = 'dashed') +
  geom_vline(xintercept = c(3.019116, 3.523616, 3.803457), linetype ='dashed') +
  annotate("text",x = 3.02/2, y = 120, label = "n = 1944", size =5)+ #bin A label
  annotate("text",x = ((3.52 - 3.02)/2)+3.02, y = 120, label = "n = 265", size =5)+ #bin B label
  annotate("text",x = ((3.81 - 3.52)/2)+3.52, y = 120, label = "n = 108", size =5)+ #bin C label
  annotate("text",x = ((4.00 - 3.81)/2)+3.81, y = 120, label = "n = 72", size =5)+ #bin D label
 theme_half_open() + 
  theme(legend.position = c(0.03, 0.83),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 18, face = "bold"))

FeeCodeDistributions


ggsave(path = "plots",
       filename = "Fee Code Distributions.png",
       FeeCodeDistributions,
       width = 10,
       scale = 2,
       dpi = 500)


#Linear

FeeCodeDistributionsLinear <- ggplot(econ3, aes(x = Service.Connections, fill = Fee.Code)) + 
  geom_histogram(position = "stack",alpha=0.6, bins = 100, color = "gray") +
  scale_x_log10() +
  annotation_logticks(base = 10, #log 10
                      sides = "b")+ #only bottom
  #geom_density(alpha=0.2)+
  # geom_rug() +
  labs(x='Service Connections',y = "Count",
       title = "Histogram of Water Sytems Considered For Survey Sampling", subtitle = "Sampling strata breaks shown in dotted lines; stacked bins",
       caption = "Data from SDWIS (10/2020)")+
  scale_fill_manual(name = "Fee Code", 
                    labels = c("Large Water System", "Disadvantaged Large Community Water System", "Disadvantaged Small Community Water System", "Small Community"),
                    values = cal_palette("superbloom3"))+
  # scale_color_manual(name = "Fee Code", 
  #                   labels = c("Large Water System", "Disadvantaged Large Community Water System", "Disadvantaged Small Community Water System", "Small Community"),
  #                  values = cal_palette("superbloom3")) +
  #geom_vline(data = breaksdf, mapping = aes(xintercept= log10(breaks)), linetype = 'dashed') +
  geom_vline(xintercept = c(1009, 3315, 6360), linetype ='dashed') +
   annotate("text",x = (1009/2)/10, y = 80, label = "n = 1944", size =6)+ #bin A label
   annotate("text",x = 2000, y = 80, label = "n = 265", size =6)+ #bin B label
   annotate("text",x = 4500, y = 80, label = "n = 108", size =6)+ #bin C label
   annotate("text",x = 9000, y = 80, label = "n = 72", size =6)+ #bin D label
   theme_half_open() + 
  theme(legend.position = c(0.01, 0.83),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15, face = "bold",),
        axis.text = element_text(size = 15))

FeeCodeDistributionsLinear


ggsave(path = "plots",
       filename = "Fee Code Distributions Linear_identity.png",
       FeeCodeDistributionsLinear_identity,
       width = 10,
       scale = 2,
       dpi = 500)

# 2D
ggplot(data = econ3, aes(x= Service.Connections, y = tag, color = Fee.Code, size = Population)) + 
  geom_point(alpha = 0.7)

  #population vs service connection
scatter1 <- ggplot(data = econ3, aes(x= Population, y = Service.Connections, color = Fee.Code)) + 
  geom_point(alpha = 0.6) +
  geom_smooth()+
  labs(x='Population',
       y = "Service Connections",
       title = "Water Systems By Population and Service Connection")

ggsave(path = "plots",
              filename = "scatter1.png",
              scatter1,
              width = 10,
              scale = 2,
              dpi = 500)
