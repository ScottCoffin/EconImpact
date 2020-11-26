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
library(readxl) #to read in excel docs

#### Load in Data ####
#load in all systems considered for survey (created from econ_impacts_plots_pre_survey.R)
allSystems <- read.csv("Datasets/econ3.csv", header =T, na.strings = "", stringsAsFactors = TRUE) #filtered dataset used to create original bins
#load in systems list for survey (created from pre-survey analysis.R)
surveyList <- read.csv("Datasets/surveyList.csv", header =T, na.strings = "", stringsAsFactors = TRUE)
#load in completed surveys list (from Marielle)
completedSurveys <- read_excel("Datasets/test_read_sm_systems_1125_4pm.xlsx", na = "") %>% 
  mutate(completed = "y") #to specify completed

#PWSIDs with a 0 in the beginning have them removed. Must put them back.
surveyList$Water.System.No. <- as.character(surveyList$Water.System.No.)
#fix PWSIDs with missing CA and 0 heads
surveyList <- surveyList %>% 
mutate(PWSID = paste0("CA", 
                      case_when(nchar(Water.System.No.) < 7 ~ paste0("0",Water.System.No.),
                         nchar(Water.System.No.) >= 7 ~ paste0(Water.System.No.))
                      )
       )
summary(nchar(surveyList$PWSID)) #to ensure all were converted

# examine PWSIDs in completed surveys
summary(nchar(completedSurveys$PWSID)) 
# need to fix the multiple PWSIDs...

# Join survey list with completed list
fullList <- left_join(surveyList, completedSurveys, by = "PWSID") %>% 
  select(!c(X, Water.System.No., Unnamed, sys_name, district, filename, log.SC))

#convert unfilled surveys to no's
fullList$completed <- fullList$completed %>%  
  replace_na("n") %>% 
  as.factor()

#examine 
summary(fullList$completed)  

#Determine proportion complete by bin
tagSummary <- fullList %>% 
  group_by(tag, completed) %>% 
  summarize(num.completed = n()) %>% 
  mutate(total = sum(num.completed),
         proportion.completed = num.completed / sum(num.completed)) %>% 
  filter(completed == "y") %>% 
  select(!completed)
tagSummary

# Determine percent complete by fee code
feeCodeSummary <- fullList %>% 
  group_by(Fee.Code, completed) %>% 
  summarize(num.completed = n()) %>% 
  mutate(total = sum(num.completed),
         proportion.completed = num.completed / sum(num.completed)) %>% 
  filter(completed == "y") %>% 
  select(!completed)
feeCodeSummary

# Determine percent complete by District
districtSummary <- fullList %>%
 filter(!DS.Class == "TD") %>% 
  filter(!DS.Class == "NA") %>% 
  filter(!DS.Class == "NR") %>% 
  group_by(DS.Class, completed) %>% 
  summarize(num.completed = n()) %>% 
  mutate(total = sum(num.completed),
         proportion.completed = num.completed / sum(num.completed)) %>% 
  filter(completed == "y") %>% 
  select(!completed)


# plot % completeness by factors
plot.dist<- ggplot(data = districtSummary, aes(x = DS.Class, y = proportion.completed, fill = DS.Class)) +
  geom_col() +
  coord_cartesian(ylim = c(0.5,1))+
  geom_text(aes(label = num.completed),
            vjust = -1)+
  theme(legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
plot.dist

plot.feeCode <- ggplot(data = feeCodeSummary, aes(x = Fee.Code, y = proportion.completed, fill = Fee.Code)) +
  geom_col() +
  coord_cartesian(ylim = c(0.5,1)) +
  geom_text(aes(label = num.completed),
            vjust = -1)+
  theme(legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

plot.tag <- ggplot(data = tagSummary, aes(x = tag, y = proportion.completed, fill = tag)) +
  geom_col()+
  coord_cartesian(ylim = c(0.5,1)) +
  geom_text(aes(label = num.completed),
            vjust = -1)+
  theme(legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
#arrange together
plotProportionComplete <- grid.arrange(plot.dist, plot.feeCode, plot.tag,
             ncol = 2,
             top = textGrob("Proportion Completed", gp=gpar(fontsize = 22, font=1)))
#save
ggsave(path = "plots",
       filename = "proportionComplete.png",
       plotProportionComplete,
       width = 5,
       scale = 2,
       dpi = 500)
