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
#allSystems <- read.csv("Datasets/econ3.csv", header =T, na.strings = "", stringsAsFactors = TRUE) #filtered dataset used to create original bins

#ALL Community Water Systems
AllSystems <- read_excel("Datasets/CWS_fmt_PWSID.xlsx", na = "")

#load in systems list for survey (created from pre-survey analysis.R)
surveyList <- read.csv("Datasets/surveyList.csv", header =T, na.strings = "", stringsAsFactors = TRUE)
#load in completed surveys list (from Marielle)
#completedSurveys_old <- read_excel("Datasets/test_read_sm_systems_1125_4pm.xlsx", na = "") %>% 
 # mutate(completed = "y") #to specify completed
## load in updated list
# completedSurveys <- read_excel("Datasets/test_read_sm_systems_1130_11am.xlsx", na = "") %>% 
#   mutate(completed = "y") #to specify completed

completedSurveys <- read.csv("Datasets/dataset_submitted_1207.csv",  header =T, na.strings = "", stringsAsFactors = TRUE) %>% 
   mutate(completed = "y") #to specify completed


#### Clean Up Data ####
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
summary(nchar(as.character(completedSurveys$PWSID)))
# All good!


#Reassign tags
#specify bin labels
Post.Bin <- c("Bin A", "Bin B", "Bin C", "Bin D")
breaks = c(1, 1045, 3339, 6360, 9944) #breaks from original dataset
#bucket values into bins
bins <- cut(completedSurveys$Service.Connections,
            breaks = breaks,
            include.lowest = TRUE,
            right = FALSE,
            labels = Post.Bin)

#inspect bins
summary(bins)

#specify bin labels
tags <- c("Bin A", "Bin B", "Bin C", "Bin D")

breaks
#Store group as new column
completedSurveys <-as_tibble(completedSurveys) %>% 
  mutate(tag = case_when(
    Service.Connections >= breaks[1] & Service.Connections < breaks[2] ~tags[1],
    Service.Connections >= breaks[2] & Service.Connections < breaks[3] ~tags[2],
    Service.Connections >= breaks[3] & Service.Connections <= breaks[4] ~tags[3],
    Service.Connections >= breaks[4] & Service.Connections <= breaks[5] ~tags[4],
  )) %>% 
  mutate(logService.Connections = log10(Service.Connections))

#convert tag to factor
completedSurveys$tag <- as.factor(completedSurveys$tag)

#final dataset (just completed)
write.csv(completedSurveys, "Datasets/completedSurveys.csv")

# #tag is character vector, so convert to factor
# completedSurveys$tag <- factor(completedSurveys$tag,
#                     levels = tag,
#                     ordered = FALSE)

surveyListTag <- surveyList %>% 
  select(PWSID, tag, Water.System.No.)

surveyListTag$Water.System.No. <- as.integer(surveyListTag$Water.System.No.)

# Join survey list with completed list
fullList <- left_join(surveyListTag, completedSurveys, by = "Water.System.No.")  %>% 
  select(!c(X, tag.y, PWSID.x)) %>%  #Water.System.No, Unnamed, sys_name, district, filename, log.SC))
  mutate(tag = tag.x, PWSID = PWSID.y) %>% 
  select(!c(tag.x, PWSID.y, logService.Connections))

#convert unfilled surveys to no's
fullList$completed <- fullList$completed %>%  
  replace_na("n") %>% 
  as.factor()

#examine
summary(fullList$completed)


#### Join survey list with FULL LIST ####
AllSystems$Service.Connections<-  AllSystems$`Service Connections`

#Store TAG as new column in FULL LIST
AllSystems <-as_tibble(AllSystems) %>% 
  mutate(tag = case_when(
    Service.Connections >= breaks[1] & Service.Connections < breaks[2] ~tags[1],
    Service.Connections >= breaks[2] & Service.Connections < breaks[3] ~tags[2],
    Service.Connections >= breaks[3] & Service.Connections <= breaks[4] ~tags[3],
    Service.Connections >= breaks[4] & Service.Connections <= breaks[5] ~tags[4])) 

#convert tag to factor
AllSystems$tag <- as.factor(AllSystems$tag)
summary(AllSystems$tag)

AllSystems$Water.System.No. <- AllSystems$`Water System No.`

AllSystems_Surveys <- left_join(AllSystems, completedSurveys, by = "Water.System.No.")  %>% 
  select(!c(X, PWSID.x)) %>%  #Water.System.No, Unnamed, sys_name, district, filename, log.SC))
  mutate(PWSID = PWSID.y, Population = Population.x, tag = tag.y) %>% 
  select(!c(PWSID.y, tag.y, logService.Connections, Population.x, Water.System.No.:Last.SNSV))
  
#convert unfilled surveys to no's
AllSystems_Surveys$completed <- AllSystems_Surveys$completed %>%  
  replace_na("n") %>% 
  as.factor() 

#convert all characters to factors
AllSystems_Surveys <- AllSystems_Surveys %>% 
  mutate_if(is.character, as.factor)

#examine
summary(AllSystems_Surveys$completed)

write.csv(AllSystems_Surveys,"Datasets/AllSystems_Surveys.csv")

##### Summary Stats Method #####
## This is done because many water systems can't be joined with sample list. Likely due to systems not being in original list
  #Summarize bin for SURVEY LIST
surveyListSumTag <- surveyList %>% 
  group_by(tag) %>% 
  summarize(total = n())
surveyListSumTag
#Summarize fee code for SURVEY LIST
surveyListSumFeeCode <- surveyList %>% 
  group_by(Fee.Code) %>% 
  summarize(total = n())
surveyListSumFeeCode
#Summarize Regulating Agency for SURVEY LIST
surveyListSumReg <- surveyList %>% 
  group_by(Regulating.Agency) %>% 
  summarize(total = n())
surveyListSumReg

#Determine proportion complete by Fee Code for COMPLETED SURVEYS
feeCodeSummary <- completedSurveys %>% 
  group_by(Fee.Code, completed) %>% 
  summarize(num.completed = n()) 
feeCodeSummary
#Determine proportion complete by bin for COMPLETED SURVEYS
tagSummary <- completedSurveys %>% 
  group_by(tag, completed) %>% 
  summarize(num.completed = n()) 
tagSummary
#Determine proportion complete by Regulating Agency for COMPLETED SURVEYS
RegSummary <- completedSurveys %>% 
  group_by(Regulating.Agency, completed) %>% 
  summarize(num.completed = n()) 
RegSummary

#Join summary lists for FEE CODES
feeCodeSummaryJoin<- left_join(feeCodeSummary, surveyListSumFeeCode) %>% 
  mutate(proportion.completed = num.completed / total) %>% 
  select(!completed)
feeCodeSummaryJoin
#Join summary lists for BINS
tagSummaryJoin<- left_join(tagSummary, surveyListSumTag) %>% 
  mutate(proportion.completed = num.completed / total) %>% 
  select(!completed)
tagSummaryJoin
#Join summary list for REGS
RegSummaryJoin<- left_join(RegSummary, surveyListSumReg) %>% 
  mutate(proportion.completed = num.completed / total) %>% 
  select(!completed)
RegSummaryJoin
# Transform Regulating Agency to just Numbers (manually in excel)
#write.csv(RegSummaryJoin, file = "Datasets/RegSummaryJoin.csv") #to manually correct names
RegNames<- read.csv("Datasets/RegSummaryJoin_fixedNames.csv")
RegSummaryJoin <- RegNames %>% 
  select(!Agency2)
#ensure reg number is character
RegSummaryJoin$Reg.num <- as.character(RegSummaryJoin$Reg.num)
RegSummaryJoinDistricts <- RegSummaryJoin %>% 
  filter(D.or.LPA == "D")
RegSummaryJoinLPA <- RegSummaryJoin %>% 
  filter(D.or.LPA == "LPA")

#### Real List Summary Method ####
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

# Determine percent complete by fee code AND District
feeCodeDistrictSummary <- fullList %>% 
  group_by(Fee.Code, completed, DS.Class) %>% 
  filter(!DS.Class == "TD") %>% 
  filter(!DS.Class == "NA") %>% 
  filter(!DS.Class == "NR") %>% 
  summarize(num.completed = n()) %>% 
  mutate(total = sum(num.completed),
         proportion.completed = num.completed / sum(num.completed)) %>% 
  filter(completed == "y") %>% 
  select(!completed)
write.csv(feeCodeDistrictSummary, "plots/Completeness Summary by District and Fee Code.csv")


#### Plots ####
#### plot % completeness by factors ####
# Plot by Regulating Agency: Districts
plot.reg.district<- ggplot(data = RegSummaryJoinDistricts, aes(x = Reg.num, y = proportion.completed, fill = D.or.LPA)) +
  geom_col() +
  coord_cartesian(ylim = c(0.5,2.5))+
  scale_fill_manual(values = cal_palette("superbloom1")) +
  geom_text(aes(label = num.completed),
            vjust = -1)+
  scale_x_discrete(name = "District Number") +
  scale_y_continuous(name = "Completeness",
                     labels = scales::percent) +
  geom_hline(yintercept = 1.0, linetype = 'dashed') +
  theme_minimal()+
  theme(legend.position = "none",
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
plot.reg.district

# Plot by Regulating Agency: LPAs
plot.reg.LPA<- ggplot(data = RegSummaryJoinLPA, aes(x = Reg.num, y = proportion.completed, fill = D.or.LPA)) +
  geom_col() +
  coord_cartesian(ylim = c(0.5,2.5))+
  scale_fill_manual(values = cal_palette("superbloom3")) +
  geom_text(aes(label = num.completed),
            vjust = -1)+
  scale_x_discrete(name = "LPA Number") +
  scale_y_continuous(name = "Completeness",
                     labels = scales::percent) +
  geom_hline(yintercept = 1.0, linetype = 'dashed') +
  labs(title = "LPA Completeness", caption = "11-30-2020")+
  theme_minimal()+
  theme(legend.position = "none",
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
plot.reg.LPA

#arrange together
plotProportionCompleteLPADistrict <- grid.arrange(plot.reg.district, plot.reg.LPA,
                                       ncol = 2,
                                       top = textGrob("Survey Completion By District and LPA", gp=gpar(fontsize = 22, font=6)))
#save
ggsave(path = "plots",
       filename = "proportionCompleteLPADistrict_12-7.png",
       plotProportionCompleteLPADistrict,
       width = 8,
       scale = 2,
       dpi = 500)

#plot by fee code
plot.feeCode <- ggplot(data = feeCodeSummaryJoin, #using summary data
                       aes(x = Fee.Code, y = proportion.completed, fill = Fee.Code)) +
  geom_col() +
  coord_cartesian(ylim = c(0.5,1)) +
  geom_text(aes(label = num.completed),
            vjust = -1)+
  scale_fill_manual(values = cal_palette("superbloom3")) +
  scale_x_discrete(name = "Fee Code", 
                    labels = c("Lrg System", "Disad. Lrg Cmmnty", "Disad. Smll Cmmnty", "Smll Cmmnty"))+
  scale_y_continuous(name = "Completeness",
                     labels = scales::percent) +
  geom_hline(yintercept = 0.8, linetype = 'dashed') +
  theme_minimal() +
  theme(legend.position = "none", #no key,
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 12))
plot.feeCode

# plot by bin
plot.tag <- ggplot(data = tagSummaryJoin, #using summary data 
                   aes(x = tag, y = proportion.completed, fill = tag)) +
  geom_col()+
  scale_fill_manual(values = cal_palette("superbloom2")) +
  coord_cartesian(ylim = c(0.5,1)) +
  geom_text(aes(label = num.completed),
            vjust = -1)+
  scale_x_discrete(name = "Sampling Bin") +
  scale_y_continuous(name = "Completeness",
                     labels = scales::percent) +
  labs(caption = "11-30-2020")+
  geom_hline(yintercept = 0.8, linetype = 'dashed') +
  theme_minimal() +
  theme(legend.position = "none", #no key
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18))
plot.tag
#arrange together
plotProportionComplete <- grid.arrange(plot.feeCode, plot.tag,
             ncol = 2,
             top = textGrob("Survey Completion By Bin and Fee Code", gp=gpar(fontsize = 22, font=6)))
#save
ggsave(path = "plots",
       filename = "proportionComplete_12-7.png",
       plotProportionComplete,
       width = 8,
       scale = 2,
       dpi = 500)

