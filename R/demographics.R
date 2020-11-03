library(tidyverse) 
library(readxl)
dem <- read_excel("Datasets/pop_weighted_characteristics.xlsx", na = c("", "#N/A", "NA"))
dem2 <- na.omit(dem) #omit all rows with NAs

#Count Fee Codes
dem2 %>% 
  group_by(Fee.Code) %>% 
  summarize(n())

## filter data ##
dem3 <- dem2 %>% 
  filter(SVC_CONNEC <10000)# %>% #filter above 2 because there are many
 # filter(Fee.Code !="WH") %>%  #filter out wholesalers 
  #filter(Fee.Code !="N1") %>% #transient non-community water systems
#  filter(Fee.Code !="N2") %>%  #transient non-community water systems (handwash exemption)
 # filter(Fee.Code != "SP") #nonTransient-NonCommunity water system


#re-count groups
dem3 %>% 
  group_by(Fee.Code) %>% 
  summarize(n())
