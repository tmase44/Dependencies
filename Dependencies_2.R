# MY LIBRARIES----
library(pacman)
p_load(tidyverse,dplyr,psych,readxl,writexl,ggpubr,
       parameters,factoextra,NbClust,cluster,tidymodels,
       ggpmisc,data.table)


# Data import----
## Mapping, dependencies & impacts
data_june <- read_excel("C:/Users/masotho/OneDrive - ISS/My Documents/R/Dependencies/dep_map.xlsx")
## data = dependencies per EXIOBASE activity + weight


## Revenue data
revenue <- read_excel("C:/Users/masotho/OneDrive - ISS/My Documents/R/Dependencies/Revenue_clean.xlsx")

# Factorize character strings----
data_june$ex_id<-factor(data_june$ex_id)
data_june$exiobase_industry<-factor(data_june$exiobase_industry)
data_june$dependency<-factor(data_june$dependency)
data_june$section<-factor(data_june$section)
data_june$materiality<-factor(data_june$materiality,
                         levels=c('VL','L','M','H','VH'))

revenue$ex_id<-factor(revenue$ex_id)
revenue$exiobase_industry<-factor(revenue$exiobase_industry)
revenue$entity_id<-factor(revenue$entity_id)
revenue$entity_name<-factor(revenue$entity_name)
revenue$Sector<-factor(revenue$Sector)

# consoliodate materiality weights
data_june_new <- data_june %>% 
  group_by(ex_id,exiobase_industry,dependency,section) %>% 
  summarise(weight=mean(weight))

data_june_new <- data_june_new %>% 
  mutate(materiality=case_when(
    weight >=4.5 ~ 'VH',
    weight >=3.5 & weight <4.5 ~ 'H',
    weight >=2.5 & weight <3.5 ~ 'M',
    weight >=1.5 & weight <2.5 ~ 'L',
    weight <1.5 ~ 'VL'))


# Merge revenue & dependency/impact data
x<-merge(revenue,data_june_new,by=c('ex_id','exiobase_industry'),all=F)
#y<-merge(revenue,data_june,by=c('ex_id','exiobase_industry'),all=F)

# Calculate values
master_data_d_new<- x %>% 
  group_by(entity_id,ex_id) %>% 
  mutate(dep_units=sum(weight),
         dep_unit_value=revenue_industry/dep_units,
         dep_value=dep_unit_value*weight,
         dep_pc=dep_value/revenue_total) %>% 
  ungroup() %>% # consolidation code below
  group_by(entity_id,entity_name,Sector,revenue_total,dependency,section) %>%
  summarise(weight=mean(weight),
            dep_value=sum(dep_value),
            dep_pc=sum(dep_pc))

master_data_d_new %>% 
  group_by(materiality) %>% 
  summarise(max=max(weight),
            min=min(weight))

master_data_dnew <- master_data_d_new %>% 
  mutate(materiality=case_when(
    weight >=4.5 ~ 'VH',
    weight >=3.5 & weight <4.5 ~ 'H',
    weight >=2.5 & weight <3.5 ~ 'M',
    weight >=1.5 & weight <2.5 ~ 'L',
    weight <1.5 ~ 'VL'))

master_data_d_new <- master_data_d_new %>% 
  relocate(1,2,3,4,5,6,7,10,8,9)

write_xlsx(master_data_d_new,'Dependencies_Impacts_Output_new.xlsx')

write_xlsx(data_june_new,'Dependencies_Map_Mean_Weights.xlsx')

# testing

test <- y %>% 
  filter(entity_id=='1538')
view(test)

test<-test %>% 
  group_by(entity_id,dependency) %>% 
  mutate(wt_new=weight*revenue_percent)
