# MY LIBRARIES----
library(pacman)
p_load(tidyverse,dplyr,psych,readxl,writexl,ggpubr,
       parameters,factoextra,NbClust,cluster,tidymodels,
       ggpmisc,data.table)


# Data import----
## Mapping, dependencies & impacts
data <- read_excel("C:/Users/masotho/OneDrive - ISS/My Documents/R/Dependencies/Exiobase_GICS_Mapping_R.xlsx")
## data = reference lit for all dependencies & impacts per EXIOBASE activity
## AND the weights for each combination

## Revenue data
revenue <- read_excel("C:/Users/masotho/OneDrive - ISS/My Documents/R/Dependencies/Revenue_clean.xlsx")

## Natural Capital Assets (NCA) data import----
nca <- read_excel("C:/Users/masotho/OneDrive - ISS/My Documents/R/Dependencies/Natural_capital_asset_categories.xlsx")

# Factorize character strings----
data$ex_id<-factor(data$ex_id)
data$exiobase_industry<-factor(data$exiobase_industry)
data$gics_sub<-factor(data$gics_sub)
data$type<-factor(data$type)
data$d_i<-factor(data$d_i)
data$materiality<-factor(data$materiality,
                                 levels=c('VL','L','M','H','VH'))

revenue$ex_id<-factor(revenue$ex_id)
revenue$exiobase_industry<-factor(revenue$exiobase_industry)
revenue$entity_id<-factor(revenue$entity_id)
revenue$entity_name<-factor(revenue$entity_name)
revenue$Sector<-factor(revenue$Sector)


#==============================================================
#Calculating scores: 21 dependencies + 11 impact categories----

# Merge revenue & dependency/impact data
x<-merge(revenue,data,by=c('ex_id','exiobase_industry'),all=F)

# Calculate values
master_data_d_i<- x %>% 
  group_by(entity_id,ex_id,type) %>% 
  mutate(d_i_units=sum(weight),
         d_i_unit_value=revenue_industry/d_i_units,
         d_i_value=d_i_unit_value*weight,
         d_i_pc=d_i_value/revenue_total) %>%  
  ungroup() %>% 
  group_by(entity_id,ex_id,type,materiality) %>% 
    mutate(count=n(),
           intensity=sum(weight))

master_data_d_i <- master_data_d_i %>% 
  relocate(1,2,9,3,4,5,6,7,8,10,11,12,13,18,19,14,15,16,17)

write_xlsx(master_data_d_i,'Dependencies_Impacts_Output.xlsx')


#===============================================================
# Calculating now + NCA data----

# Merge NCA with dependency_impact data
master_dependencies <- merge(data,nca, by=c('type','d_i'),all=F)
## master_dependencies = the reference list for:
## the dependencies and impacts for every EXIOBASE activity,
## AND the weights of each combination
## AND the overarching NCA categories of those dependencies & impacts

# MERGE revenue & master_dependencies
master_data_d_i_nca <- merge(revenue,master_dependencies,by=c("ex_id",'exiobase_industry'),all=T)

## rearrange column order for readability
master_data_d_i_nca <- master_data_d_i_nca %>% 
  relocate(3,4,5,11,6,7,8,1,2,9,10,12,13,14,15,16)

# Calculations for weights and values
master_data_d_i_nca <- master_data_d_i_nca %>% 
  group_by(entity_id,ex_id,type) %>% 
  mutate(d_i_units=sum(weight),
         d_i_unit_value=revenue_industry/d_i_units,
         d_i_value=d_i_unit_value*weight,
         d_i_pc=d_i_value/revenue_total,
         nca_units=sum(nca_weight),
         nca_unit_value=revenue_industry/nca_units,
         nca_value=nca_unit_value*nca_weight,
         nca_pc=nca_value/revenue_total)

# EXPORT SAMPLE
master_sample<-master_data_d_i_nca %>% 
  filter(entity_id==7614)

write_xlsx(master_sample,'Dependencies_Impacts_NCA_Sample.xlsx')




