# LIBRARIES----

# these are just my default packages, not all are required, but all are nice to have

library(pacman)
p_load(tidyverse,dplyr,psych,readxl,writexl,ggpubr,
       parameters,factoextra,NbClust,cluster,tidymodels) 

# DIRECTORY----

# use the BIAT Sharepoint

setwd('C:/Users/masotho/OneDrive - ISS/BIAT/1_Product Development/2_Enhancements/2_Ranking Factors/R Files')
#               ^ your id here
getwd()

# DATA IMPORT----

# The only file used here is the BIAT output with the following metrics:
# Entity name (and/or Entity ID although is omitted in this file)
# Industry
# BIATPDF (REFINED SCORE), BIATPDF/EURO
# The read excel file here is the sample found in BIAT sharepoint folder
# Although we do not use MSA in the clustering, I have left them in here.

data <- read_excel('C:/Users/masotho/OneDrive - ISS/BIAT/1_Product Development/2_Enhancements/2_Ranking Factors/R Files/biat_sample.xlsx')

# DATA EXPLORATION----

# box plots, histograms, quartiles,
# identification of outliers should be conducted to identify cut-off points
# for very high categorization
# not including the code for that here

# We identified outlier data beyond the filtered thresholds:
# BIATPDF >156,000
# BIATPDFEURO >98

# Because the range is so massive, datapoints beyond these thresholds are filered
## out. Leaving them in distorts clustering. 
# All data above these values == Very High categorisation by default

data_new <- data %>% 
  select(c(1,2,5,6)) %>% # entity, industry and PDF data
  filter(BIATPDF<156500 & BIATPDFEURO<98) # beyond thresholds excluded

# CLUSTERING----

# NOTE 1: This is an iternative process, proper clusters will not be achieved 
## on the first pass. Probably a few filters of data and re-clusters are needed.

# NOTE2: the clustering defines the most organic thresholds and it is up to us
## to define the final groups based on logical limits

cluster_data <- data_new %>% 
  column_to_rownames(var='EntityName') %>%  # fix row names
  select(BIATPDFEURO) # take only PDFEUR (normalised) data 

# save the model
model <- kmeans(cluster_data, centers = 4, nstart = 10)
# centers = desired number of clusters
## 4 refers to n categories: very low, low, medium & high [very high = excluded outliers]
# n start = repeatability, so model can be repeated and same clusters retained

# store the groups as a column in the dataframe
cluster_data_2 <- data.frame(cluster_data,
                          cluster = as.factor(model$cluster)) %>% 
  arrange(cluster,desc(BIATPDFEURO))

# NOTE: If you check this dataframe you will see the clusters are not exactly in
## order. E.g. cluster 1,2,3,4 are not groups in ascending order.

# check the accuracy / confidence
(BSS <- model$betweenss)
(TSS <- model$totss)
BSS / TSS * 100
fviz_nbclust(cluster_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle
# 4 clusters is idealafter 4 it tapers off and there is little differentiation
## between groups.
## See the documentation for factoextra package for explanations

## FIRST VISUALISATION----
cluster_data_2 %>% 
  ggplot(aes(BIATPDFEURO,BIATPDFEURO,color=cluster))+
  geom_point()+
  # scale_y_log10(oob = scales::squish_infinite)+
  # scale_x_log10(oob = scales::squish_infinite)+
  theme_pubclean()+
  labs(title='Cluster')

## THRESHOLDS & COUNTS----
cluster_data_2 %>% 
  group_by(cluster) %>% 
  summarise(max(BIATPDFEURO), # define upper threshold
            min(BIATPDFEURO)) # define lower threshold

# Here you see the mix up in names and order
# Cluster 2 = Very Low range
# Cluster 1 = Low range
# Cluster 3 = Medium range
# Cluster 4 = High range

cluster_data_2 %>% 
  count(cluster) # n companies per cluster

# Now, in the intial clustering, groups 1 & 2 were too large, and the ranges too 
## wide for it to be meaningful. So, the above cluster code was run again, only 
## data within those lower thresholds to pick out a more refined cluster.

# After 2-3 re-runs of the cluster we have our final segments. Once satisfied 
## with the thresholds, we simply apply the min-max cluster limits to the dataset

# CREATE THE CLASSIFIED DATASET----

# Apply the cluster thresholds to the main dataset to label each issuer

data_classified <- data %>% 
  mutate(PDFEURRating=case_when(
    BIATPDFEURO >98 ~ 'Very high', # all PDFEUR outliers 
    BIATPDFEURO <=98 & BIATPDFEURO >30.91 ~ 'High', 
    BIATPDFEURO <=30.91 & BIATPDFEURO > 12.98 ~ 'Medium',
    BIATPDFEURO <=12.98 & BIATPDFEURO >6.52 ~ 'Low',
    BIATPDFEURO <=6.52 ~ 'Very low',
  ))
data_classified$PDFEURRating<-factor(data_classified$PDFEURRating,
                                   levels = c("Very low","Low","Medium","High","Very high"))

# RE-RATING OUTLIERS----

# A number of extra rules are needed to handle industry dynamics and revenue distortion
## This uses total PDF as the basis:
### making step-wise upgrades where total PDF is high, relative to PDFEUR
### and making step-wise downgrades where total PDF is low, relative to PDFEUR
#### the grades are determined by quartiles (see section: Data exploration)

# VL;L;M;H;VH = very low;low;medium;high;very high

data_classified <- data_classified %>% 
  mutate(PDFEURRating_Final=case_when(
    # criteria for ~INCREASING~ the rating.
    ## this resolves instances where high revenues distort biodiversity impact [PDF] by accounting for issuers Total PDF score.
    ### There is no limit on how high a rating can be reassigned (VL CAN become VH).
    #### however the ratings increase IS restricted by total PDF score (VL can only become VH in extreme instances).
    # quartile bands are rounded as issuers are few.
    BIATPDF >37263 & BIATPDF <156500 & BIATPDFEURO <=6.52 ~ 'Low',      # VL       --> L  IF PDF between 3rd quartile & threshold (normal data range)
    BIATPDF>=156500 & BIATPDF <430000 & BIATPDFEURO <=12.98 ~ 'Medium', # VL+L     --> M  IF PDF between threshold & median (outlier range)
    BIATPDF>=430000 & BIATPDF <1000000 & BIATPDFEURO <=30.91 ~ 'High',  # VL+L+M   --> H  IF PDF between median & 3rd quartile (outlier range)
    BIATPDF>=1000000 ~ 'Very high',                                     # VL+L+M+H --> VH IF PDF above 3rd quartile (outlier range)
    # criteria for ~DECREASING~ the rating.
    ## this resolves overly high scores affecting high revenue+low impact issuers [R&D pharma, service sector e.g. digital payments]. 
    ## similarly, 
    # quartile bands are specific as the bulk of issuers fall into this score zone.
    BIATPDF >10439 & BIATPDF <156500 & BIATPDFEURO >=98 ~ 'High',       # VH       --> H  IF PDF between threshold & median
    BIATPDF >=2080 & BIATPDF <10439 & BIATPDFEURO >30.91 ~ 'Medium',    # H+VH     --> M  IF PDF between 1st-median
    BIATPDF >=10 & BIATPDF<2080 & BIATPDFEURO >12.98 ~ 'Low',           # M+H+VH   --> L  IF PDF below 1st quartile
    BIATPDF<10 & BIATPDFEURO >6.52 ~ 'Very low',                        # L+M+H+VH --> VL IF PDF is exceptionally low
    
    TRUE ~ as.character(PDFEURRating))) # copy everything else from the intial rating
data_classified$PDFEURRating_Final<-factor(data_classified$PDFEURRating_Final,
                                     levels = c("Very low","Low","Medium","High","Very high"))


# Cluster visual----
## Clusters now have some 'over-under' conditions accounting for both PDF/EUR and total PDF skews
data_classified %>% 
  ggplot(aes(BIATPDFEURO,BIATPDF,color=PDFEURRating_Final))+
  geom_point(alpha=.5,position='jitter')+
  scale_y_log10(oob = scales::squish_infinite)+
  scale_x_log10(oob = scales::squish_infinite)+
  theme_pubclean()+
  theme(plot.margin = margin(1,1,1,1,'cm'),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  labs(title='Corporate Biodiversity Impact: Intensity & Absoloute footprints',
       x = 'PDF Intensity',
       y = 'PDF Absoloute')+
  scale_color_manual(name='Impact classification',
                     values=c('Very low'='#9AD4C6',
                              'Low'='#00BD8D',
                              'Medium'='#C0BEBE',
                              'High'='#367296',
                              'Very high'='#96B8CF'))

# FINAL COUNTS----
data_classified %>% 
  count(PDFEURRating_Final)



