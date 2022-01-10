#' ---
#' #' title: Cleaning code for Raman spectra
#' author: Emily Miller
#' date: 2021
#' output: csvs that are ready for preprocessing script
#' order: This is the first script use to prepare raw
#' spectra from the lab. This script prepares the data 
#' for the preprocessing script and for constructing figures
#' for the Miller et al. manuscript.
#' ---



# read in CSVs
getwd()
setwd("C:/Users/emily/Documents/MBARI/microplastics/spectra_microplas_reflib")
reflib214<-read.csv("./S&N Labs results/MBA Raman Project Shipment #2/MBA Reference Library (White Box)/PLAS 214 532 nm.csv",
                    skip=1)
head(reflib214)
plot(reflib214$INT~reflib214$Raman.Shift...cm.1)   

library(ggplot2)
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

# visualizing with one spectra
ggplot(reflib214)+
  geom_line(aes(x=Raman.Shift...cm.1,y=INT))+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo

library(tidyverse)
library(stringr)


list_of_files <- list.files(path = "./S&N Labs results/MBA Raman Project Shipment #2/MBA Reference Library (White Box)",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df<-list_of_files %>% set_names() %>%
  map_df(read_csv,skip=1,.id="file_name")
df<-as.data.frame(df)
head(df)
df$wave_number1 = str_sub(df$file_name,-10)
df$wave_number <- str_sub(df$wave_number1,1,3)
df$wave_number1 <- NULL
df$sample_id1 <- str_sub(df$file_name,-19)
df$sample_id <-str_sub(df$sample_id1,1,8)
df$file_name <-NULL
df$sample_id1<-NULL
head(df)

df$raman_shift<-df[,1]

dev.off()

############################################################################
# now incorporate shipment one data
############################################################################


list_of_files <- list.files(path = "./S&N Labs results/24364 Raw Spectra and CSV Files",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df2<-list_of_files %>% set_names() %>%
  map_df(read_csv,skip=1,.id="file_name")

df2<-as.data.frame(df2)
head(df2)
names(df2)
unique(df2$ArbY)
df2$ArbY<-NULL

# all files from shipment one were using 532 nm wavelength
df2$wave_number <- 532
df2$sample_id1 <- str_sub(df2$file_name,-19)
df2$sample_id <-str_sub(df2$sample_id1,1,9)
df2$file_name <-NULL
df2$sample_id1<-NULL
head(df2)
#df2$sample_id<-gsub(" ","",as.character(df2$sample_id))
head(df2)

df2$raman_shift<-df2[,1]
df2$`Raman Shift / cm-1`<-NULL
head(df2)
dev.off()


###########################################################################
#
# add in the weathered plastics (shipment #5)
#
###########################################################################


list_of_files <- list.files(path = "./S&N Labs results/S&N Labs JN 25305 (Shipment #5)",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df5<-list_of_files %>% set_names() %>%
  map_df(read_csv,skip=1,.id="file_name")
df5<-as.data.frame(df5)
head(df5)
df5$wave_number1 = str_sub(df5$file_name,-12)
df5$wave_number <- str_sub(df5$wave_number1,1,3)
df5$wave_number1 <- NULL
df5$sample_id1 <- str_sub(df5$file_name,-21)
df5$sample_id <-str_sub(df5$sample_id1,1,8)
df5$file_name <-NULL
df5$sample_id1<-NULL
head(df5)

df5$raman_shift<-df5[,1]


#
############################################################################
# combine reflib batches

head(df)
df$`Raman Shift / cm-1`<-NULL
head(df2)
head(df5)
df5$`Raman Shift / cm-1`<-NULL
df3<-rbind(df,df2,df5)

############################################################################

# visualizing for exploration

# single page of all plots by sample (both wave numbers
# for a given sample are on one plot)
ggplot(df3, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))

write.csv(df3,file="reflib_combined_df.csv")


### Split into multiple pages for size and separated
### samples that were run at both wave numbers (532 & 780nm) 
### into two plots. Axes are different.

#install.packages("ggforce")
library(ggforce)

# Pagination: page 1 example
df3$wave_number<-as.factor(df3$wave_number)
ggplot(df3, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap_paginate(sample_id~wave_number,scales="free",
                                   ncol = 4, nrow = 4,
                                   page = 1)+ # change page
                      # number here to see all pages
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

reflib<-df3 # has all three batches

dev.off()


######################################

# formatting dataframes reflib


library(stringr)
reflib$sample_id<-str_replace_all(reflib$sample_id, fixed(" "), "")
head(reflib)


# add a source column and material column by merging with database
reflib_meta<-read.csv("polymer_specimen_library_for_spectroscopy.csv")

head(reflib_meta)
names(reflib_meta)
reflib_meta<-reflib_meta[,1:22]

colnames(reflib_meta)[3]<-"sample_id"
reflib_source<-merge(x = reflib, y = reflib_meta, by = "sample_id", all.x = TRUE)
head(reflib_source)
sum(is.na(reflib_source$INT))
rm(reflib_meta)
rm(reflib)
rm(reflib214)


write.csv(reflib_source,file="reflib_source.csv") 


############################################

# add category column
# biological polymers, pristine plastic polymers,
# wild plastic polymers

# make only one row per sample_id
category_counts <- reflib_source[!duplicated(reflib_source$sample_id),]
unique(category_counts$parent_grp)

# biological then biological

# pristine plastic =
# industrial
# bioplastic
# consumer (+ new in estimated age)
# consumer (+ post purchase new in estimated age)
# building material 
# boating
# container (new)
# electronics
# adhesive
# fiber
# upholstery

# wild plastic = 
# fishery
# agricultural
# beachcast
# consumer (+used)
# consumer (+used kids toy)
# container (used)
# oml

category_counts$category<-ifelse(category_counts$parent_grp=="biological","biological",
                          ifelse(category_counts$parent_grp=="fishery","wild_plastic",
                          ifelse(category_counts$parent_grp=="consumer" & category_counts$estimated_age=="used","wild_plastic",
                          ifelse(category_counts$parent_grp=="consumer" & category_counts$estimated_age=="used kids toys","wild_plastic",
                          ifelse(category_counts$parent_grp=="container" & category_counts$estimated_age=="used","wild_plastic",
                         ifelse(category_counts$parent_grp=="agricultural", "wild_plastic",
                      ifelse(category_counts$parent_grp=="beachcast", "wild_plastic",
                      ifelse(category_counts$parent_grp=="oml", "wild_plastic", "pristine_plastic"))))))))

head(category_counts)
library(plyr)
head(category_counts)
category_counts$gen_poly
category_counts$gen_poly<-as.character(category_counts$gen_poly)
# 

# rename biological polymers by tissue type
# chitin
# cellulose
# dentin
# calcium carbonate
# keratin
# bone
class(category_counts$gen_poly)
category_counts$gen_poly
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Zostera eelgrass blade","cellulose",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "tiger shrimp carapace","chitin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "surf clam shell","calcium carbonate",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "squid beaks, albatross stomach","chitin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "sea otter tooth","dentin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "rhino auklet upper mandible horn","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Pacific purple urchin spines; Strongylocentrotus purpuratus; from otter female 613","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "olive ridley sea turtle scapula condyle","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "dungeness crab carapace","chitin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Macrocystis kelp blade","algin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "sea otter maxilla","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Laysan albatross feather","keratin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "market squid bell","myofibrillar protein",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "green mussel shell","calcium carbonate",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Chinook salmon muscle","myofibrillar protein",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Chinook salmon skin","collagen",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "acrylic","PMMA",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Chinook salmon skin","collagen",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "blue cotton towel","cellulose",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
                                   "Choy et al 2019 gear #8","cellulose",category_counts$gen_poly)

category_counts$category<-as.factor(category_counts$category)

# create column that is a combo of polymer-category
# then pull unique

category_counts$polymer_category <- paste(category_counts$gen_poly,
                                          category_counts$category,
                                          sep="-")
head(category_counts)
sort(unique(category_counts$polymer_category))


head(reflib_source)
names(reflib_source)
names(category_counts)
head(category_counts) # want to add gen_poly and category back into 
# reflib_source
colnames(category_counts)[14]<-"polymer_material"
library(dplyr)
category_counts2<-category_counts %>%
  select(sample_id, polymer_material, category)
head(category_counts2)
head(reflib_comb)
reflib_categories<-merge(reflib_comb,category_counts2,by="sample_id",
                         all.x=T)
head(reflib_categories)
unique(reflib_categories$polymer_material)
unique(reflib_categories$category)

######################################################
#
#cleaning
#
######################################################

rm(df3)
rm(data)
rm(df2)
rm(histo)
rm(reflib_comb)
rm(reflib_source)
rm(category_counts2)

######################################################
#
# correcting wave number in Shipment #1 specimens
# (they were labeled in original pdf S&N sent, not in 
# data files)
#
#####################################################

class(reflib_categories$wave_number)
reflib_categories$wave_number<-as.character(reflib_categories$wave_number)

reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS157","785",reflib_categories$wave_number)
head(reflib_categories)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS115","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS117","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS163","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS032","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS175","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "BIOL006","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "BIOL013","785",reflib_categories$wave_number)

head(reflib_categories)


#######################################################

write.csv(reflib_categories,file="reflib_categories.csv")


###################################################

###################################################

## # use reflib_categories for preprocessing and eventually
# matching protocol in next scripts
# -> go to preprocessing to continue with data analyses

#-> or can also go to reflib_fig1.R for paper fig construction

#######################################################