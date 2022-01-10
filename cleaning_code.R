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



# use reflib_source for preprocessing and eventually
# matching protocol in next scripts 

write.csv(reflib_source,file="reflib_source.csv") 
# -> go to reflib_fig1.R 

