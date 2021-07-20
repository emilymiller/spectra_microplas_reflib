# read in CSVs
getwd()
setwd("C:/Users/emily/Documents/MBARI/spectra")
reflib214<-read.csv("./S&N Labs results/MBA Raman Project Shipment #2/MBA Reference Library (White Box)/PLAS 214 532 nm.csv",
                    skip=1)
head(reflib214)
plot(reflib214$INT~reflib214$Raman.Shift...cm.1)   

themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

ggplot(reflib214)+
  geom_line(aes(x=Raman.Shift...cm.1,y=INT))+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo

library(tidyverse)
library(stringr)
library(ggplot2)

list_of_files <- list.files(path = "./S&N Labs results/MBA Raman Project Shipment #2/MBA Reference Library (White Box)",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df<-list_of_files %>% set_names() %>%
  map_df(read_csv,skip=1,.id="file_name")
df<-as.data.frame(df)
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

# single page of all plots by sample (both wave numbers
# for a given sample are on one plot)
ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

### Split into multiple pages for size and separated
### samples that were run at both wave numbers (532 & 780nm) 
### into two plots. Axes are different.

#install.packages("ggforce")
library(ggforce)

# Pagination: page 1 example
df$wave_number<-as.factor(df$wave_number)
ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap_paginate(sample_id~wave_number,scales="free",
                                   ncol = 4, nrow = 4,
                                   page = 3)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

reflib<-df
#dev.off()


##########################################
#
#* then do same for the sediment data
#
##########################################



setwd("C:/Users/emily/Documents/MBARI/spectra")

list_of_files <- list.files(path = "./S&N Labs results/MBA Raman Project Shipment #2/MBA Microparticles (Red Box)",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
df<-list_of_files %>% set_names() %>%
  map_df(read_csv,skip=1,.id="file_name")
df<-as.data.frame(df)
head(df)

df$wave_number1 = str_sub(df$file_name,-14)
df$wave_number <- str_sub(df$wave_number1,1,7)
df$wave_number1 <- NULL
df$wave_number <- str_sub(df$wave_number,-3)

df$sample_id1 <- str_sub(df$file_name,-14)
df$sample_id <-str_sub(df$sample_id1,1,3)
df$sample_id1<-NULL

df$file_name <-NULL

head(df)

df$raman_shift<-df[,1]

dev.off()

# single page of all plots by sample (both wave numbers
# for a given sample are on one plot)
ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))

# needs redo for sample ids with duplicate numbers (# 050)

### Split into multiple pages for size and separated
### samples that were run at both wave numbers (532 & 780nm) 
### into two plots. Axes are different.

#install.packages("ggforce")
library(ggforce)

# Pagination: page 1 example
df$wave_number<-as.factor(df$wave_number)
ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap_paginate(sample_id~wave_number,scales="free",
                                   ncol = 4, nrow = 4,
                                   page = 4)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

micropart<-df

#dev.off()





#* go through tyler's code

# add a source column and material column or merge with database


#* how to combine the two wave numbers per sample

# pearson's distance
