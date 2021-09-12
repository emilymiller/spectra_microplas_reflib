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

# single page of all plots by sample (both wave numbers
# for a given sample are on one plot)
ggplot(df3, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))

write.csv(df3,file="reflib_combined_df.csv")

# reorder by name

# separate those with fluorecense

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
                                   page = 5)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

reflib<-df3 # has all three batches

dev.off()


##########################################
#
#* then do same for the sediment data
#
##########################################


##########################################

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
df$`Raman Shift / cm-1`<-NULL

dev.off()

# single page of all plots by sample (both wave numbers
# for a given sample are on one plot)
ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))

# needs redone for sample ids with duplicate numbers (# 050)

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
                                   page = 1)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

micropart<-df

#dev.off()


######################################

# formatting dataframes reflib and micropart

micropart<-df
library(stringr)
reflib$sample_id<-str_replace_all(reflib$sample_id, fixed(" "), "")
head(reflib)
unique(micropart$sample_id)

micropart$sample_id<-ifelse(micropart$sample_id=="0-4","050-4",micropart$sample_id)
micropart$sample_id<-ifelse(micropart$sample_id=="0-1","050-1",micropart$sample_id)
micropart$sample_id<-ifelse(micropart$sample_id=="-10","050-10",micropart$sample_id)
micropart$sample_id<-ifelse(micropart$sample_id=="0-2","050-2",micropart$sample_id)
micropart$sample_id<-ifelse(micropart$sample_id=="0-3","050-3",micropart$sample_id)
micropart$sample_id<-ifelse(micropart$sample_id=="0-5","050-5",micropart$sample_id)
micropart$sample_id<-ifelse(micropart$sample_id=="0-9","050-9",micropart$sample_id)

micropart$sample_id <- paste("mp", micropart$sample_id, sep="")


# # #
df<-micropart

# single page of all plots by sample (both wave numbers
# for a given sample are on one plot)
ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))

# redone for sample ids with duplicate numbers (# 050)

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
                                   page = 1)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))




# # # 




# add a source column and material column by merging with database
reflib_meta<-read.csv("polymer_specimen_library_for_spectroscopy.csv")
micropart_part_meta<-read.csv("sediment_microplastic_particles.csv")
micropart_samp_meta<-read.csv("sediment_sample_metadata.csv")

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

# add metadata to particles dataframe
head(micropart)
unique(micropart$sample_id)
class(micropart$sample_id)
head(micropart_part_meta)

# pull out the slide50 particles and remove from 
# metadata to add back in later
unique(micropart_part_meta$circled_slide_no)
air_blank = subset(micropart_part_meta, circled_slide_no == "50-1"|
                   circled_slide_no == "50-2"|
                     circled_slide_no == "50-3"|
                     circled_slide_no == "50-4"|
                     circled_slide_no == "50-5"|
                     circled_slide_no == "50-6"|
                     circled_slide_no == "50-7"|
                     circled_slide_no == "50-8"|
                     circled_slide_no == "50-9"|
                     circled_slide_no == "50-10")

head(air_blank)

micropart_part_meta2 = subset(micropart_part_meta, circled_slide_no != "50-1"&
                     circled_slide_no != "50-2"&
                     circled_slide_no != "50-3"&
                     circled_slide_no != "50-4"&
                     circled_slide_no != "50-5"&
                     circled_slide_no != "50-6"&
                     circled_slide_no != "50-7"&
                     circled_slide_no != "50-8"&
                     circled_slide_no != "50-9"&
                     circled_slide_no != "50-10")
# 
head(micropart_part_meta2)

micropart_part_meta2$sample_id2<- 
  formatC(micropart_part_meta2$circled_slide_no, width = 3, 
        format = "d", flag = "0")
colnames(micropart_part_meta2)[2]<-"slide_id"
colnames(micropart_part_meta2)[15]<-"sample_id"
micropart_part_meta2$sample_id <- paste("mp", micropart_part_meta2$sample_id, sep="")
head(micropart_part_meta2)

# merge in particle metadata
head(air_blank)
colnames(air_blank)[2]<-"slide_id"
air_blank$sample_id <- paste("mp0", air_blank$circled_slide_no, sep="")

# bring air blanks back in to main file
micropart_part_meta3<-rbind(micropart_part_meta2,air_blank)
head(micropart_part_meta3)
rm(micropart_part_meta)
rm(micropart_part_meta2)
rm(air_blank)

# merge micropart_particles with micropart_sample_metadata, keeping
# all particle rows and dropping sample data that didn't produce any particles
names(micropart_part_meta3)
head(micropart_part_meta3)
unique(micropart_part_meta3$slide_id)
names(micropart_samp_meta)
unique(micropart_samp_meta$label_supernatant_filter)
# in micropart_samp_meta$label_supernatant_filter, drop last three 
# characters if they are "_sn" 
micropart_samp_meta$label_supernatant_filter <- 
  as.character(micropart_samp_meta$label_supernatant_filter)
head(micropart_samp_meta)
library(stringr)
micropart_samp_meta <- micropart_samp_meta %>%
  mutate_at("label_supernatant_filter", str_replace, "_sn", "")
unique(micropart_samp_meta$label_supernatant_filter)
micropart_samp_meta$slide_id<-micropart_samp_meta$label_supernatant_filter
unique(micropart_samp_meta$slide_id)

unique(micropart_part_meta3$slide_id)

micropart_part_meta3 <- micropart_part_meta3 %>%
  mutate_at("slide_id", str_replace, "_sn", "")
micropart_part_meta3 <- micropart_part_meta3 %>%
  mutate_at("slide_id", str_replace, "_ws", "")
unique(micropart_part_meta3$slide_id)

micropart_allmeta<- merge(x = micropart_part_meta3, y = micropart_samp_meta,
                         by="slide_id",all.x=T)
sort(unique(micropart_allmeta$sample_id.x))
sort(unique(micropart_allmeta$sample_id.y))
micropart_allmeta$sample_id<-micropart_allmeta$sample_id.x

micropart_source<-merge(x = micropart, y = micropart_allmeta, by = "sample_id", all.x = TRUE)
head(micropart_source)
sum(is.na(micropart_source$INT))
unique(micropart_source$sample_id) # 48 out of 59
unique(micropart_source$site)

rm(micropart_allmeta)
rm(micropart_part_meta3)
rm(micropart_samp_meta)
rm(micropart)
rm(df)
head(micropart_source$sample_id)
##########################################

unique(micropart_source$site) # site location
particles_meta <- micropart_source[!duplicated(micropart_source$sample_id), ]

# use micropart_source and reflib_source to match in 
# next script
write.csv(micropart_source,file="micropart_source.csv")
write.csv(reflib_source,file="reflib_source.csv") # -> go to reflib_fig1.R 


#* how to combine the two wave numbers per sample
# or just choose the 780 if both present
# rescale 780 and 532 to be same scale

# pearson's distance
