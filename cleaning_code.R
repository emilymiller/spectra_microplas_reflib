# read in CSVs
getwd()
setwd("C:/Users/emily/Documents/MBARI/spectra")
reflib214<-read.csv("./S&N Labs results/MBA Raman Project Shipment #2/MBA Reference Library (White Box)/PLAS 214 532 nm.csv",
                    skip=1)
head(reflib214)
plot(reflib214$INT~reflib214$Raman.Shift...cm.1)                  
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

ggplot(df, aes(x=raman_shift, y=INT,
               color=wave_number)) + 
  geom_line()+ facet_wrap(.~sample_id,scales="free")+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=10))

#* start by doing separate paneled plot for 532 and 780
#(so that the axes are dif)

#* then do same for the sediment data

#* go through tyler's code

#* how to combine the two wave numbers per sample
