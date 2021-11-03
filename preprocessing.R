
#' ---
#' #' title: Preprocessing Raman spectra
#' author: Emily Miller, based on Tyler Gagne's processing
#' date: Summer 2021
#' output: github_document
#' ---

# In this script a number of preprocessing steps are executed
# First, we read in the raw Wave/Intensity spectra values. These
# values reflect the observed intensity at varied Raman shift wavenumbers.
# S&N had to run spectra scans for some specimens at two different wavenumbers 
# (532 and 785 nm) when the 532 nm did not produce clear spectra.

# Basic protocol:
# Read in raw spectra from S&N (pristine plastics, wild plastics (fishing gear,
# consumer, beachcast marine, beachcast estuarine, biological).

# Run a 15 Wavenumber wide median filter window through the spectra to pull out 
# noise/cosmic ray as best we can
# Fit a 7th order polynomial baseline and calculate residuals as mean to remove
# fluorescence
# Conduct basic SVN/min-max standardization
# Calculate the product moment correlation (Pearson's R) of the standardized
# unlabeled specimens relative to all available training plastic. Essentially, 
# how closely does the unlabeled specimen covary with the known plastic spectra.
# I also explored cosine similarity, dot product similarity, and RMSE, other common
# Raman 'Hit Quality Index' (HQI) methods.
# We could include those for comparison, there is some variance between the 
# assignments.
# I get the impression they do not excel at peak location matching in the way
# the Pearson's product moment correlation does, this is based on my own observation
# of the spectra assignments. 
# Calculate the most closely correlated plastic to the unlabeled specimen.

setwd("C:/Users/emily/Documents/MBARI/microplastics/spectra_microplas_reflib")
getwd()

# Load libraries 
library(ggplot2)
library(Matrix)
library(reshape2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(prospectr)
library(matrixStats)
library(gridExtra)

##############################
###  Popular ggPlot theme  ###
##############################
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

# read in CSVs
# for console compile

reflib_categories<-read.csv("reflib_categories.csv")
names(reflib_categories)
reflib_categories$X.1<-NULL
reflib_categories$X<-NULL
colnames(reflib_categories)[1]<- "ID"
colnames(reflib_categories)[25]<- "SAMPLE" # 26
colnames(reflib_categories)[2]<- "INTENSITY"
colnames(reflib_categories)[4]<- "WAVE"
colnames(reflib_categories)[26]<- "SOURCE" #27
reflib_categories$PARTICLE <- reflib_categories$ID
head(reflib_categories)
##########################################################
#
# if both 785 and 532 runs present, go with 785
# 
##########################################################
library(dplyr)
reflib_categories$wave_number<-as.numeric(as.character(reflib_categories$wave_number))
reflib_simple <- reflib_categories %>% 
  dplyr::group_by(ID) %>% 
  dplyr::filter(wave_number==max(wave_number))

# select just those columns

library(dplyr)
reflib <- dplyr::select(reflib_simple, ID, SAMPLE, INTENSITY, WAVE, PARTICLE, SOURCE)
head(reflib)
reflib<-as.data.frame(reflib)
head(reflib)

reflib$INTENSITY <- reflib$INTENSITY + 10000    # deal with negatives
#######################################################
#
#
######################################################
# preprocessing function
prePro_sh3et <- function(full_set) {         
  
  full_set$ID <- as.factor(paste(full_set$SAMPLE,full_set$PARTICLE))   # build unique ID variable from SAMPLE and PARTICLE
  full_set <- subset(full_set, INTENSITY > 0)                          # subset out rows with INTENSITY values less than 0
  full_set$INTENSITY <- round(full_set$INTENSITY, digits = 0)          # round values to the nearest 1
  full_set$WAVE <- round(full_set$WAVE, digits = 0)
  
  fact_vars <- c("SAMPLE","PARTICLE","ID")                     # string of varnames that should be factors
  # convert those columns to factors
  for( i in 1:length(fact_vars)){                         
    full_set[,fact_vars[i]] <- as.factor(full_set[,fact_vars[i]])}
  
  min_spec <- 200 #min(full_set$WAVE)                                              # these are the tails of the 1520 band 
  max_spec <- 3400 # max(full_set$WAVE)
  full_set <- subset(full_set, WAVE >= 200 & WAVE <= 3400 ) #min(full_set$WAVE) & WAVE <= max(full_set$WAVE))
  
  freq_index <- seq(min_spec,max_spec, by = 1)                                      # establish a full band of which we want readings of
  
  IDs <- levels(droplevels(full_set$ID))                                            # string of all IDs
  foundation <- NULL                                                                # blank DF to fill
  
  # run through all IDs 
  for( i in 1:length(IDs)) {
    
    # subset out one ID
    sub_set <- subset(full_set, ID == IDs[i])
    sub_set <- droplevels(sub_set)
    
    # approximate the values of INTENSITY given the WAVE value to approximate the sequence supplied
    f <- approxfun(sub_set$WAVE,sub_set$INTENSITY)
    approxed <- f(freq_index)
    
    # build a DF
    block <- data.frame(
      ID        = IDs[i]
      ,SAMPLE    = sub_set$SAMPLE[1]
      ,INTENSITY = approxed
      ,WAVE      = freq_index
      ,PARTICLE  = sub_set$PARTICLE[1] )
    
    # check it
    str(block)
    
    # build in to full DF
    foundation <- rbind(block,foundation)
    
    # plot it to look, black showing up is not being 
    # approximated sufficiently
    plot(sub_set$WAVE,sub_set$INTENSITY, type = "l")
    lines(freq_index,approxed, col = "red")
    
  }
  
  str(foundation) 
  
  #ggplot(foundation,aes(WAVE,INTENSITY,color = ID))+
  # geom_line(show.legend = F)
 #  
 #  #remove IDs that do not cover the full defined band, 
 #  # limits defined above, this is a double check
 #   foundation$rows_w_na <- complete.cases(foundation)                    # what rows have NA
 #   shortys <- levels(droplevels(subset(foundation, rows_w_na == F)$ID))  # what are the IDs of those rows
 #   foundation$rows_w_na <- NULL
 #   
 #   if(length(shortys) == 0) { foundation <- foundation } else {
 #     
 # #    # go through and remove the IDs
 #     for(x in 1:length(shortys)){                          
 #       foundation <- foundation[!foundation$ID == shortys[x],]
 #     }
 # #    
 #     foundation <- droplevels(foundation)
 # #    
 #  }
  
  #if(!is.null(dev.list())) dev.off()
 
  return(foundation) }

##### Preprocess the individual datasets
reflib_processed <- prePro_sh3et(full_set = reflib)
head(reflib_processed)
sum(is.na(reflib_processed$INTENSITY)==T)#0
#7251
range(reflib_processed$INTENSITY) # 9500.5 to 56119.0
#reflib_proc_na<-reflib_processed[is.na(reflib_processed$INTENSITY)==T,]
#sort(unique(reflib_proc_na$WAVE))# <=200 and <=3400
######################################################
#
# quick look
colourCount =length(unique(reflib_processed$ID))

ggplot(reflib_processed, aes(WAVE,INTENSITY, color = ID)) +
  geom_line(show.legend = F, alpha = 1) +
  scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(limits = c(0,10000)) +
  scale_color_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(colourCount)  )+
  themeo+
  facet_wrap(~SAMPLE, scales = "free_y")+
  theme(axis.text.y = element_blank())
dev.off()
head(reflib_processed)

sum(is.na(reflib_processed$INTENSITY)==T)#0
#7752

#### Rolling median spike removal and polynomial baseline flouresence correction
# the plot for each spectra shows the polynomial in red, the raw data in purple, and the rolled median output in black
poly_below_baseline <- function(foundation) {
  
  base_df <- NULL
  IDs <- levels(foundation$ID)                       # string of all IDs
  library(fda)
  library(hyperSpec)
  library(pspline)
  library(signal)
  
  min_spec <- min(foundation$WAVE)                                  # these are the tails of the 1520 band, usually...
  max_spec <- max(foundation$WAVE)
  
  freq_index <- seq(min_spec,max_spec, by = 1)    # establish a full band of which we want readings of
  
  
  for( i in 1:length(IDs)) {
    
    spec1 <- subset(foundation, ID == IDs[i])                # build a single dataframe by WB
    
    mod <- list( y = runmed(x = spec1$INTENSITY, k = 15))   #running median filter # D
    spc <- (as.matrix(t(data.frame(mod$y))))  # D
    
    spc <- new("hyperSpec", spc = spc, wavelength = freq_index )
    spc_poly <- spc.fit.poly.below(fit.to = spc, poly.order = 7) # attempted 4 earlier, 9 is based on Li et al. 2016, 5th suggested Lieber & Mahadevan-Jansen in 2003
    
    plot(x = spec1$WAVE, y = spec1$INTENSITY, col = "purple", type = "l")
    lines(x = spec1$WAVE, y = spc$spc, type = "l")
    lines(x = spec1$WAVE, y = spc_poly@data $spc ,col = "red")
    
    resid <- mod$y - (spc_poly@data $spc)                # calculate residuals  for smooth.spline() and rolling median

    spec1$INTENSITY <- as.vector(resid)
    
    base_df <- rbind(base_df, spec1)
    
  }
  # plot clear
  if(!is.null(dev.list())) dev.off()
  return(base_df)
}

#par(mfrow = c(5,5))
base_df <- poly_below_baseline(foundation = reflib_processed)
str(base_df)

sum(is.na(base_df$INTENSITY)==T)#0 #7242
class(base_df$INTENSITY)
base_df_no_na<-base_df[is.na(base_df$INTENSITY)==F,]
sum(is.na(base_df_no_na$INTENSITY)==T)
range(base_df_no_na$INTENSITY) # -119.3321 21894.1809 # -119 to 21894
range(reflib$INTENSITY) # 9489 to 59707
base_df2<-base_df[base_df$INTENSITY>=200 & base_df$INTENSITY <=3500,]


# normalization and scaling
# scale and center by ID 
# run SVN 
IDs <- levels(droplevels(base_df$ID))
IDs

base_df_na <- base_df[is.na(base_df$INTENSITY==TRUE),]
#7242
unique(base_df_na$ID) # 68 specimens have na values
base_df_new <- NULL

for(y in 1:length(IDs)){
  svn_df <- subset(base_df, ID == IDs[y])                                           # grab the specimen
  plot(svn_df$WAVE, svn_df$INTENSITY, type = "l", yaxt = 'n', ylim = c(-10,5000))     # plot it to look
  svn_df$INTENSITY <- prospectr::detrend(X = svn_df$INTENSITY, wav = svn_df$WAVE)   # conduct SVN normalization
  svn_df$INTENSITY <- {(svn_df$INTENSITY-min(svn_df$INTENSITY))/(max(svn_df$INTENSITY)-min(svn_df$INTENSITY))} # range 0-1
  par(new = T)
  plot(svn_df$WAVE,svn_df$INTENSITY, type = "l", col = "red", ylim = c(-2,5))       # plot the SVN transformation
  base_df_new <- rbind(svn_df,base_df_new)
  if(!is.null(dev.list())) dev.off()
}
head(base_df_new)

base_df_new_na <- base_df_new[is.na(base_df_new$INTENSITY==TRUE),]
# 0 #7242
unique(base_df_new_na$ID) #79 #68
sum(is.na(base_df_new_na$INTENSITY)==TRUE)  # 0 # nearly all



base_df <- base_df_new

# 0 - 1 scaling, min-max or std dev 1, mean = 0 scaling...
IDs <- levels(droplevels(base_df$ID))
base_df_new <- NULL
for(y in 1:length(IDs)){
  svn_df <- subset(base_df, ID == IDs[y])
  svn_df$INTENSITY <- (svn_df$INTENSITY-min(svn_df$INTENSITY))/(max(svn_df$INTENSITY)-min(svn_df$INTENSITY))
  plot(svn_df$WAVE,svn_df$INTENSITY, type = "l", ylim = c(-2,6))
  base_df_new <- rbind(svn_df,base_df_new)
  if(!is.null(dev.list())) dev.off()
}
base_df <- base_df_new

# add labels to plastic references
poly <- c(unique(reflib_processed$SAMPLE))
base_df$poly_lab <- "."

for( i in 1:length(poly)){
  base_df$poly_lab <- ifelse(base_df$SAMPLE == poly[i], poly[i], base_df$poly_lab)
}

str(base_df)

# intercept loop for vis with differing intercepts to reduce overlap
#IDs <- levels(droplevels(base_df$ID))
#base_df_new <- NULL
#for(z in 1:length(IDs)){
#  svn_df <- subset(base_df, ID == IDs[z])
#  svn_df$INTENSITY <- svn_df$INTENSITY + z + 5
#  base_df_new <- rbind(svn_df,base_df_new)
#}
#base_df <- base_df_new
#str(base_df)

head(base_df)
base_df$poly_lab <-base_df$SAMPLE

ggplot(base_df, aes(as.numeric(WAVE),INTENSITY, color = ID))+
  geom_line(show.legend = F, alpha = .5)+
  scale_x_continuous(expand = c(0,0))+
  #scale_y_continuous(limits = c(-5,10), expand = c(0,0))+
  facet_wrap(~as.factor(SAMPLE), scale = "free_y")+
  scale_color_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(colourCount)  ) + 
  themeo

#cleanup
rm(base_df_new,svn_df,foundation,i,IDs,y,poly_below_baseline)


#hop over to:

# product_moment_corr_2021.R # ProductMomemntCorr.R for spectra reference matching
# with base_df
write.csv(base_df,file="reflib_rescaled.csv")
