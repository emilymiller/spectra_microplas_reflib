#' ---
#' 
#' #' title: Technical validation using weathered plastics
#' author: Emily Miller, based on Tyler Gagne's script
#' date: Fall 2021
#' output: Start with cleaning_code.R followed by preprocessing.R before 
#' using this script). Here, we take unknown weathered plastics and follow our
#' matching protocol to assign known labels from our reference library.
#'  We later cross-reference these assignments with those done by S&N Labs
#'  using their (~6000 plastics in their library). This script produces a
#'  similarity/distance matrix based on Pearson's correlation coefficients between
#'  paris. It then assigns the label of the closest match from the reference 
#'  library to the unlabeled spectra. It
#'  is the matching protocol described in Miller et al.
#' ---

###################################################

  # try with the 8 unknowns 
  # use our library
  # and use S&N's library
  
###############################################

reflib<-read.csv("reflib_rescaled.csv") # from preprocessing.R
head(reflib)
reflib$X<-NULL
class(reflib$poly_lab)
reflib$poly_lab<-as.character(reflib$poly_lab)
reflib$poly_lab<-ifelse(reflib$ID=="strawberry container PLAS241","unlabeled",
          ifelse(reflib$ID=="strapping tape blue PLAS244","unlabeled",
          ifelse(reflib$ID=="white and gray mask filter PLAS252","unlabeled",
          ifelse(reflib$ID=="green plastic mulch PLAS240","unlabeled",
          ifelse(reflib$ID=="drip irrigation tubing PLAS248","unlabeled",
          ifelse(reflib$ID=="clear plastic hollow sphere PLAS256","unlabeled",
          ifelse(reflib$ID=="blue fragment PLAS253","unlabeled",
          ifelse(reflib$ID=="beachcast white foam sheet PLAS251","unlabeled",reflib$poly_lab))))))))
reflib$poly_lab<-as.factor(reflib$poly_lab)
head(reflib)
###############
#
### match unknowns to reflib
#
################

#unknowns$poly_lab<-"unlabeled"

#test_cast <- spread(data = unknowns, key = WAVE, value = INTENSITY)
str(test_cast)
head(base_df)
library(tidyverse)

reflib2<-reflib[!reflib$poly_lab=="unlabeled",]
unique(reflib2$poly_lab)
reflib2$ID<-as.character(reflib2$ID)
reflib2$ID<-as.factor(reflib2$ID)
reflib2$poly_lab<-as.character(reflib2$poly_lab)
reflib2$poly_lab<-as.factor(reflib2$poly_lab)
poly <- unique(reflib2$ID)


head(reflib)
head(unknowns)
#base_dt_new$poly_lab<-base_dt_new$ID

head(reflib)
test_cast_ref<-spread(data=reflib,key=WAVE,value=INTENSITY)
rm(reflib_processed)
rm(test_castRMSE)
head(test_cast)

#test_cast_bind<-rbind(test_cast,test_cast_ref)
test_cast_bind<-test_cast_ref
head(test_cast_bind)
unique(test_cast_bind$ID)
unique(test_cast_bind$`200`)
# test_cast_bind is one dataframe (long form)
# of both the known "reflib" and unknown "base_df"
# spectra. 
# All known samples have a plastic type in the
# "poly_lab" column. All unknown samples have
# "unlabeled" in that column.
#
# We want to select one unknown spectra and compare it
# to all known spectra using the following loop that 
# calculates a Pearson similarity measure for each pair.
# Then we select the known spectra with the most similar
# value and assign that identity along with that value to
# each unknown sample in a new dataframe.
#


# make a plastic DF
test_castRMSE <- test_cast_bind # test_cast
lab_df<- matrix(ncol = length(poly), nrow = dim(test_castRMSE)[1]) %>% as.data.frame()
lab_df$ID <- test_castRMSE$ID

head(reflib)

for(d in 1:length(poly)){
  print(d)
  # first create temporary datafram just with reference samples
  tmp <- dplyr::filter(test_castRMSE, poly_lab != 'unlabeled')
  # create a subset that is just a single entry for ID
  tmp$ID <- factor(tmp$ID)
  plas_one <- subset(tmp, ID == poly[d])
  
  #plas_one <- subset(test_castRMSE, ID == poly[d])    # obtain spectra of one plastic
  plas_one <- plas_one[,6:dim(plas_one)[2]] %>%       # turn it in to a vector
    as.matrix() %>% as.vector()                        # 
  colnames(lab_df)[d] <- poly[d]
  
  for(i in 1:dim(test_castRMSE)[1]){
    
    
    unlab_one <- test_castRMSE[i,6:dim(test_castRMSE)[2]] %>% # obtain one unlabeled spectra
      as.matrix() %>% as.vector() 
    
    RMSE <- cor(unlab_one,plas_one)    # this is Pearson's R, calling it RMSE for script continuity
    lab_df[ i, d ] <- as.numeric(RMSE)    # provide the RMSE val for that spectra for that plastic
    #print(i)
  }
}


rm(myplots)

#clean up
rm(d,i,plas_one,RMSE,unlab_one)
str(lab_df)
lab_df2<-lab_df
# assign columns based on poly vector
poly2 <- append(as.character(poly), "ID")
poly
poly2
colnames(lab_df) <- poly2
head(lab_df)


#highest_pearson<-as.data.frame(lab_df$ID)
#df$max_points_rebs <- pmax(df$points, df$rebounds)
#highest_pearson$pearson_value<-pmax(highest_pearson)
head(lab_df)
class(lab_df$ID)
rownames(lab_df)<-lab_df$ID
lab_df$ID<-NULL
head(lab_df)
library(tidyverse)


##############################
#
# PULLING JUST HIGHEST VALUE FOR EACH ROWNAME (ID)
# AND TAKING THE COLUMN NAME (ID) FOR WHICH 
# THAT VALUE CAME FROM
library(tidyverse)
head(lab_df)

high_pear2<- lab_df %>% 
  rownames_to_column() %>%
  gather(column, value, -rowname) %>%
  group_by(rowname) %>% 
  dplyr::filter(rank(-value) == 1) 

######################################

head(high_pear2)
head(high_pear)
high_pear<-as.data.frame(high_pear)
colnames(high_pear)[1]<-"ID"
unknowns$poly_lab<-"unlabeled"
meta<-rbind(reflib,unknowns)
head(meta)
meta<-meta[!duplicated(meta$ID),]

high_pear_meta<-merge(high_pear,meta,by="ID",all.x=T)
head(high_pear_meta)
high_pear_meta$WAVE<-NULL
high_pear_meta$INTENSITY<-NULL


pear_unlab<-high_pear_meta[high_pear_meta$poly_lab=="unlabeled",]
unique(pear_unlab$ID)

library(stringr)

#pear_unlab$assignment <- #word(pear_unlab$column, 1)
head(pear_unlab)
colnames(pear_unlab)[2]<-"assignment"
#class(pear_unlab$assignment)
pear_unlab$assignment<-as.factor(pear_unlab$assignment)

pear_unlab2<-pear_unlab[!duplicated(pear_unlab$ID),]
write.csv(pear_unlab2,file="slopp_reflib_assign.csv")

palette_poly <- c("lightgreen","green","limegreen","seagreen2",
                  "darkgreen","seagreen4","darkred","maroon4",
                  "pink","magenta","purple","red")
poly_list <- c("algin","calcium","cellulose","charcoal",
               "chitin","myofibrillar","PBAT","PC",
               "PEEK","PMMA","PS","PVC")

ggplot(pear_unlab, aes(x=ID, y=y, fill=assignment)) +
  geom_bar(stat='identity')+
  # scale_fill_manual()+#values = palette_poly) +
  themeo


