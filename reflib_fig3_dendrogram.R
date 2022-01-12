#' ---
#' #' title: Reflib Fig 3 dendrogram using
#'      Product Moment Correlation (Pearson's R, a similarity measure)
#' author: Emily Miller, based on Tyler Gagne's script in Choy et al. 2019
#' date: Fall 2021
#' output: This matching protocol (start with cleaning_code.R followed by
#'  preprocessing.R before using this script) will produce a similarity/distance 
#'  matrix to assign labels from the reference library to unlabeled spectra. It
#'  is the matching protocol described in Miller et al. and can be used
#'  to create the dendrogram in Figure 3 when all pairwise Pearson's
#'  values are used to construct relationships.
#' ---
#' 
#' 
#' base_df<- read.csv("reflib_rescaled.csv")
str(base_df)

#dropping levels
base_df$ID<-factor(base_df$ID)
base_df$SAMPLE<-factor(base_df$SAMPLE)
base_df$poly_lab<-factor(base_df$poly_lab)

#cast
test_cast <- spread(data = base_df, key = WAVE, value = INTENSITY)
str(test_cast)

gc()

head(base_df)
poly <- unique(base_df$ID)
head(base_df)
base_df$X<-NULL

# make a plastic DF
test_castRMSE <- test_cast
rm(test_cast)
lab_df<- matrix(ncol = length(poly), nrow = dim(test_castRMSE)[1]) %>% 
  as.data.frame()
lab_df$ID <- test_castRMSE$ID
test_castRMSE$X<-NULL

for(d in 1:length(poly)){
  
  plas_one <- subset(test_castRMSE, ID == poly[d])    # obtain spectra of one plastic
  plas_one <- plas_one[,6:dim(plas_one)[2]] %>%       # turn it in to a vector
    as.matrix() %>% as.vector()      # 
  colnames(lab_df)[d] <- poly[d]
  
  for(i in 1:dim(test_castRMSE)[1]){
    
    unlab_one <- test_castRMSE[i,6:dim(test_castRMSE)[2]] %>% # obtain one unlabeled spectra
      as.matrix() %>% as.vector() 
    RMSE <- cor(unlab_one,plas_one)                                                              # this is Pearson's R, calling it RMSE for script continuity
    lab_df[ i, d ] <- as.numeric(RMSE)    # provide the RMSE val for that spectra for that plastic
    print(i)
  }
}

#clean up
rm(d,i,plas_one,RMSE,unlab_one)
str(lab_df)
RMSE_poly <- merge(base_df,lab_df, by = "ID")



library(RColorBrewer)
n <- 71
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
palette_poly = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
poly_list <- unique(base_df$ID)#c("PVC","PSA","PS","PP","Poly","PLA","PET","PC","PA","LDPE","HDPE","ABS",'PMMA','POM') # no N


# grid of figures
myplots <- list()  # new empty list
for (i in 1:length(poly_list))
  local({
    i <- i
    name <- poly_list[i]
    p1 <- ggplot(data = RMSE_poly,aes(x = RMSE_poly$WAVE, y = RMSE_poly$INTENSITY + (RMSE_poly[,name]), color = ID))+ # *4 for RMSE
      geom_line(aes(group = ID), show.legend = F) +
      #scale_color_brewer(palette = "Spectral")+
      annotate("text", x = 950, y = 1.9, label = name, hjust = "left")+
      scale_color_manual(values = palette_poly) +
      scale_x_continuous(expand = c(0,0)) +
      #geom_hline(yintercept = c(.13,.22,.4)) +
      coord_cartesian(ylim = c(0.13,2)) + # median cutoff established
      xlab(NULL)+
      ylab(NULL)+
      themeo
    p1
    print(i)
    myplots[[i]] <<- p1  # add each plot into plot list
  })
library(gridExtra)
grid.arrange(grobs = myplots)


###################################################
#
# cladogram (dendrogram)
#
#####################################################

# lab_df is our matrix of 79 by 79 (80 with id)

# rename all the columns the ids
class(lab_df)
lab_df2<-lab_df

colnames(lab_df2)<-c(test_castRMSE$ID, "ID")
colnames(lab_df2) <- c(as.character(test_castRMSE$ID), 'ID')
head(lab_df2)

#################################################
#
# turn similarity matrix into distance matrix
#
###############################################

library(ape)
library(phangorn)

# Turn into a distnace matrix. 
# This is 2 steps and requires the as.dist() command
rowname_df<-lab_df2$ID
#lab_df2$ID<-NULL
head(lab_df2)
#five_dist_mat<- as.matrix(lab_df2)
five_dist_mat<-data.matrix(lab_df2, rownames.force = NA)
five_dist_mat<- five_dist_mat[,1:79]
head(five_dist_mat)
colnames(five_dist_mat)
row.names(five_dist_mat)<-rowname_df
head(five_dist_mat) # distance matrix

###################
library(ape)
library(stats)
library(ade4)
library(adegenet)
library(phangorn)

temp <- as.data.frame(as.matrix(five_dist_mat))
temp$ID<-NULL
table.paint(temp, cleg=0, clabel.row=.5, clabel.col=.5)

#####################################


#####################################
five_dist_mat <- 1-five_dist_mat
five_dist_mat2  <- as.dist(five_dist_mat)

#Neighbor-Joining tree with nj()

# cluster dendrogram
h_cluster <- hclust(five_dist_mat2, method = "average", members = NULL) # method = average is used for UPGMA, members can be equal to NULL or a vector with a length of size D
plot(h_cluster, cex = 0.6)
