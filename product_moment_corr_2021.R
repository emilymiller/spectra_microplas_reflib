# from preprocessing.R

# addittional useful libraries
library(forcats)
library(ggridges)
library(stringr)

base_df<- read.csv("reflib_rescaled.csv")
str(base_df)

#dropping levels
base_df$ID<-factor(base_df$ID)
base_df$SAMPLE<-factor(base_df$SAMPLE)
base_df$poly_lab<-factor(base_df$poly_lab)

#cast
test_cast <- spread(data = base_df, key = WAVE, value = INTENSITY)
str(test_cast)
head(test_cast)

###################################################
#
write.csv(test_cast,file="test_cast.csv")
# use in cart.R or maybe not
#
###################################################


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

unique(RMSE_poly$ID) # 79
unique(base_df$ID) # 79
unique(lab_df$ID)# 79



library(RColorBrewer)
n <- 79
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
palette_poly = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#poly_list <- unique(base_df$ID)#c("PVC","PSA","PS","PP","Poly","PLA","PET","PC","PA","LDPE","HDPE","ABS",'PMMA','POM') # no N
poly_list <- unique(RMSE_poly$ID)

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

grid.arrange(grobs = myplots)


###################################################
#
# cladogram
#
#####################################################

# lab_df is our matrix of 71 by 71 (72 with id)

# rename all the columns the ids
class(lab_df)
lab_df2<-lab_df

colnames(lab_df2)<-c(test_castRMSE$ID, "ID")
colnames(lab_df2) <- c(as.character(test_castRMSE$ID), 'ID')
head(lab_df2) # pearson similarity matrix

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
five_dist_mat<- five_dist_mat[,1:71]
head(five_dist_mat)
colnames(five_dist_mat)
row.names(five_dist_mat)<-rowname_df
head(five_dist_mat) # distance matrix

################################################33
five_dist_mat <- 1-five_dist_mat
five_dist_mat2  <- as.dist(five_dist_mat)

#Neighbor-Joining tree with nj()
five_nj <- ape::nj(five_dist_mat2)
#Plot unrooted NJ tree
plot(five_nj, "unrooted")
?nj()

#Plot rooted NJ tree
plot.phylo(five_nj,font=1,adj=0.2)



#Build UPGMA tree
five_upgma <- phangorn::upgma(five_dist_mat2)
#Plot UPGMA tree
plot(five_upgma)
#Compare the rooted NJ and the UPGMA
par(mfrow = c(1,2))
plot(five_nj)
my_upgma <- phangorn::upgma(five_dist_mat2)
plot(my_upgma)
plot(fastme.ols(five_dist_mat2))

class(rowname_df)
rowname_chr<-as.character(rowname_df)
vert.tree<-read.tree(text=rowname_chr)
plot(vert.tree,no.margin=TRUE,edge.width=2)#error
library(phytools)
roundPhylogram(vert.tree) # error


#######################################################################
#
#
#
#######################################################################


# pull out closest plastic and dist away from sample
poly_df <- NULL
str(RMSE_poly)
close_plastic <- spread(data = RMSE_poly, key = WAVE, value = INTENSITY)
str(close_plastic)

for(i in 1:dim(close_plastic)[1]) {
  row <- close_plastic[i,] 
  col_num  <- which.max(row[,c(6:19)]) # for Pearsons R
  #col_num  <- which.min(row[,c(6:18)]) # for RMSE
  col_name <- names(col_num)
  dist     <- row[,col_name]
  row <- data.frame(cbind(col_name,dist))
  poly_df <- rbind(poly_df,row)
  print(i)
}

str(poly_df)

poly_df <- cbind(ID = close_plastic$ID, poly_df)
poly_df$dist <- as.numeric(as.character(poly_df$dist))

# generate columns of distance from poly[i] plastic
# Bar plot
poly_df$col_name <- fct_infreq( poly_df$col_name, ordered = T)
closest_p <- ggplot(poly_df,aes(col_name, fill = col_name ) ) +
  geom_bar()+
  scale_fill_manual(values = palette_poly) +
  themeo
closest_p

avg_dst <-poly_df %>% group_by(col_name) %>% summarise(mean_dist = mean(dist)) 
avg_dist <- ggplot(avg_dst, aes(x = col_name, y = mean_dist, fill = col_name))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = palette_poly) +
  themeo

grid.arrange(closest_p, avg_dist, ncol = 1)

dist_dist <- ggplot(poly_df,aes(y = col_name, x = dist, fill = col_name))+
  geom_density_ridges(scale = 2, bandwidth = .01, stat = "binline", bins = 50)+
  geom_vline(xintercept = quantile(poly_df$dist, .22), color = "red", linetype = "dotted")+
  #annotate("text",y = quantile(poly_df$dist, .75),x = 9 ,vjust = -1, label = "Quantile cutoff", color = "red")+
  scale_fill_manual(values = palette_poly)+
  scale_x_continuous(expand = c(0,0), limits = c(0,.75))+
  themeo
grid.arrange(closest_p, dist_dist)

# plot high scores
poly_df <- droplevels(poly_df)
str(poly_df)

# for online database repository
kyle_df <- close_plastic[,6:19]
kyle_df$closest_poly <- poly_df$col_name
kyle_df$dist_closest_poly <- poly_df$dist
kyle_df$SAMPLE <- close_plastic$SAMPLE
kyle_df$PARTICLE <- close_plastic$PARTICLE
kyle_df$SOURCE <- close_plastic$SOURCE
kyle_df$training_poly <- close_plastic$poly_lab
kyle_df$ID <- close_plastic$ID
kyle_df$ID <- as.factor(gsub("-"," ",kyle_df$ID))
kyle_df$ID <- as.factor(gsub("_"," ",kyle_df$ID))
kyle_df$depth <- str_extract(kyle_df$ID, pattern = regex("(?<= ).*(?=m)"))
kyle_df$depth <- sub(".* ", "", kyle_df$depth) %>% as.numeric()
str(kyle_df)

write.csv(kyle_df,"kyle_df2_2021.csv" )

#spectra database build
write.csv(base_df,"spectra_db_2021.csv")

hs_poly_df <- subset(poly_df, dist > .13) #cutoff established from lower 90% quantile, see KVH
hs_poly_df$col_name <- fct_infreq( hs_poly_df$col_name, ordered = T)

hs_poly <- ggplot(hs_poly_df,aes(col_name, fill = col_name ) ) +
  geom_bar()+
  scale_fill_manual(values = palette_poly) +
  themeo
grid.arrange(hs_poly,dist_dist)

hs_poly_df <- merge(hs_poly_df, base_df, by = "ID")
ggplot(hs_poly_df,aes(x = WAVE, y = INTENSITY + (dist)*4, group = ID, color = poly_lab))+
  geom_line(show.legend = F, size = .25)+
  facet_wrap(~col_name, ncol = 3)+
  scale_x_continuous(expand = c(0,0))+
  scale_color_manual(values = palette_poly)+
  themeo

str(hs_poly_df)
# head over to:
# similiarity_decay_plot.r


