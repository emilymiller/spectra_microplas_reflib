# reflib fig3b
#
# identifying local maxima for each spectra

base_df<- read.csv("reflib_rescaled.csv")

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
head(base_df)

plas193<-base_df[base_df$PARTICLE=="PLAS193",]

localMaxima(plas193$INTENSITY)
range(plas193$WAVE)#200 - 3400
range(base_df$WAVE) # 


              
range(plas193$INTENSITY)  # 0-1       

#install.packages("ggpmisc")
library(ggpmisc)
library(tidyverse)
library(splus2R)

head(base_df)
?ggpmisc
?stat_peaks
tmp <- dplyr::filter(base_df, PARTICLE==particle.vec[1])
peaks(tmp$INTENSITY, span=101, strict=TRUE)


particle.vec <- sort(unique(base_df$PARTICLE))
max.list <- list()

for(i in 1:length(particle.vec)){
  tmp <- dplyr::filter(base_df, PARTICLE==particle.vec[i])
  
  tmp.peak <- peaks(tmp$INTENSITY, #ignore.threshold=0.2, 
        span=151,
             strict=TRUE)#, na.rm=FALSE)
  tmp.out <- tmp[which(tmp.peak==TRUE),]
  tmp.out <- filter(tmp.out, INTENSITY>0.2)
  max.list[[i]] <- tmp.out
}
max.list[[1]]
max.list[[2]]

# convert list to data frame
max.df <- do.call(rbind.data.frame,max.list)
head(max.df)
nrow(max.df)

table(max.df$PARTICLE)

themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
sc <- scale_colour_gradientn(colours = myPalette(100))#, 
                             #limits=c(1, 8))

ggplot(max.df)+
  geom_point(aes(x=SAMPLE,y=WAVE,color=INTENSITY))+#,color=SAMPLE))+
  sc +
  themeo+ theme(#legend.position="none",
                axis.text.x = 
                  element_text(angle = 90, vjust = 0.5, hjust=1))
# change order manually
max.df <- max.df[order(max.df$PARTICLE),]
write.csv(max.df,file="max.df.csv")

max.df2<-read.csv("max.df.poly.attrib.csv")# reordered
# and named categories, and renamed all unlabeled
# as unlabeled under SAMPLE for figure 3b

max.df.order<-max.df2[!duplicated(max.df2$SAMPLE),]
the.order<-max.df.order$SAMPLE
class(max.df2$SAMPLE)
max.df2$SAMPLE <- 
  factor(max.df2$SAMPLE, levels = the.order)

ggplot(max.df2)+
  geom_point(aes(x=SAMPLE,y=WAVE,color=INTENSITY))+#,color=SAMPLE))+
  sc +
  themeo+ theme(#legend.position="none",
    axis.text.x = 
      element_text(angle = 90, vjust = 0.5, hjust=1))
