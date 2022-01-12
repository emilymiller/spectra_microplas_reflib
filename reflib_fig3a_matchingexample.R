# 
#' #' title: reflib figure 4
#' author: Emily Miller
#' date: Fall 2021
#  output: This was a Figure 3 panel component for Miller et al., but
#   became Figure 4 for Miller et al. A figure depicting 
#   the spectral matching of an unknown weathered plastic to spectra
#   in the reference library. It depicts the spectra of its match
#   (polypropylene) and several spectra it did not match with. This figure
#   is for illustrative puproses as our matching protocol is quantitative
#   and uses Pearson's coefficient of correlation (R) but this figure
#   shows intuitively how these spectra match.




# example of matching
#
# using polypropylene

# PLAS241 weathered PP (strawberry container (agricultural))
# match - PLAS 223 pristine PP (clear plastic drinking straw)
# not match - P - polyester plas 206
# not match - PI - polyimide Plas 197

# plotting all PP

base_df<-read.csv("reflib_rescaled.csv") # from end of preprocessing
head(base_df)

base_df$X<-NULL
PP<- base_df[base_df$poly_lab == "PP",]
unique(base_df$poly_lab)
straw <- base_df[base_df$poly_lab == "strawberry container",]
PP<-rbind(PP,straw)

library(ggplot2)
ggplot(PP)+
  geom_line(aes(x=PP$WAVE,y=PP$INTENSITY,
                color=PP$PARTICLE))+
  facet_wrap(~PARTICLE)+
  themeo

# plotting P

P<- base_df[base_df$poly_lab == "P",]
unique(base_df$poly_lab)
#straw <- base_df[base_df$poly_lab == "strawberry container",]
#PP<-rbind(PP,straw)

ggplot(P)+
  geom_line(aes(x=WAVE,y=INTENSITY,
                color=PARTICLE))+
  facet_wrap(~PARTICLE)+
  themeo

# plotting PI

PI<- base_df[base_df$poly_lab == "PI",]
unique(base_df$poly_lab)
#straw <- base_df[base_df$poly_lab == "strawberry container",]
#PP<-rbind(PP,straw)

ggplot(PI)+
  geom_line(aes(x=WAVE,y=INTENSITY,
                color=PARTICLE))+
  facet_wrap(~PARTICLE)+
  themeo

FPM<-base_df[base_df$PARTICLE=="PLAS031",]# (fluorocarbon used fishing gear)
ggplot(FPM)+
  geom_line(aes(x=WAVE,y=INTENSITY,color=PARTICLE))+
  themeo

latex<-base_df[base_df$PARTICLE=="PLAS221",]# (latex)
ggplot(latex)+
  geom_line(aes(x=WAVE,y=INTENSITY,color=PARTICLE))+
  themeo

chinookmuscle<-base_df[base_df$PARTICLE=="BIOL015",]# (myofibrillar protein)
ggplot(chinookmuscle)+
  geom_line(aes(x=WAVE,y=INTENSITY,color=PARTICLE))+
  themeo


fig2a<- rbind(PP,P,PI,latex,chinookmuscle,FPM)

fig3a<-fig2a[fig2a$PARTICLE=="PLAS241"|
                fig2a$PARTICLE=="PLAS223"|
              fig2a$PARTICLE=="PLAS206"|
              fig2a$PARTICLE=="PLAS197"|
             fig2a$PARTICLE=="BIOL015"|
               fig2a$PARTICLE=="PLAS221"|
             fig2a$PARTICLE=="PLAS031",]
ggplot(fig3a)+
  geom_line(aes(x=WAVE,y=INTENSITY,
                color=PARTICLE))+
  facet_wrap(~PARTICLE)+
  themeo

