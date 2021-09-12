# reflib figure 3
# part a
# spectra matching


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

PP<- base_df[base_df$poly_lab == "PP",]
unique(base_df$poly_lab)
straw <- base_df[base_df$poly_lab == "strawberry container",]
PP<-rbind(PP,straw)

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

fig2a<- rbind(PP,P,PI)

fig3a<-fig2a[fig2a$PARTICLE=="PLAS241"|
                fig2a$PARTICLE=="PLAS223"|
              fig2a$PARTICLE=="PLAS206"|
              fig2a$PARTICLE=="PLAS197",]
ggplot(fig3a)+
  geom_line(aes(x=WAVE,y=INTENSITY,
                color=PARTICLE))+
  facet_wrap(~PARTICLE)+
  themeo
