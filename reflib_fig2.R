# 
#' #' title: reflib figure 2
#' author: Emily Miller
#' date: Fall 2021
#  output: Figure 2 for Miller et al. A figure depicting the steps used to 
#  process the spectra before inputing them into our matching protocol.
#
# BIOL 004
getwd()
biol004_532<-read.csv("./S&N Labs results/24364 Raw Spectra and CSV Files/24364r31 BIOL 004 SPA_1.csv",
                    skip=1)
biol004_785 <- read.csv("./S&N Labs results/24364 Raw Spectra and CSV Files/24364r31 BIOL 004 correction.csv",
                        skip=1)
head(biol004_532)
biol004_532$wavenumber<-532
biol004_785$wavenumber<-785

# plot these on top of one another
biol004<-rbind(biol004_532,biol004_785)
plot(biol004$INT~biol004_532$Raman.Shift...cm.1)
ggplot(biol004, aes(x=Raman.Shift...cm.1, y=INT,
                color=wavenumber)) + 
  geom_line()+ facet_wrap(.~wavenumber,scales="free",ncol=1)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))

# with polynomial corrected version on bottom
head(base_df)# from preprocessing.R
postprocbiol004<-base_df[base_df$PARTICLE=="BIOL004",]

postprocbiol004$wavenumber<-"postprocessed"
colnames(postprocbiol004)[4]<-"Raman.Shift...cm.1"
colnames(postprocbiol004)[3]<-"INT"
postprocbiol004$ID<-NULL
postprocbiol004$SAMPLE<-NULL
postprocbiol004$PARTICLE<-NULL
postprocbiol004$poly_lab<-NULL
head(postprocbiol004)
head(biol004)
biol004<-biol004[,c(2,1,3)]
biol004_again<-rbind(biol004,postprocbiol004)

postproc<-ggplot(postprocbiol004, 
       aes(as.numeric(WAVE),INTENSITY))+
  geom_line(show.legend = F, alpha = .5)+
  scale_x_continuous(expand = c(0,0))+
  scale_color_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(colourCount)  ) + 
  themeo

unique(biol004_again$wavenumber)
ggplot(biol004_again, aes(x=Raman.Shift...cm.1, y=INT)) + 
  geom_line()+ facet_wrap(.~wavenumber,scales="free",ncol=1)+
  xlab("Raman Shift / cm-1")+
  ylab("Intensity")+
  themeo + theme(text=element_text(size=6))
