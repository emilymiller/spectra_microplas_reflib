#
#' #' title: Reference library Figure 1 (infographic)
#' author: Emily Miller
#' date: 2021
#' output: plots that are used in Figure 1 of Miller et al.
#' manuscript.
#' order: This is the second script used after 
#' cleaning_code.R gets all spectra in a usable format.
#' ---

getwd()
setwd("C:/Users/emily/Documents/MBARI/microplastics/spectra_microplas_reflib")

themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

reflib_source<-read.csv("reflib_source.csv")
reflib_comb<-reflib_source # saving for later
category_counts <- reflib_source[!duplicated(reflib_source$sample_id),]
unique(category_counts$parent_grp)

category_counts$category<-ifelse(category_counts$parent_grp=="biological","biological",
                          ifelse(category_counts$parent_grp=="fishery","wild_plastic",
                          ifelse(category_counts$parent_grp=="consumer" & category_counts$estimated_age=="used","wild_plastic",
                          ifelse(category_counts$parent_grp=="consumer" & category_counts$estimated_age=="used kids toys","wild_plastic",
                          ifelse(category_counts$parent_grp=="container" & category_counts$estimated_age=="used","wild_plastic",
                          ifelse(category_counts$parent_grp=="agricultural", "wild_plastic",
                          ifelse(category_counts$parent_grp=="beachcast", "wild_plastic",
                          ifelse(category_counts$parent_grp=="oml", "wild_plastic", "pristine_plastic"))))))))

head(category_counts)
library(plyr)
# Donut plot

library(ggplot2)

histo<-as.data.frame(plyr::count(category_counts$category))
colnames(histo)[1]<-"category"
colnames(histo)[2]<-"count"
histo

data<-histo
# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

#####################################################
#
# break out weathered plastics
#
#####################################################

head(category_counts)
unique(category_counts$category)
weathered<-category_counts[category_counts$category==
                "wild_plastic",]
head(weathered)
# plot counts of parent_grp

ggplot(weathered,aes(x=parent_grp))+
  geom_bar(stat="count")+
  themeo

#######################################################
#
# animal species silhouettes
# 
#######################################################
#
unique(category_counts$source)

# pacific purple sea urchin
# squid beaks
# chinook salmon
# green mussel
# squid bell
# tiger shrimp carapace
# laysan albatros
# sea otter
# rhino auklet
# macrocystis
# dungeness crab
# zostera eelgrass
# surf clam
# charcoal
# olive ridley sea turtle

# use phylopic downloads

########################################################
#
# bar plot of counts of polymer type
# using reflib_source$gen_poly
# (brand_poly and material_form could be useful,
# especially for simplifying the biological polymers)
# 
########################################################
head(data)
# labeled bar plot

head(category_counts)
category_counts$gen_poly
category_counts$gen_poly<-as.character(category_counts$gen_poly)
# 
# library(plyr)
# polymer_counts<-as.data.frame(plyr::count(category_counts$gen_poly))
# head(polymer_counts)
# colnames(polymer_counts)[1]<- "polymer"
# colnames(polymer_counts)[2]<- "count"
# 


# rename biological polymers by tissue type
# chitin
# cellulose
# dentin
# calcium carbonate
# keratin
# bone
class(category_counts$gen_poly)
category_counts$gen_poly
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
    "Zostera eelgrass blade","cellulose",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
    "tiger shrimp carapace","chitin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "surf clam shell","calcium carbonate",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
   "squid beaks, albatross stomach","chitin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
   "sea otter tooth","dentin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "rhino auklet upper mandible horn","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "Pacific purple urchin spines; Strongylocentrotus purpuratus; from otter female 613","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "olive ridley sea turtle scapula condyle","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "dungeness crab carapace","chitin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "Macrocystis kelp blade","algin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "sea otter maxilla","bone",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "Laysan albatross feather","keratin",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
     "market squid bell","myofibrillar protein",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
    "green mussel shell","calcium carbonate",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
   "Chinook salmon muscle","myofibrillar protein",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
   "Chinook salmon skin","collagen",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
  "acrylic","PMMA",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
   "Chinook salmon skin","collagen",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
   "blue cotton towel","cellulose",category_counts$gen_poly)
category_counts$gen_poly<-ifelse(category_counts$gen_poly==
    "Choy et al 2019 gear #8","cellulose",category_counts$gen_poly)

# Make the plot

# rainbow color scheme
library(RColorBrewer)
colourCount = length(unique(category_counts$gen_poly))
getPalette = colorRampPalette(brewer.pal(11, "RdYlBu"))

# polymer category color scheme
#unique(category_counts$category)



# plot

category_counts$category<-as.factor(category_counts$category)
ggplot(category_counts)+#,
  #   aes(y=reorder(gen_poly,gen_poly,
  #                 function(y)+length(y))))+
  geom_bar(aes(y=reorder(gen_poly,gen_poly,
                        function(y)+length(y))),
           width=0.5, fill=getPalette(colourCount))+
           #width=0.5,fill=getPalette(3))+
  scale_fill_brewer(palette="Spectral")+
  xlab("no. specimens represented")+
  ylab("polymer")+
  themeo+theme(axis.ticks.length = unit(0,"cm"),
               text=element_text(size=8))

################################################################
#
#  NOW INCLUDES THE AGRICULTURAL AND BEACHCAST PLASTICS
# THAT INCLUDE UNKNOWN LABELS
# - first assign knowns like strawberry container (polypropelene)
# - then group others as unknown
# - then color code for polymer categories that are both
# - weathered and pristine
#
# create column that is a combo of polymer-category
# then pull unique
# finalize aesthetics in illustrator
# 
category_counts$polymer_category <- paste(category_counts$gen_poly,
                                          category_counts$category,
                                          sep="-")
head(category_counts)
sort(unique(category_counts$polymer_category))
###################################################################
head(reflib_source)
names(reflib_source)
names(category_counts)
head(category_counts) # want to add gen_poly and category back into 
# reflib_source
colnames(category_counts)[14]<-"polymer_material"
library(dplyr)
category_counts2<-category_counts %>%
  select(sample_id, polymer_material, category)
head(category_counts2)
head(reflib_comb)
reflib_categories<-merge(reflib_comb,category_counts2,by="sample_id",
                         all.x=T)
head(reflib_categories)
unique(reflib_categories$polymer_material)
unique(reflib_categories$category)
write.csv(reflib_categories,file="reflib_categories.csv")

######################################################
#
#cleaning
#
######################################################

rm(df3)
rm(data)
rm(df2)
rm(histo)
rm(reflib_comb)
rm(reflib_source)
rm(category_counts2)

######################################################
#
# correcting wave number in Shipment #1 specimens
# (they were labeled in original pdf S&N sent, not in 
# data files)
#
#####################################################

class(reflib_categories$wave_number)
reflib_categories$wave_number<-as.character(reflib_categories$wave_number)

reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                   "PLAS157","785",reflib_categories$wave_number)
head(reflib_categories)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS115","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS117","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS163","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS032","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "PLAS175","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "BIOL006","785",reflib_categories$wave_number)
reflib_categories$wave_number<-ifelse(reflib_categories$wave_number==
                                        "BIOL013","785",reflib_categories$wave_number)

head(reflib_categories)
write.csv(reflib_categories,file="reflib_categories.csv")

#######################################################

# adjusting color scheme and plotting
# counts of polymers represented in the library

head(category_counts)
str(category_counts)
# color scheme
library(RColorBrewer)
colourCount = length(unique(category_counts$gen_poly))
getPalette = colorRampPalette(brewer.pal(11, "RdYlBu"))

# plot

ggplot(category_counts)+#,
  #   aes(y=reorder(gen_poly,gen_poly,
  #                 function(y)+length(y))))+
  geom_bar(aes(y=reorder(gen_poly,gen_poly,
                         function(y)+length(y))),
           width=0.5,fill=getPalette(colourCount))+
  scale_fill_brewer(palette="Spectral")+
  xlab("no. specimens represented")+
  ylab("polymer")+
  themeo+theme(axis.ticks.length = unit(0,"cm"),
               text=element_text(size=8))


#######################################################
#
#
# take reflib_categories and
# go to preprocessing.R
#
# (reflib_categories is also generated using
# same steps in cleaning_code.R such that cleaning_code.R
# and reflib_fig1.R run independently)
#
#
########################################################

