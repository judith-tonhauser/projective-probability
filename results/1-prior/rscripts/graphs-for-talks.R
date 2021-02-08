# Prior paper
# Exp 2a (prior measured separately)
# establish prior probabilities for contents given one of two facts
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(forcats)
library(dichromat)
theme_set(theme_bw())

# load clean data for analysis 
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
nrow(cd) #1650

summary(cd)

# target data
target <- subset(cd, cd$item != "F1" & cd$item != "F2")
target <- droplevels(target)
nrow(target) #1500 = 75 participants x 20 items
table(target$item)

# add content information
target$event <- target$prompt
target$event <- gsub("How likely is it that ","",target$event)
target$event <- gsub("\\?","",target$event)
table(target$event)

cols <- c("itemNr","event")
cols
target$eventItemNr <- do.call(paste, c(target[cols], sep=": "))
table(target$eventItemNr)

# mean responses and standard deviations of responses to H and L items
mean.HL = aggregate(response~itemType, data=target, FUN="mean")
mean.HL
#H: .7
#L: .16
sd.HL = aggregate(response~itemType, data=target, FUN="sd")
sd.HL
#H: .21
#L: .17

names(target)
table(target$prompt)

# create column that codes the content with its number
target$eventItemNr  = factor(target$eventItemNr, 
                             levels=c("1:  Mary is pregnant",
                                      "2:  Josie went on vacation to France",
                                      "3:  Emma studied on Saturday morning",     
                                      "4:  Olivia sleeps until noon",
                                      "5:  Sophia got a tattoo",
                                      "6:  Mia drank 2 cocktails last night",
                                      "7:  Isabella ate a steak on Sunday",       
                                      "8:  Emily bought a car yesterday",
                                      "9:  Grace visited her sister",
                                      "10:  Zoe calculated the tip",               
                                      "11:  Danny ate the last cupcake",
                                      "12:  Frank got a cat",
                                      "13:  Jackson ran 10 miles",                
                                      "14:  Jayden rented a car",
                                      "15:  Tony had a drink last night",
                                      "16:  Josh learned to ride a bike yesterday",        
                                      "17:  Owen shoveled snow last winter",       
                                      "18:  Julian dances salsa",
                                      "19:  Jon walks to work",                    
                                      "20:  Charley speaks Spanish"))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

# plot for XPRAG abstract (content identified only by number) ----
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999", 

means = target %>%
  group_by(itemNr,itemType) %>%
  summarise(Mean = mean(response),CILow=ci.low(response),CIHigh=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(itemType = fct_recode(itemType,high="H",low="L"))
means

low = means %>%
  filter(itemType == "low") %>%
  mutate(itemNr = fct_reorder(itemNr,Mean))
low

means = means %>%
  mutate(item = fct_relevel(itemNr,levels(low$itemNr)))
means

ggplot(means, aes(x=item, y=Mean, color=itemType)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  # scale_x_continuous(breaks=seq(1,20,1)) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Fact", breaks=c("high","low"),labels=c("high", "low"), 
                     values=cbPalette) +
  theme(legend.position = "top") +
  #theme(legend.position = c(0.3, 0.8)) +
  ylab("Mean prior probability") +
  xlab("Content of complement") 
ggsave(f="../graphs/ratings-for-CCs.pdf",height=3.2,width=6)

