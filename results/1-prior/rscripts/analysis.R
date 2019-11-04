# Prior probability work
# 1-prior: analysis of experiment that measured prior given one of two facts

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(forcats)
library(dichromat)
theme_set(theme_bw())

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #2090 (95 participants x 22 items)
names(d)
length(unique(d$workerid)) #95 participants

d = d %>%
  dplyr::select(workerid,rt,prompt,itemType,itemNr,list,item,response,fact,slide_number_in_experiment,gender,american,age,language,comments,Answer.time_in_minutes)
nrow(d) #2090

# look at Turkers' comments
unique(d$comments)

# age and gender info
table(d$age) #21-75
median(d$age) #33
table(d$gender)
#45 female, 50 male

### exclude non-American English speakers
length(unique(d$workerid))
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "hindi" & d$language != "female" & d$language != "russian"))
d = droplevels(d)
length(unique(d$workerid)) #92

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #87 total

## how often was each list completed?
table(d$list)

## exclude Turkers based on the two control stimuli
table(d$item)

# make relevant subsets
# filler/control 1 (high responses expected)
d.f1 <- subset(d, d$item == "F1")
d.f1 <- droplevels(d.f1)
nrow(d.f1) #87

# filler/control 2 (low responses expected)
d.f2 <- subset(d, d$item == "F2")
d.f2 <- droplevels(d.f2)
nrow(d.f2) #87

# data on both controls/fillers
d.f12 <- rbind(d.f1,d.f2)
nrow(d.f12) #174

# group mean on filler 1
round(mean(d.f1$response),2) #.86

# group mean on filler 2
round(mean(d.f2$response),2) #.03

ggplot(d.f12, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(d.f12$response, n = 10)) +
  ylab("Responses to fillers") +
  xlab("Participant")
ggsave(f="../graphs/filler-ratings.pdf",height=4,width=20)

# Turkers with response to filler 1 that is more than 2sd below group mean
# this is the exclusion criterion we decided on for the factivity paper
c.means = aggregate(response~workerid, data=d.f1, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=d.f1, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=d.f1, FUN="ci.high")$response
c.means

c.f1 <- c.means[c.means$response < (mean(c.means$response) - 2*sd(c.means$response)),]
c.f1
unique(length(c.f1$workerid)) #9 Turkers
mean(c.f1$response)

# Turkers with response to filler 2 that is more than 2sd above group mean
# this is the exclusion criterion we decided on for the factivity paper
c.means = aggregate(response~workerid, data=d.f2, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=d.f2, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=d.f2, FUN="ci.high")$response
c.means

c.f2 <- c.means[c.means$response > (mean(c.means$response) + 2*sd(c.means$response)),]
c.f2
unique(length(c.f2$workerid)) #3 Turkers
mean(c.f2$response)

# # Turkers who gave responses to F1 lower than .8
# # this is a bit arbitrary, I don't remember how we came to this number
# f1 <- d.f1[d.f1$response < .8,]
# f1
# nrow(f1) #17
# 
# ggplot(f1, aes(x=workerid,y=response)) +
#   geom_point(aes(colour = item)) +
#   geom_text(aes(label=workerid), vjust = 1, cex= 5) +
#   geom_text(aes(label=response), vjust = 2.5, cex= 5) +
#   scale_y_continuous(breaks = pretty(f1$response, n = 10)) +
#   ylab("Responses to filler 1") +
#   xlab("Participants who gave bad responses (expected high)")
# 
# # Turkers who gave responses to F2 higher than .2
# f2 <- d.f2[d.f2$response > .2,]
# nrow(f2) #3
# 
# ggplot(f2, aes(x=workerid,y=response)) +
#   geom_point(aes(colour = item)) +
#   geom_text(aes(label=workerid), vjust = 1, cex= 5) +
#   geom_text(aes(label=response), vjust = -2.5, cex= 5) +
#   scale_y_continuous(breaks = pretty(f2$response, n = 10)) +
#   ylab("Responses to filler 2") +
#   xlab("Participants who gave bad responses (expected low)")

# f <- rbind(c.f1,c.f2)
# f
# nrow(f) #22
# 
# ggplot(f, aes(x=workerid,y=response)) +
#   geom_point(aes(colour = item)) +
#   geom_text(aes(label=workerid), vjust = 1, cex= 5)+
#   #geom_text(aes(label=response), vjust = -1, cex= 5) +
#   scale_y_continuous(breaks = pretty(f$response, n = 10)) +
#   ylab("Responses to fillers") +
#   xlab("Participants who gave bad responses (red/F1 expected high, blue/F2 expected low)")
# ggsave(f="../graphs/bad-filler-ratings.pdf",height=4,width=20)

# length(unique(f$workerid)) #19 Turkers

# exclude the Turkers identified above
d <- subset(d, !(d$workerid %in% c.f1$workerid | d$workerid %in% c.f2$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #75 Turkers remain

filler <- droplevels(subset(d,d$itemType == "F"))
nrow(filler)

ggplot(filler, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(filler$response, n = 10)) +
  ylab("Responses to fillers") +
  xlab("'Good' Participant")
ggsave(f="../graphs/filler-ratings-good-participants.pdf",height=4,width=20)

# clean data = cd
cd = d
write.csv(cd, file = "../data/cd.csv")
head(cd)
nrow(cd) #1650 / 22 items = 75 participants
table(cd$fact,cd$itemNr)

# load clean data for analysis ----
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
nrow(cd) #1650

# age info
table(cd$age) #21-75
median(cd$age) #35
table(cd$gender)
#34 female, 41 male

# look at responses to fillers in clean data
# filler/control 1 (high responses expected)
cd.f1 <- subset(cd, cd$item == "F1")
cd.f1 <- droplevels(cd.f1)
nrow(cd.f1) #75

# filler/control 2 (low responses expected)
cd.f2 <- subset(cd, cd$item == "F2")
cd.f2 <- droplevels(cd.f2)
nrow(cd.f2) #75

# group mean on filler 1
round(mean(cd.f1$response),2) #.95
round(sd(cd.f1$response),2) #.14

# group mean on filler 2
round(mean(cd.f2$response),2) #.01
round(sd(cd.f2$response),2) #.03

# target data
target <- subset(cd, cd$item != "F1" & cd$item != "F2")
target <- droplevels(target)
nrow(target) #1500 = 75 participants x 20 items
table(target$item)

# add content information
target$event <- target$prompt
target$event <- gsub("How likely is it that","",target$event)
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

# plot for XPRAG abstract (content identified only by number)
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999", 
                
means = target %>%
  group_by(itemNr,itemType) %>%
  summarise(Mean = mean(response),CILow=ci.low(response),CIHigh=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(itemType = fct_recode(itemType,high="H",low="L"))

high = means %>%
  filter(itemType == "high") %>%
  mutate(itemNr = fct_reorder(itemNr,Mean))

means = means %>%
  mutate(item = fct_relevel(itemNr,levels(high$itemNr)))
means

# ggplot(means, aes(x=response,fill=itemType)) +
#   geom_histogram(binwidth=.05,alpha=.8) +
#   scale_fill_manual(values=cbPalette,name="Prior content probability") +
#   xlab("Mean probability rating") +
#   ylab("Number of cases") +
#   theme(legend.position=c(.7,.8))
# ggsave("../graphs/meanprobratings.pdf",height=2.8,width=4.5)

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

ggplot(target, aes(x=itemNr,y=response,color=itemType)) +
  geom_point(aes(colour = itemType)) +
  scale_fill_manual(values=cbPalette,name="Prior content probability") +
  geom_point(data = means, size = 3) +
  geom_errorbar(data = means, aes(ymin=YMin, ymax=YMax)) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(breaks = seq(0,1,by = .2)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  #scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  #, "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.position="none") +
  ylab("Mean prior probability \n rating by fact") +
  xlab("Content of complement") 
ggsave(f="../graphs/target-ratings.pdf",height=3.2,width=6)

# plot that also displays content 
target$event <- target$prompt
target$event <- gsub("How likely is it that","",target$event)
target$event <- gsub("\\?","",target$event)
table(target$event)

cols <- c("itemNr","event")
cols
target$eventItemNr <- do.call(paste, c(target[cols], sep=": "))
table(target$eventItemNr)

means = aggregate(response~item+itemType+eventItemNr, data=target, FUN="mean")
means$YMin = means$response - aggregate(response~item+itemType+eventItemNr, data=target, FUN="ci.low")$response
means$YMax = means$response + aggregate(response~item+itemType+eventItemNr, data=target, FUN="ci.high")$response
means


sd = aggregate(response~item+itemType+eventItemNr, data=target, FUN="sd")
sd

print(unique(target$eventItemNr))

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


ggplot(target, aes(x=eventItemNr,y=response)) +
  geom_point(aes(colour = itemType)) +
  geom_point(data = means, size = 3) +
  geom_errorbar(data = means, aes(ymin=YMin, ymax=YMax)) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(breaks = seq(0,1,by = .2)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  #, "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.position="none") +
  ylab("Likeliness rating") +
  xlab("Event") 
ggsave(f="../graphs/target-ratings.pdf",height=8,width=10)

means = target %>%
  group_by(event,itemType,fact) %>%
  summarize(PriorMean = mean(response)) %>%
  ungroup() %>%
  mutate(itemType = paste("fact",itemType,sep=""))
write.csv(means,file="../data/prior_means.csv",row.names=F,quote=F)

