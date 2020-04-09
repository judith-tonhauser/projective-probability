# Prior probability work
# norming study to establish prior probabilities for contents given one of two facts
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

# load clean data for analysis ----
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
nrow(cd) #1650

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

# plot for XPRAG abstract (content identified only by number)
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

# mean prior probability ratings, content identified by clause
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999", 

means = target %>%
  group_by(event,itemType) %>%
  summarise(Mean = mean(response),CILow=ci.low(response),CIHigh=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  select(-CILow,-CIHigh) %>%
  mutate(itemType = fct_recode(itemType,high="H",low="L"))
means

# save for comparison with Exp2 findings
write.csv(means,file="../data/prior_means.csv",row.names=F,quote=F)


# means_wide = means %>%
#   #select(-YMax,-YMin) %>%
#   #pivot_wider(names_from = itemType, values_from = c(Mean))
#   pivot_wider(names_from = itemType, values_from = c(Mean,YMax,YMin)) %>%
#   select(event,Mean_high,Mean_low)
# means_wide
# means_wide$diff = means_wide$Mean_high - means_wide$Mean_low
# 
# means$event <- as.factor(means$event)
# str(means$event)
# levels(means$event)
# 
# means = merge(means,means_wide, by = "event")
# means
# 
# tmp = means %>%
#   filter(itemType == "high") %>%
#   mutate(event = fct_reorder(event,diff))
# 
# means = means %>%
#   mutate(event = fct_relevel(event,levels(tmp$event)))
# means

low = means %>%
  filter(itemType == "low") %>%
  mutate(event = fct_reorder(event,Mean))

means = means %>%
  mutate(event = fct_relevel(event,levels(low$event)))
means

subjmeans = target %>%
  group_by(event,workerid,itemType) %>%
  summarize(Mean = mean(response)) %>%
  mutate(itemType = fct_recode(itemType,high="H",low="L"))
subjmeans$event <- factor(subjmeans$event, levels = unique(levels(means$event)))
levels(subjmeans$event)
names(subjmeans)

ggplot(means, aes(x=event, y=Mean, color=itemType,shape=itemType,fill=itemType)) + 
  geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.08) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Fact", breaks=c("higher probability","lower probability"),labels=c("higher probability", "lower probability"), 
                     values=cbPalette) +
  theme(legend.position = "top",legend.text=element_text(size=12)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean prior probability") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings.pdf",height=7,width=8)

# comparison of prior probability means from Exp1 and Exp2

# load data from Exp1 (called exp4 in repo)
means1 <- read.csv(file="../../exp4/data/prior_means.csv")
summary(means1) #eventItem, prior_type, Mean, YMin, YMax, CILow, CIHigh

means1 = means1 %>%
  select(-CILow,-CIHigh)
summary(means1) #eventItem, prior_type (high_prior, low_prior), Mean, YMin, YMax

# load data from this experiment (Exp2, called 1-prior in repo)
means2 <- read.csv(file="../data/prior_means.csv")
summary(means2) #event, itemType (high, low), Mean, YMin, YMax

# rename columns and levels in preparation for merging
means1 = means1 %>%
  mutate(prior_type = fct_recode(prior_type,high="high_prior",low="low_prior")) %>%
  rename(event = "eventItem", Mean1 = "Mean", YMin1 = "YMin", YMax1 = "YMax")
summary(means1) #prior_type (high, low), event, Mean1, YMin1, YMin1
str(means1$prior_type)
str(means1$event)

means2 = means2 %>%
  rename(prior_type = "itemType", Mean2 = "Mean", YMin2 = "YMin", YMax2 = "YMax")
summary(means2) #event, prior_type (high, low), Mean2, YMin2, YMax2
str(means2$prior_type)
str(means1$event)

# join the two datasets
means = left_join(means1, means2, by= c("event", "prior_type")) 
means
nrow(means)
names(means)

ggplot(means, aes(x=Mean1, y=Mean2, color=prior_type,shape=prior_type,fill=prior_type)) +
  geom_errorbar(aes(ymin=YMin2,ymax=YMax2)) +
  geom_errorbarh(aes(xmin=YMin1,xmax=YMax1)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_color_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Exp.~2 mean prior probability") +
  xlab("Exp.~1 mean prior probability") +
  theme(legend.position = "top",legend.text=element_text(size=12)) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/prior-probability-comparison-exp1-exp2.pdf",height=4,width=4)


corr_means = means %>%
  summarize(Cor=cor(Mean1,Mean2,method="spearman"))
corr_means #.977

