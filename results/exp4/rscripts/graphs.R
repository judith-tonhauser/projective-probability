# experiment investigating prior and presupposition
# contents of complements of 20 predicates
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(RColorBrewer)
library(zoo)  # needed for function na_locf() which replaces each NA with the next non-NA 

theme_set(theme_bw())

d = read_csv("../data/data_preprocessed.csv")
nrow(d) #14872 / 52 = 286 turkers

summary(d)
table(d$prior) # half is high_prior, half is low_prior

# prepare for spreading:
# (1) rename the "prior" column into "prior_type" and
colnames(d)[colnames(d)=="prior"] = "prior_type"
# (2) fill in (arbitrarily) 'high_prior' for the "prior_type" of main clauses (MC)
d[d$short_trigger == "MC",]$prior_type <- "high_prior"

# spread responses over separate columns for prior probability and projectivity
head(d)

table(d$block,d$question_type)
#         prior projective
# block1  4056       3380
# block2  3380       4056

cd = d %>%
  mutate(block_proj = ifelse(question_type=="projective"&block=="block1", "block1", 
                             ifelse(question_type=="projective"&block=="block2","block2",
                                    ifelse(question_type=="prior"&block=="block1","block2","block1")))) %>%
  select(content,question_type,short_trigger,response,workerid,prior_type,prior_fact,block_proj) %>% 
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)
head(cd)

table(cd$block_proj) 
# block1 block2 
# 3380   4056

nrow(cd)
#7436 = 286 x 26 rows (each row has prior and projection)


# there's no need to change cd verb names to match veridicality names but it's helpful to
# copy the column "short_trigger" and name it "verb"
cd = cd %>%
  mutate(verb=recode(short_trigger, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# construct 'eventItemNr' using 'prior_fact'
cd = cd %>%
  mutate(eventItemNr = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "20: Charley speaks Spanish",
                        ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "11: Danny ate the last cupcake",
                        ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "8: Emily bought a car yesterday",
                        ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "3: Emma studied on Saturday morning",
                        ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "12: Frank got a cat",
                        ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "9: Grace visited her sister",
                        ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "7: Isabella ate a steak on Sunday",
                        ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "13: Jackson ran 10 miles",
                        ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "14: Jayden rented a car",
                        ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "19: Jon walks to work",
                        ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "16: Josh learned to ride a bike yesterday",
                        ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "2: Josie went on vacation to France",
                        ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "18: Julian dances salsa",
                        ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "1: Mary is pregnant",
                        ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "6: Mia drank 2 cocktails last night",
                        ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "4: Olivia sleeps until noon",
                        ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "17: Owen shoveled snow last winter",
                        ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "5: Sophia got a tattoo",
                        ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "15: Tony had a drink last night",
                        ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "10: Zoe calculated the tip",
                                       NA)))))))))))))))))))))

# construct 'eventItem' using 'prior_fact'
cd = cd %>%
  mutate(eventItem = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "Charley speaks Spanish",
                              ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "Danny ate the last cupcake",
                                     ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "Emily bought a car yesterday",
                                            ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "Emma studied on Saturday morning",
                                                   ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "Frank got a cat",
                                                          ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "Grace visited her sister",
                                                                 ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "Isabella ate a steak on Sunday",
                                                                        ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "Jackson ran 10 miles",
                                                                               ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "Jayden rented a car",
                                                                                      ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "Jon walks to work",
                                                                                             ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "Josh learned to ride a bike yesterday",
                                                                                                    ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "Josie went on vacation to France",
                                                                                                           ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "Julian dances salsa",
                                                                                                                  ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "Mary is pregnant",
                                                                                                                         ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "Mia drank 2 cocktails last night",
                                                                                                                                ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "Olivia sleeps until noon",
                                                                                                                                       ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "Owen shoveled snow last winter",
                                                                                                                                              ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "Sophia got a tattoo",
                                                                                                                                                     ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "Tony had a drink last night",
                                                                                                                                                            ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "Zoe calculated the tip",
                                                                                                                                                                   NA)))))))))))))))))))))

# target data (no main clause content)
t <- droplevels(subset(cd,cd$short_trigger != "MC"))
nrow(t) #5720 / 286 = 20 rows per participant

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

summary(t)
table(t$prior_fact) #40 facts
table(t$eventItem) #20 clauses
str(t$prior)

# plot prior ratings by content
means = t %>%
  group_by(prior_type,eventItem) %>%
  summarise(Mean=mean(prior),CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means

names(means)
table(means$prior_type)

low = means %>%
  filter(prior_type == "low_prior") %>%
  mutate(eventItem = fct_reorder(eventItem,Mean))

means = means %>%
  mutate(eventItem = fct_relevel(eventItem,levels(low$eventItem)))
means

subjmeans = t %>%
  group_by(eventItem,workerid,prior_type) %>%
  summarize(Mean = mean(prior))
subjmeans$eventItem <- factor(subjmeans$eventItem, levels = unique(levels(means$eventItem)))
levels(subjmeans$eventItem)
names(subjmeans)

ggplot(means, aes(x=eventItem, y=Mean, color=prior_type,shape=prior_type,fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.1) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Fact", breaks=c("higher probability","lower probability"),labels=c("higher probability", "lower probability"), 
                     values=cbPalette) +
  theme(legend.position = "top") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean likelihood rating") +
  xlab("Clause") 
ggsave(f="../graphs/prior-ratings.pdf",height=7,width=9)


# plot projection by predicate and prior_type 

# mean projectivity by predicate, with main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
proj.means

# define colors for the predicates
cols = data.frame(V=levels(proj.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

proj.means$VeridicalityGroup = as.factor(
  ifelse(proj.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(proj.means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(proj.means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(proj.means$verb  %in% c("MC"),"MC","V")))))

# plot of means, 95% bootstrapped CIs and participants' ratings
ggplot(proj.means, aes(x=verb, y=Mean, fill=prior_type,shape=prior_type)) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(colour = "black", size = 3) +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower probability","higher probability")),name="Fact") +
  #scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low")) +
  scale_color_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                     values=cbPalette) +  
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  #geom_errorbar(aes(x=4,ymin=proj.means[proj.means$verb == "MC",]$YMin,ymax=proj.means[proj.means$verb == "MC",]$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  #geom_point(aes(x=4,y=proj.means[proj.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=4,width=7)


#### plot projectivity by prior probability on a by-participant level ----

cd$verb <- factor(cd$verb, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
cd$verb

# remove main clauses since they are not relevant here
target <- droplevels(subset(cd,cd$verb != "MC"))
nrow(target)

# relevel for color coding
target$verb <- factor(target$verb, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
target$verb

# no facet wrap, veridicality colors
ggplot(target, aes(x=prior, y=projective, color=verb)) +
  # geom_abline(intercept=0, slope=1, linetype="dashed", color="gray50") +
  geom_smooth(method="lm", color="black") +
  geom_point(pch=20, size=3, show.legend = FALSE) +
  scale_color_manual(breaks=c("F","NF","V","VNF"), labels=c("factive","non-factive","V","VNF"), values=cols$Colors) +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) 
ggsave(f="../graphs/projectivity-by-prior_color-veridicality.pdf",height=8,width=8)

# no facet wrap, individual color for each verb
ggplot(target, aes(x=prior, y=projective, color=verb)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",color="black") +
  geom_point(pch=20, size=3) +
  theme(legend.position = "right") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) 
ggsave(f="../graphs/projectivity-by-prior_color.pdf",height=8,width=11)

# facet wrap: CC
ggplot(target, aes(x=prior,y=projective)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="black") +
  geom_point(size=1) +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~eventItemNr)
ggsave(f="../graphs/projectivity-by-prior_facet-wrap-CC.pdf",height=8,width=10)

# facet wrap: verb
ggplot(target, aes(x=prior, y=projective, fill=verb)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="black",show.legend = FALSE) +
  geom_point(pch=21, size=2, show.legend = FALSE) +
  scale_fill_manual(breaks=c("F","NF","V","VNF"), values=cols$Colors) +
  #scale_color_manual(breaks=c("F","MC","NF","V","VNF"), values=cols$Colors) +  
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~verb)
ggsave(f="../graphs/projectivity-by-prior_facet-wrap-verb.pdf",height=8,width=10)

