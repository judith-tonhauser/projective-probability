# experiment investigating prior and projection
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
d[d$short_trigger == "MC",]$prior_type <- "main clause"

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

# construct 'eventNr' using 'prior_fact'
cd = cd %>%
  mutate(eventNr = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "20",
                              ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "11",
                                     ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "8",
                                            ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "3",
                                                   ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "12",
                                                          ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "9",
                                                                 ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "7",
                                                                        ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "13",
                                                                               ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "14",
                                                                                      ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "19",
                                                                                             ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "16",
                                                                                                    ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "2",
                                                                                                           ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "18",
                                                                                                                  ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "1",
                                                                                                                         ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "6",
                                                                                                                                ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "4",
                                                                                                                                       ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "17",
                                                                                                                                              ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "5",
                                                                                                                                                     ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "15",
                                                                                                                                                            ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "10",
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

# plot of slider ratings to see if linear regression model is justified or whether we need Beta regression
# do this on the target ratings only because that is what the model will be fitted to

# projection 
ggplot(t, aes(x=projective)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/bunching-projection.pdf",width=3.4,height=2)

# prior 
ggplot(t, aes(x=prior)) +
  geom_histogram(bins=50) +
  xlab("Prior probability rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/bunching-prior.pdf",width=3.4,height=2)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

summary(t)
table(t$prior_fact) #40 facts
table(t$eventItem) #20 clauses
str(t$prior)

# Fig 2: prior ratings by content (no main clause content) ----
means = t %>%
  group_by(prior_type,eventItem) %>%
  summarise(Mean=mean(prior),CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means

# save for comparison with Exp2 findings
write.csv(means,file="../data/prior_means.csv",row.names=F,quote=F)

names(means)
table(means$prior_type)

high = means %>%
  filter(prior_type == "high_prior") %>%
  mutate(eventItem = fct_reorder(eventItem,Mean))

means = means %>%
  mutate(eventItem = fct_relevel(eventItem,levels(high$eventItem)))
means

subjmeans = t %>%
  group_by(eventItem,workerid,prior_type) %>%
  summarize(Mean = mean(prior))
subjmeans$eventItem <- factor(subjmeans$eventItem, levels = unique(levels(means$eventItem)))
levels(subjmeans$eventItem)
names(subjmeans)

ggplot(means, aes(x=eventItem, y=Mean, color=prior_type,shape=prior_type,fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Fact", breaks=c("higher probability","lower probability"),labels=c("higher probability", "lower probability"), 
                     values=cbPalette) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean prior probability rating") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings.pdf",height=7,width=8)


# Fig 3: plot projection by prior type collapsing over predicate (with main clause content) ----
nrow(cd)
table(cd$prior_type)

# mean projectivity by predicate and prior type, with main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
proj.means

# define colors for the predicates
# cols = data.frame(V=levels(proj.means$verb))
# 
# cols$VeridicalityGroup = as.factor(
#   ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
#          ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
#                 ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
#                        ifelse(cols$V %in% c("MC"),"MC","V")))))
# 
# levels(cols$V)
# cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
# 
# cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
#                       ifelse(cols$VeridicalityGroup == "NF", "gray60", 
#                              ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
#                                     ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))
# 
# cols$Colors
# cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
# levels(cols$V)
# 
# proj.means$VeridicalityGroup = as.factor(
#   ifelse(proj.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
#          ifelse(proj.means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
#                 ifelse(proj.means$verb  %in% c("be_right","demonstrate"),"VNF",
#                        ifelse(proj.means$verb  %in% c("MC"),"MC","V")))))


# order predicates by high_prior
high = proj.means %>%
  filter(prior_type != "low_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))
high
levels(high$short_trigger)

proj.means = proj.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(high$short_trigger)))
levels(proj.means$short_trigger)

# to plot MC in different color and shape, copy MC data to new data frame and 
# remove MC data, but not factor level, from proj.means
mc.data = droplevels(subset(proj.means, proj.means$verb == "MC"))
mc.data

proj.means[proj.means$short_trigger == "MC",]$Mean <- NA
proj.means[proj.means$short_trigger == "MC",]$YMin <- NA
proj.means[proj.means$short_trigger == "MC",]$YMax <- NA

# to add participants' ratings
subjmeans = cd %>%
  group_by(workerid,short_trigger,prior_type) %>%
  summarize(Mean = mean(projective))
subjmeans$short_trigger <- factor(subjmeans$short_trigger, levels = unique(levels(proj.means$short_trigger)))
levels(subjmeans$short_trigger)
  
levels(proj.means$prior_type)
proj.means$prior_type <- as.factor(proj.means$prior_type)

ggplot(proj.means, aes(x=short_trigger, y=Mean, color=prior_type,fill=prior_type,shape=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(21, 25, 24)),labels=rev(c("main clause","lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("black","#56B4E9","#E69F00")),labels=rev(c("main clause","lower probability","higher probability")),name="Fact") +
  scale_color_manual(values=rev(c("black","#56B4E9","#E69F00")),labels=rev(c("main clause","lower probability","higher probability")),name="Fact") +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_errorbar(aes(x=1,ymin=mc.data$YMin,ymax=mc.data$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(shape=20,size=4,aes(x=1,y=mc.data$Mean),color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=5,width=7)

#### Fig 4: plot projectivity by prior probability on a by-participant level (no MC content) ----
nrow(cd) #7436 (all data, i.e., with main clause content)
nrow(t)  #5720 (target, i.e., without main clause content)

names(t)
table(t$short_trigger)

proj.means = t %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),Mean))  
proj.means

nrow(proj.means) #40 (high_prior and low_prior for each of the 20 predicates)

low = proj.means %>%
  filter(prior_type == "low_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))

t = t %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(low$short_trigger)))

ggplot(t, aes(x=prior, y=projective,color=prior_type)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~short_trigger)
ggsave(f="../graphs/projection-by-prior.pdf",height=7,width=7)

#### For discussion: plot projectivity by prior probability on a by-content level ----
summary(t)

proj.means = t %>%
  group_by(prior_type,content) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, content = fct_reorder(as.factor(content),Mean))  
proj.means

nrow(proj.means) #40 (high_prior and low_prior for each of the 20 predicates)

low = proj.means %>%
  filter(prior_type == "low_prior") %>%
  mutate(content = fct_reorder(content,Mean))

t = t %>%
  mutate(content = fct_relevel(content,levels(low$content)))

ggplot(t, aes(x=prior, y=projective,color=prior_type)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~content)
ggsave(f="../graphs/projection-by-prior-by-content.pdf",height=7,width=7)


## plot comparison and calculate Spearman rank correlation with Exp1a from factives paper ----
## with main clauses

# data from this experiment
summary(cd)
table(cd$short_trigger)

# load data from Exp1a 
exp1a = read.csv("../../5-projectivity-no-fact/data/cd.csv") %>%
  mutate(short_trigger=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform")) 
#%>%
#  filter(short_trigger != "MC") %>%
#  droplevels()
summary(exp1a)
table(exp1a$short_trigger)

# for projectivity data, plot proportions against mean slider ratings
p_thisExp = cd %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),Mean)) %>%
  select(-CILow,-CIHigh)
View(p_thisExp)
levels(p_thisExp$short_trigger)

p_Exp1a = exp1a %>%
  group_by(short_trigger) %>%
  summarize(Mean2 = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin2 = Mean2 - CILow, YMax2 = Mean2 + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),p_thisExp$Mean)) %>%
  select(-CILow,-CIHigh)
View(p_Exp1a)
levels(p_Exp1a$short_trigger)

p = p_thisExp %>%
  left_join(p_Exp1a) %>%
  mutate(VeridicalityGroup = factor(case_when(
    short_trigger %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    short_trigger %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    short_trigger %in% c("be_right","demonstrate") ~ "VNF",
    short_trigger %in% c("MC") ~ "MC",
    TRUE ~ "V")))
View(p)
levels(p$VeridicalityGroup)

p$VeridicalityGroup <- factor(p$VeridicalityGroup, levels =rev(c("F","V","VNF","NF","MC")))

# shape-predicate mapping
# 21: mc
# 22: non-veridical non-factive NF
# 23: factive F
# 24: optionally factive V
# 25: veridical non-factive VNF

# plot this experiment findings (Mean, Ymin, YMax) against Exp1a (Mean2, YMin2, YMax2)

ggplot(p, aes(x=Mean2, y=Mean, fill=VeridicalityGroup,shape=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMin2,xmax=YMax2),width=0) +
  geom_point(stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  #scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),
                     labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),
                    labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean certainty rating in Exp 1") +
  xlab("Mean certainty rating in Tonhauser & Degen's Exp 1a") +
  theme(legend.position = "top",legend.text=element_text(size=12)) +
  theme(text = element_text(size=12)) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/projection-comparison.pdf",height=5,width=5)

corr_projectivity = p %>%
  #filter(verb != "MC") %>%
  summarize(Cor=cor(Mean,Mean2,method="spearman"))
corr_projectivity #.991

## SALT talk: prior plot with contents identified by numbers ----
names(t)
table(t$eventNr)

means = t %>%
  group_by(prior_type,eventNr) %>%
  summarise(Mean=mean(prior),CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means

names(means)
table(means$prior_type)

high = means %>%
  filter(prior_type == "high_prior") %>%
  mutate(eventNr = fct_reorder(eventNr,Mean))

means = means %>%
  mutate(eventNr = fct_relevel(eventNr,levels(high$eventNr)))
means

subjmeans = t %>%
  group_by(eventNr,workerid,prior_type) %>%
  summarize(Mean = mean(prior))
subjmeans$eventNr <- factor(subjmeans$eventNr, levels = unique(levels(means$eventNr)))
levels(subjmeans$eventNr)
names(subjmeans)

ggplot(means, aes(x=eventNr, y=Mean, color=prior_type,shape=prior_type,fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Fact", breaks=c("higher probability","lower probability"),labels=c("higher probability", "lower probability"), 
                     values=cbPalette) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean prior probability rating") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings-content-as-Nr.pdf",height=5,width=8)

### SALT talk: projection plot with color-coded predicates (with MCs) ----
nrow(cd)
table(cd$prior_type)

# mean projectivity by predicate and prior type, with main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
proj.means
levels(proj.means$verb)

# order predicates by high_prior
high = proj.means %>%
  filter(prior_type != "low_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))
high
levels(high$short_trigger)

proj.means = proj.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(high$short_trigger)))
levels(proj.means$short_trigger)

# define colors for the predicates
cols = data.frame(V=levels(proj.means$short_trigger))
cols
str(cols$V) #factor
levels(cols$V) #not ordered by projection

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F",
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF",
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

str(cols$VeridicalityGroup) #factor
levels(cols$VeridicalityGroup) #"F"   "MC"  "NF"  "V"   "VNF"
cols$VeridicalityGroup #right order

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$short_trigger)),]$V, ordered = TRUE)
str(cols$V) #factor
levels(cols$V) #ordered by projection

cols$Colors =  as.factor(
  ifelse(cols$VeridicalityGroup == "F", "darkorchid",
                      ifelse(cols$VeridicalityGroup == "NF", "gray60",
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1")))))

cols$Colors
str(cols$Colors)

proj.means$VeridicalityGroup = as.factor(
  ifelse(proj.means$short_trigger %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F",
         ifelse(proj.means$short_trigger  %in% c("pretend", "think", "suggest", "say"), "NF",
                ifelse(proj.means$short_trigger  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(proj.means$short_trigger  %in% c("MC"),"MC","V")))))


cols$Colors
#cols$V <- factor(cols$V, levels = cols[order(as.character(high$short_trigger)),]$V, ordered = TRUE)
#levels(cols$V)

# to plot MC in different color and shape, copy MC data to new data frame and 
# remove MC data, but not factor level, from proj.means
mc.data = droplevels(subset(proj.means, proj.means$verb == "MC"))
mc.data

proj.means[proj.means$short_trigger == "MC",]$Mean <- NA
proj.means[proj.means$short_trigger == "MC",]$YMin <- NA
proj.means[proj.means$short_trigger == "MC",]$YMax <- NA

# to add participants' ratings
subjmeans = cd %>%
  group_by(workerid,short_trigger,prior_type) %>%
  summarize(Mean = mean(projective))
subjmeans$short_trigger <- factor(subjmeans$short_trigger, levels = unique(levels(proj.means$short_trigger)))
levels(subjmeans$short_trigger)

levels(proj.means$prior_type)
proj.means$prior_type <- as.factor(proj.means$prior_type)

ggplot(proj.means, aes(x=short_trigger, y=Mean, color=prior_type,fill=prior_type,shape=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(21, 25, 24)),labels=rev(c("main clause","lower probability","higher probability")),name="Fact") +
  scale_fill_manual(values=rev(c("black","#56B4E9","#E69F00")),labels=rev(c("main clause","lower probability","higher probability")),name="Fact") +
  scale_color_manual(values=rev(c("black","#56B4E9","#E69F00")),labels=rev(c("main clause","lower probability","higher probability")),name="Fact") +  
  scale_alpha(range = c(.3,1)) +
  # guides(fill=FALSE, shape=F) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors)) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_errorbar(aes(x=1,ymin=mc.data$YMin,ymax=mc.data$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(shape=20,size=4,aes(x=1,y=mc.data$Mean),color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/means-projectivity-by-predicate-and-prior-color-for-preds.pdf",height=4.5,width=7)

#### SALT talk: projectivity by prior for "discover" on a by-participant level ----
nrow(cd) #7436 (all data, i.e., with main clause content)
nrow(t)  #5720 (target, i.e., without main clause content)

names(t)
table(t$short_trigger)

discover <- droplevels(subset(t,t$short_trigger == "discover"))
names(discover)

# correlation coefficient 
cor(discover$projective,discover$prior) #.27

ggplot(disc, aes(x=prior, y=projective,color=prior_type)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=1) +
  scale_color_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 
ggsave(f="../graphs/projection-by-prior-discover.pdf",height=4,width=4)

inform <- droplevels(subset(t,t$short_trigger == "inform"))

# correlation coefficient 
cor(inform$projective,inform$prior) #.31


ggplot(inform, aes(x=prior, y=projective,color=prior_type)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=1) +
  scale_color_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower probability","higher probability")),name="Fact") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 
ggsave(f="../graphs/projection-by-prior-inform.pdf",height=4,width=4)

