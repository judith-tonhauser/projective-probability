# Prior probability work
# 3-projectivity: analysis of experiment that tested projectivity with sensitivity to fact

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(forcats)
library(RColorBrewer)
theme_set(theme_bw())

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #7800 = 300 participants x 26 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #7.1
median(d$Answer.time_in_minutes) #6

d = d %>%
  dplyr::select(workerid,rt,subjectGender,speakerGender,content,verb,fact,fact_type,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes)
nrow(d) #7800

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #0 missing values
table(d$age) #21-72
median(d$age,na.rm=TRUE) #36
table(d$gender)
#145 female, 154 male

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "" & d$language != "Russian" & d$language != "Ukrainian" & d$language != "Arabic" & d$language != "chinese" & d$language != "hungarian" & d$language != "spanish"))
d = droplevels(d)
length(unique(d$workerid)) #292 (8 Turkers excluded)

table(d$gender)
length(which(is.na(d$american))) #78 (3 people didn't respond)
table(d$american) 
# coding error in HTML file: m=yes, f=no
d <- subset(d, d$american == "m")
d = droplevels(d)
length(unique(d$workerid)) #277 (15 Turkers excluded, 23 excluded in total for language reasons)

## exclude Turkers based on 6 controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "control")
c <- droplevels(c)
nrow(c) #1662 / 6 controls = 277 Turkers

# group mean on controls
round(mean(c$response),2) #.21

ggplot(c, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")
ggsave(f="../graphs/raw-responses-to-controls.pdf",height=4,width=6.5)

# group means on each control
means = aggregate(response ~ contentNr, data=c, FUN="mean")
means
# contentNr  response
# 1  control1 0.1877256
# 2  control2 0.1822744
# 3  control3 0.1749458
# 4  control4 0.2440794
# 5  control5 0.1886282
# 6  control6 0.2560289

# Turkers with response means on controls more than 2sd above group mean
# this is the exclusion criterion we decided on for factivity paper
c.means = aggregate(response~workerid, data=c, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=c, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=c, FUN="ci.high")$response
c.means

c.g <- c.means[c.means$response > (mean(c.means$response) + 2*sd(c.means$response)),]
c.g 
unique(length(c.g$workerid)) #11 Turkers gave high response
mean(c.g$response) #.69

outliers <- subset(c, c$workerid %in% c.g$workerid)
outliers = droplevels(outliers)

# look at the responses to the controls that these "outlier" Turkers did

ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
ggsave(f="../graphs/raw-responses-to-controls-by-outliers.pdf",height=6,width=10)

# responses here are supposed to be low but these Turkers
# gave high response to most control items

# exclude the Turker identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #266 Turkers remain (277 - 11)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #6916 / 26 items = 266 participants

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #6916

# load prior means
pmeans = read.csv("../../1-prior/data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)
pmeans
head(pmeans)

# change predicate names
cd = cd %>%
  mutate(verb=recode(verb, annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# merge contradictoriness means into cd
# cd = left_join(cd,vmeans,by=c("verb"))
cd = left_join(cd,pmeans,by=c("fact"))

head(cd)


# mean of non-projecting controls
table(cd$verb)
mean(cd[cd$verb == "control",]$response) #.19
ci.low(cd[cd$verb == "control",]$response) #.19
ci.high(cd[cd$verb == "control",]$response) #.01

# age info of remaining Turkers
table(cd$age) #21-72
length(which(is.na(cd$age))) #0 missing values
median(cd$age,na.rm=TRUE) #36
table(cd$gender)
#129 female, 136 male

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "control")
t <- droplevels(t)
nrow(t) #5320 / 20 items = 266 Turkers
table(t$verb,t$content)

names(t)
table(t$trigger_class)

head(t)

names(t)
table(t$itemType)
# mean rating across predicates by fact type
mean(t[t$itemType == "factL",]$response) #.32
mean(t[t$itemType == "factH",]$response) #.5

# plot mean projectivity by complement prior ----
means = t %>%
  group_by(verb, fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means
levels(means$Verb)

cols = data.frame(V=levels(means$Verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","PNF")))))

levels(cols$V)
cols

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols
cols$V <- factor(cols$V, levels = cols[order(as.character(means$Verb)),]$V, ordered = TRUE)
levels(cols$V)

#cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#009E73", 
#                      ifelse(cols$VeridicalityGroup == "NF", "black", 
#                             ifelse(cols$VeridicalityGroup == "VNF","black","black")))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

# xprag abstract
ggplot(means, aes(x=Verb, y=Mean, color=fact_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=cbPalette) +
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = c(0.2, 0.8)) +
  ylab("Mean certainty rating \n (higher is more projective)") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=3.2,width=6)

# xprag talk 
ggplot(means, aes(x=Verb, y=Mean, color=fact_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=cbPalette) +
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = c(0.2, 0.75)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=3.2,width=6)

# xprag talk with main clause controls
table(cd$verb)

means = cd %>%
  group_by(verb, fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means
levels(means$Verb)

means[means$Verb == "control",]

# means_subj = cd %>%
#   group_by(verb,fact_type,workerid) %>%
#   summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
#   ungroup() %>%
#   mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))

cols = data.frame(V=levels(means$Verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("control"),"MC","PNF")))))

levels(cols$V)
cols

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols
cols$V <- factor(cols$V, levels = cols[order(as.character(means$Verb)),]$V, ordered = TRUE)
levels(cols$V)

#cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#009E73", 
#                      ifelse(cols$VeridicalityGroup == "NF", "black", 
#                             ifelse(cols$VeridicalityGroup == "VNF","black","black")))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",


ggplot(means, aes(x=Verb, y=Mean, fill=fact_type)) + 
  # geom_point(data=means_subj,aes(color=fact_type),alpha=.3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(pch = 21, colour = "black", size = 3) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=cbPalette) +
  scale_color_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                    values=cbPalette) +  
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  geom_errorbar(aes(x=1,ymin=means[means$Verb == "control",]$YMin,ymax=means[means$Verb == "control",]$YMax,width=.25),color="black") +
  geom_point(aes(x=1,y=means[means$Verb == "control",]$Mean), color="black",show.legend = FALSE ) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=4,width=6)

# models ----
library(lmerTest)

# model on full data, with controls as reference level
names(cd)

str(cd$response)
str(cd$verb)
cd$verb <- as.factor(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)

# set reference levels
cd$itemType <- relevel(cd$itemType, ref="factL")
cd$verb <- relevel(cd$verb, ref="control")

# make item variable
cd$item = as.factor(paste(cd$verb, cd$content, cd$fact))
table(cd$item)

contrasts(cd$verb)

# no interaction
model.1 = lmer(response ~ itemType + verb + (1+itemType|workerid) + (1|item), data=cd, REML=F)
summary(model.1)

# with interaction
model.2 = lmer(response ~ itemType * verb + (1+itemType|workerid) + (1|item), data=cd, REML=F)
summary(model)

anova(model.1,model.2) # model with interaction is not better


# model only on target data, with pretend, low prior as reference level
names(t)
table(t$verb)
table(t$content)
table(t$itemType)
table(t$fact)



str(t$response)
str(t$verb)
t$verb <- as.factor(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)

# set reference levels
t$itemType <- relevel(t$itemType, ref="factL")
t$verb <- relevel(t$verb, ref="pretend")

# make item variable
t$item = as.factor(paste(t$verb, t$content, t$fact))
table(t$item)

contrasts(t$verb)

# no interaction
model.1 = lmer(response ~ itemType + verb + (1+itemType|workerid) + (1|item), data=t, REML=F)
summary(model.1)

# with interaction
model.2 = lmer(response ~ itemType * verb + (1+itemType|workerid) + (1|item), data=t, REML=F)
summary(model)

anova(model.1,model.2)


# models for xprag talk  ----

# first model: testing for effect of prior
# results: big main effect of prior, no interactions; this means: effect of prior regardless of verb
t = droplevels(t)
t$item = as.factor(paste(t$verb, t$content,t$fact))
m = lmer(response ~ PriorMean*verb + (1+PriorMean|workerid) + (1|item), data=t)
# adding by-participant slope for "verb" doesn't work
summary(m)

m.main = lmer(response ~ PriorMean+verb + (1+PriorMean|workerid) + (1|item), data=t)
summary(m.main)

anova(m.main,m) # interaction model not better

# second model: testing for projectivity compared to control
# results: almost all verb/fact combinations are significantly different from the control, suggesting they are all more projective than the controls. none of the what look like anti-projective cases in the plot are actually different from the control
cd$verbfact = as.factor(paste(cd$verb,cd$fact_type))
table(cd$verbfact)
cd$item = as.factor(paste(cd$verb, cd$content,cd$fact))

cd = cd %>%
  mutate(verbfact = fct_relevel(verbfact,"control NA"))
m = lmer(response ~ verbfact + (1|workerid) + (1|item), data=cd)
summary(m)

### comparing projection with and without prior probability manipulation ----

nrow(cd) #6919
table(cd$verb)

# load data from projection experiment without manipulation
cd.noPrior = read.csv("../../5-projectivity-no-fact/data/cd.csv")
nrow(cd.noPrior) #6919 (I double-checked: yes, it is really the same)
table(cd.noPrior$verb)
cd.noPrior = cd.noPrior %>%
  mutate(verb=recode(verb, control = "control", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# plot mean slider ratings against mean slider ratings
p_means_Prior = cd %>%
  group_by(verb) %>%
  summarize(MeanP = mean(response), CILowP = ci.low(response), CIHighP = ci.high(response)) %>%
  mutate(YMinP = MeanP - CILowP, YMaxP = MeanP + CIHighP) %>%
  dplyr::select(-CILowP,-CIHighP)
levels(p_means_Prior$verb)
p_means_Prior

p_means_noPrior = cd.noPrior %>%
  group_by(verb) %>%
  summarize(MeanNP = mean(response), CILowNP = ci.low(response), CIHighNP = ci.high(response)) %>%
  mutate(YMinNP = MeanNP - CILowNP, YMaxNP = MeanNP + CIHighNP) %>%
  dplyr::select(-CILowNP,-CIHighNP)
levels(p_means_noPrior$verb)
p_means_noPrior

p_comp = p_means_Prior %>%
  left_join(p_means_noPrior) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("control") ~ "control",
    TRUE ~ "PNF")))
p_comp
levels(p_comp$verb)

p_comp = p_comp %>%
  mutate(verb = fct_reorder(verb,MeanP))
p_comp
levels(p_comp$verb)
levels(p_comp$VeridicalityGroup)

ggplot(p_comp, aes(x=MeanP, y=MeanNP, fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMinNP,ymax=YMaxNP),width=0) +
  geom_errorbarh(aes(xmin=YMinP,xmax=YMaxP),width=0) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue","black")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean certainty rating (no prior)") +
  xlab("Mean certainty rating (with prior)") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/projectivity-comparison.pdf",height=3,width=3)

corr_proj = p_comp %>%
  filter(verb != "control") %>%
  summarize(Cor=cor(MeanP,MeanNP,method="spearman"))
corr_proj

################### nothing below here relevant for XPRAG abstract #########


# projectivity of complement of "discover" by complement prior
discover <- droplevels(subset(cd, cd$verb == "discover"))
nrow(discover) #252 = 1 rating per 252 Turkers
names(discover)
table(discover$content) #7-19 ratings per complement (20 complements)
head(discover)
discover$EventFact <- paste(discover$event, discover$fact_type, sep="-")
head(discover)
table(discover$EventFact) #2-10 ratings per complement/fact pairing
head(table)

str(discover$EventFact)
discover$EventFact <- as.factor(discover$EventFact)

means = discover %>%
  group_by(EventFact,PriorMean) %>%
  summarize(ProjMean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = ProjMean - CILow, YMax = ProjMean + CIHigh)
  #mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Compl = fct_reorder(Mean))
View(means)

means$EventFact <- factor(means$EventFact, levels=means[order(means$PriorMean), "EventFact"])

model <- lm(ProjMean ~ PriorMean, data = means)
summary(model)

ggplot(means, aes(x=PriorMean, y=ProjMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title = paste("Adj R2 = ",signif(summary(model)$adj.r.squared, 2),
                     #"Intercept =",signif(model$coef[[1]],5 ),
                     #" Slope =",signif(model$coef[[2]], 5),
                     ", p <",".001")) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_alpha(range = c(.3,1)) +
  #theme(legend.position="top") +
  ylab("Mean certainty rating of complement/fact") +
  xlab("Mean prior probability of event/fact (discover)") +
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/discover-mean-projectivity-by-mean-prior.pdf",height=4,width=7)

# mean projectivity of the predicates by verb
means = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))

t$verb <-factor(t$verb, levels=levels(means$Verb))

ggplot(t, aes(x=Verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Certainty rating")+
  xlab("Predicate")
ggsave("../graphs/boxplot-projectivity.pdf",height=4,width=6.5)

# boxplot for MIT 2018 talk 
# higher event probability only (to show by-predicate and within-predicate variability)
# mean projectivity of the predicates by verb
names(t)
head(t)
table(t$itemType)
table(t$verb)

tH <- subset(t, t$itemType == "factH")
tH <- droplevels(tH)
nrow(tH)
head(tH)
table(tH$verb)

means = tH %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
View(means)

# groups of verbs
# F (factive): know, discover, reveal, see, be_annoyed
# NF (non-factive/non-veridical): pretend, think, suggest, say, hear
# VNF (veridical non-factive): be_right, demonstrate, establish
# V (apparently veridical/factive): else
cols = data.frame(V=levels(tH$verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say", "hear"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate","establish"),"VNF","V"))))
#cols$Colors =  ifelse(cols$VeridicalityGroup == "E", brewer.pal(3,"Paired")[2], ifelse(cols$VeridicalityGroup == "NE", brewer.pal(3,"Paired")[1],brewer.pal(3,"Paired")[3]))
cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "blue", 
                      ifelse(cols$VeridicalityGroup == "NF", "brown", 
                             ifelse(cols$VeridicalityGroup == "VNF","cornflowerblue","black")))

tH$verb <-factor(tH$verb, levels=levels(means$Verb))

ggplot(tH, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Certainty rating")+
  xlab("Predicate") 
ggsave("../graphs/boxplot-projectivity-factH.pdf",height=3.5,width=8)

# mean projectivity by mean veridicality
head(t)
means = t %>%
  group_by(verb, VeridicalityMean) %>%
  summarize(ProjectionMean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = ProjectionMean - CILow, YMax = ProjectionMean + CIHigh, Verb = fct_reorder(verb,ProjectionMean))
View(means)

cols = data.frame(V=levels(means$Verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed", "hear"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say", "hear"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF","V"))))
#cols$Colors =  ifelse(cols$VeridicalityGroup == "E", brewer.pal(3,"Paired")[2], ifelse(cols$VeridicalityGroup == "NE", brewer.pal(3,"Paired")[1],brewer.pal(3,"Paired")[3]))
cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "blue", 
                      ifelse(cols$VeridicalityGroup == "NF", "brown", 
                             ifelse(cols$VeridicalityGroup == "VNF","cornflowerblue","black")))


ggplot(means, aes(x=VeridicalityMean, y=ProjectionMean))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_text(aes(label=Verb),hjust=.5, vjust=1) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_color_manual(name="Prior probability\nof content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     #values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  #theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Mean contradictoriness rating") +
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/mean-projectivity-by-mean-contradictoriness.pdf",height=4,width=7)


means = means %>%
  mutate(Verb = fct_reorder(verb,VeridicalityMean))
means

ggplot(means, aes(x=Verb, y=Mean, color=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point(aes(shape = fact_type)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/boxplot-projectivity-by-predicate-and-facttype-and veridicality.pdf",height=4,width=6.5)


means = t %>%
  group_by(verb, fact_type, content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means

ggplot(means, aes(x=Verb, y=Mean, color=content)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  # geom_line(aes(group=content)) +
  geom_smooth(aes(group=content),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  facet_wrap(~fact_type)
ggsave("../graphs/boxplot-projectivity-by-predicate-and-content.pdf",height=4,width=6.5)

means = t %>%
  group_by(verb, fact_type, content, PriorMean) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(verb,Mean))
means

ggplot(means, aes(x=PriorMean, y=Mean, color=verb)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  # geom_line(aes(group=content)) +
  geom_smooth(aes(group=verb),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Prior event probability") 
ggsave("../graphs/means-projectivity-by-predicate-content-prior.pdf",height=5,width=6.5)

means = t %>%
  mutate(VeridicalityGroup = cut_number(VeridicalityMean,3,labels=c("low","mid","high"))) %>%
  group_by(verb, fact_type, content, PriorMean, VeridicalityGroup) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
means

ggplot(means, aes(x=PriorMean, y=Mean, color=VeridicalityGroup)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  # geom_line(aes(group=content)) +
  geom_smooth(aes(group=VeridicalityGroup),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Prior event probability") 
ggsave("../graphs/means-projectivity-by-predicate-veridicality-prior.pdf",height=5,width=6.5)

means = t %>%
  group_by(PriorMean,fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
means

ggplot(means, aes(x=PriorMean, y=Mean, color=fact_type)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
  geom_smooth(method="lm") +
  # geom_line(aes(group=content)) +
  # geom_smooth(aes(group=VeridicalityGroup),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  xlim(0.05,.9) +
  ylim(0.05,.9) +
  ylab("Mean certainty rating") +
  xlab("Prior event probability") 
ggsave("../graphs/means-projectivity-by-prior.pdf",height=4,width=5.5)

### PAIRWISE DIFFERENCES ###
library(lsmeans)
library(lme4)
str(t$response)
str(t$verb)
t$verb <- as.factor(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
model = lmer(response ~ verb + (1|workerid), data=t, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                      estimate         SE   df t.ratio p.value
# pretend - be_right        -0.022658730 0.02715554 4788  -0.834  1.0000
# pretend - suggest         -0.044523810 0.02715554 4788  -1.640  0.9861
# pretend - say             -0.046309524 0.02715554 4788  -1.705  0.9786
# pretend - prove           -0.066388889 0.02715554 4788  -2.445  0.6271
# pretend - think           -0.076230159 0.02715554 4788  -2.807  0.3509
# pretend - confirm         -0.081269841 0.02715554 4788  -2.993  0.2357
# pretend - establish       -0.086031746 0.02715554 4788  -3.168  0.1526
# pretend - demonstrate     -0.169404762 0.02715554 4788  -6.238  <.0001
# pretend - announce        -0.217301587 0.02715554 4788  -8.002  <.0001
# pretend - confess         -0.231031746 0.02715554 4788  -8.508  <.0001
# pretend - reveal          -0.249126984 0.02715554 4788  -9.174  <.0001
# pretend - admit           -0.262222222 0.02715554 4788  -9.656  <.0001
# pretend - acknowledge     -0.285158730 0.02715554 4788 -10.501  <.0001
# pretend - discover        -0.325317460 0.02715554 4788 -11.980  <.0001
# pretend - hear            -0.329285714 0.02715554 4788 -12.126  <.0001
# pretend - see             -0.364007937 0.02715554 4788 -13.405  <.0001
# pretend - inform          -0.384960317 0.02715554 4788 -14.176  <.0001
# pretend - know            -0.420000000 0.02715554 4788 -15.466  <.0001
# pretend - be_annoyed      -0.429801587 0.02715554 4788 -15.827  <.0001
# be_right - suggest        -0.021865079 0.02715554 4788  -0.805  1.0000
# be_right - say            -0.023650794 0.02715554 4788  -0.871  1.0000
# be_right - prove          -0.043730159 0.02715554 4788  -1.610  0.9886
# be_right - think          -0.053571429 0.02715554 4788  -1.973  0.9136
# be_right - confirm        -0.058611111 0.02715554 4788  -2.158  0.8255
# be_right - establish      -0.063373016 0.02715554 4788  -2.334  0.7107
# be_right - demonstrate    -0.146746032 0.02715554 4788  -5.404  <.0001
# be_right - announce       -0.194642857 0.02715554 4788  -7.168  <.0001
# be_right - confess        -0.208373016 0.02715554 4788  -7.673  <.0001
# be_right - reveal         -0.226468254 0.02715554 4788  -8.340  <.0001
# be_right - admit          -0.239563492 0.02715554 4788  -8.822  <.0001
# be_right - acknowledge    -0.262500000 0.02715554 4788  -9.667  <.0001
# be_right - discover       -0.302658730 0.02715554 4788 -11.145  <.0001
# be_right - hear           -0.306626984 0.02715554 4788 -11.292  <.0001
# be_right - see            -0.341349206 0.02715554 4788 -12.570  <.0001
# be_right - inform         -0.362301587 0.02715554 4788 -13.342  <.0001
# be_right - know           -0.397341270 0.02715554 4788 -14.632  <.0001
# be_right - be_annoyed     -0.407142857 0.02715554 4788 -14.993  <.0001
# suggest - say             -0.001785714 0.02715554 4788  -0.066  1.0000
# suggest - prove           -0.021865079 0.02715554 4788  -0.805  1.0000
# suggest - think           -0.031706349 0.02715554 4788  -1.168  0.9998
# suggest - confirm         -0.036746032 0.02715554 4788  -1.353  0.9987
# suggest - establish       -0.041507937 0.02715554 4788  -1.529  0.9938
# suggest - demonstrate     -0.124880952 0.02715554 4788  -4.599  0.0008
# suggest - announce        -0.172777778 0.02715554 4788  -6.363  <.0001
# suggest - confess         -0.186507937 0.02715554 4788  -6.868  <.0001
# suggest - reveal          -0.204603175 0.02715554 4788  -7.534  <.0001
# suggest - admit           -0.217698413 0.02715554 4788  -8.017  <.0001
# suggest - acknowledge     -0.240634921 0.02715554 4788  -8.861  <.0001
# suggest - discover        -0.280793651 0.02715554 4788 -10.340  <.0001
# suggest - hear            -0.284761905 0.02715554 4788 -10.486  <.0001
# suggest - see             -0.319484127 0.02715554 4788 -11.765  <.0001
# suggest - inform          -0.340436508 0.02715554 4788 -12.537  <.0001
# suggest - know            -0.375476190 0.02715554 4788 -13.827  <.0001
# suggest - be_annoyed      -0.385277778 0.02715554 4788 -14.188  <.0001
# say - prove               -0.020079365 0.02715554 4788  -0.739  1.0000
# say - think               -0.029920635 0.02715554 4788  -1.102  0.9999
# say - confirm             -0.034960317 0.02715554 4788  -1.287  0.9993
# say - establish           -0.039722222 0.02715554 4788  -1.463  0.9964
# say - demonstrate         -0.123095238 0.02715554 4788  -4.533  0.0010
# say - announce            -0.170992063 0.02715554 4788  -6.297  <.0001
# say - confess             -0.184722222 0.02715554 4788  -6.802  <.0001
# say - reveal              -0.202817460 0.02715554 4788  -7.469  <.0001
# say - admit               -0.215912698 0.02715554 4788  -7.951  <.0001
# say - acknowledge         -0.238849206 0.02715554 4788  -8.796  <.0001
# say - discover            -0.279007937 0.02715554 4788 -10.274  <.0001
# say - hear                -0.282976190 0.02715554 4788 -10.421  <.0001
# say - see                 -0.317698413 0.02715554 4788 -11.699  <.0001
# say - inform              -0.338650794 0.02715554 4788 -12.471  <.0001
# say - know                -0.373690476 0.02715554 4788 -13.761  <.0001
# say - be_annoyed          -0.383492063 0.02715554 4788 -14.122  <.0001
# prove - think             -0.009841270 0.02715554 4788  -0.362  1.0000
# prove - confirm           -0.014880952 0.02715554 4788  -0.548  1.0000
# prove - establish         -0.019642857 0.02715554 4788  -0.723  1.0000
# prove - demonstrate       -0.103015873 0.02715554 4788  -3.794  0.0213
# prove - announce          -0.150912698 0.02715554 4788  -5.557  <.0001
# prove - confess           -0.164642857 0.02715554 4788  -6.063  <.0001
# prove - reveal            -0.182738095 0.02715554 4788  -6.729  <.0001
# prove - admit             -0.195833333 0.02715554 4788  -7.212  <.0001
# prove - acknowledge       -0.218769841 0.02715554 4788  -8.056  <.0001
# prove - discover          -0.258928571 0.02715554 4788  -9.535  <.0001
# prove - hear              -0.262896825 0.02715554 4788  -9.681  <.0001
# prove - see               -0.297619048 0.02715554 4788 -10.960  <.0001
# prove - inform            -0.318571429 0.02715554 4788 -11.731  <.0001
# prove - know              -0.353611111 0.02715554 4788 -13.022  <.0001
# prove - be_annoyed        -0.363412698 0.02715554 4788 -13.383  <.0001
# think - confirm           -0.005039683 0.02715554 4788  -0.186  1.0000
# think - establish         -0.009801587 0.02715554 4788  -0.361  1.0000
# think - demonstrate       -0.093174603 0.02715554 4788  -3.431  0.0719
# think - announce          -0.141071429 0.02715554 4788  -5.195  <.0001
# think - confess           -0.154801587 0.02715554 4788  -5.701  <.0001
# think - reveal            -0.172896825 0.02715554 4788  -6.367  <.0001
# think - admit             -0.185992063 0.02715554 4788  -6.849  <.0001
# think - acknowledge       -0.208928571 0.02715554 4788  -7.694  <.0001
# think - discover          -0.249087302 0.02715554 4788  -9.173  <.0001
# think - hear              -0.253055556 0.02715554 4788  -9.319  <.0001
# think - see               -0.287777778 0.02715554 4788 -10.597  <.0001
# think - inform            -0.308730159 0.02715554 4788 -11.369  <.0001
# think - know              -0.343769841 0.02715554 4788 -12.659  <.0001
# think - be_annoyed        -0.353571429 0.02715554 4788 -13.020  <.0001
# confirm - establish       -0.004761905 0.02715554 4788  -0.175  1.0000
# confirm - demonstrate     -0.088134921 0.02715554 4788  -3.246  0.1238
# confirm - announce        -0.136031746 0.02715554 4788  -5.009  0.0001
# confirm - confess         -0.149761905 0.02715554 4788  -5.515  <.0001
# confirm - reveal          -0.167857143 0.02715554 4788  -6.181  <.0001
# confirm - admit           -0.180952381 0.02715554 4788  -6.664  <.0001
# confirm - acknowledge     -0.203888889 0.02715554 4788  -7.508  <.0001
# confirm - discover        -0.244047619 0.02715554 4788  -8.987  <.0001
# confirm - hear            -0.248015873 0.02715554 4788  -9.133  <.0001
# confirm - see             -0.282738095 0.02715554 4788 -10.412  <.0001
# confirm - inform          -0.303690476 0.02715554 4788 -11.183  <.0001
# confirm - know            -0.338730159 0.02715554 4788 -12.474  <.0001
# confirm - be_annoyed      -0.348531746 0.02715554 4788 -12.835  <.0001
# establish - demonstrate   -0.083373016 0.02715554 4788  -3.070  0.1959
# establish - announce      -0.131269841 0.02715554 4788  -4.834  0.0002
# establish - confess       -0.145000000 0.02715554 4788  -5.340  <.0001
# establish - reveal        -0.163095238 0.02715554 4788  -6.006  <.0001
# establish - admit         -0.176190476 0.02715554 4788  -6.488  <.0001
# establish - acknowledge   -0.199126984 0.02715554 4788  -7.333  <.0001
# establish - discover      -0.239285714 0.02715554 4788  -8.812  <.0001
# establish - hear          -0.243253968 0.02715554 4788  -8.958  <.0001
# establish - see           -0.277976190 0.02715554 4788 -10.236  <.0001
# establish - inform        -0.298928571 0.02715554 4788 -11.008  <.0001
# establish - know          -0.333968254 0.02715554 4788 -12.298  <.0001
# establish - be_annoyed    -0.343769841 0.02715554 4788 -12.659  <.0001
# demonstrate - announce    -0.047896825 0.02715554 4788  -1.764  0.9697
# demonstrate - confess     -0.061626984 0.02715554 4788  -2.269  0.7558
# demonstrate - reveal      -0.079722222 0.02715554 4788  -2.936  0.2682
# demonstrate - admit       -0.092817460 0.02715554 4788  -3.418  0.0749
# demonstrate - acknowledge -0.115753968 0.02715554 4788  -4.263  0.0034
# demonstrate - discover    -0.155912698 0.02715554 4788  -5.741  <.0001
# demonstrate - hear        -0.159880952 0.02715554 4788  -5.888  <.0001
# demonstrate - see         -0.194603175 0.02715554 4788  -7.166  <.0001
# demonstrate - inform      -0.215555556 0.02715554 4788  -7.938  <.0001
# demonstrate - know        -0.250595238 0.02715554 4788  -9.228  <.0001
# demonstrate - be_annoyed  -0.260396825 0.02715554 4788  -9.589  <.0001
# announce - confess        -0.013730159 0.02715554 4788  -0.506  1.0000
# announce - reveal         -0.031825397 0.02715554 4788  -1.172  0.9998
# announce - admit          -0.044920635 0.02715554 4788  -1.654  0.9846
# announce - acknowledge    -0.067857143 0.02715554 4788  -2.499  0.5848
# announce - discover       -0.108015873 0.02715554 4788  -3.978  0.0107
# announce - hear           -0.111984127 0.02715554 4788  -4.124  0.0060
# announce - see            -0.146706349 0.02715554 4788  -5.402  <.0001
# announce - inform         -0.167658730 0.02715554 4788  -6.174  <.0001
# announce - know           -0.202698413 0.02715554 4788  -7.464  <.0001
# announce - be_annoyed     -0.212500000 0.02715554 4788  -7.825  <.0001
# confess - reveal          -0.018095238 0.02715554 4788  -0.666  1.0000
# confess - admit           -0.031190476 0.02715554 4788  -1.149  0.9999
# confess - acknowledge     -0.054126984 0.02715554 4788  -1.993  0.9057
# confess - discover        -0.094285714 0.02715554 4788  -3.472  0.0633
# confess - hear            -0.098253968 0.02715554 4788  -3.618  0.0393
# confess - see             -0.132976190 0.02715554 4788  -4.897  0.0002
# confess - inform          -0.153928571 0.02715554 4788  -5.668  <.0001
# confess - know            -0.188968254 0.02715554 4788  -6.959  <.0001
# confess - be_annoyed      -0.198769841 0.02715554 4788  -7.320  <.0001
# reveal - admit            -0.013095238 0.02715554 4788  -0.482  1.0000
# reveal - acknowledge      -0.036031746 0.02715554 4788  -1.327  0.9990
# reveal - discover         -0.076190476 0.02715554 4788  -2.806  0.3519
# reveal - hear             -0.080158730 0.02715554 4788  -2.952  0.2588
# reveal - see              -0.114880952 0.02715554 4788  -4.230  0.0039
# reveal - inform           -0.135833333 0.02715554 4788  -5.002  0.0001
# reveal - know             -0.170873016 0.02715554 4788  -6.292  <.0001
# reveal - be_annoyed       -0.180674603 0.02715554 4788  -6.653  <.0001
# admit - acknowledge       -0.022936508 0.02715554 4788  -0.845  1.0000
# admit - discover          -0.063095238 0.02715554 4788  -2.323  0.7181
# admit - hear              -0.067063492 0.02715554 4788  -2.470  0.6078
# admit - see               -0.101785714 0.02715554 4788  -3.748  0.0251
# admit - inform            -0.122738095 0.02715554 4788  -4.520  0.0011
# admit - know              -0.157777778 0.02715554 4788  -5.810  <.0001
# admit - be_annoyed        -0.167579365 0.02715554 4788  -6.171  <.0001
# acknowledge - discover    -0.040158730 0.02715554 4788  -1.479  0.9959
# acknowledge - hear        -0.044126984 0.02715554 4788  -1.625  0.9874
# acknowledge - see         -0.078849206 0.02715554 4788  -2.904  0.2877
# acknowledge - inform      -0.099801587 0.02715554 4788  -3.675  0.0324
# acknowledge - know        -0.134841270 0.02715554 4788  -4.966  0.0001
# acknowledge - be_annoyed  -0.144642857 0.02715554 4788  -5.326  <.0001
# discover - hear           -0.003968254 0.02715554 4788  -0.146  1.0000
# discover - see            -0.038690476 0.02715554 4788  -1.425  0.9974
# discover - inform         -0.059642857 0.02715554 4788  -2.196  0.8030
# discover - know           -0.094682540 0.02715554 4788  -3.487  0.0605
# discover - be_annoyed     -0.104484127 0.02715554 4788  -3.848  0.0175
# hear - see                -0.034722222 0.02715554 4788  -1.279  0.9994
# hear - inform             -0.055674603 0.02715554 4788  -2.050  0.8814
# hear - know               -0.090714286 0.02715554 4788  -3.341  0.0944
# hear - be_annoyed         -0.100515873 0.02715554 4788  -3.701  0.0296
# see - inform              -0.020952381 0.02715554 4788  -0.772  1.0000
# see - know                -0.055992063 0.02715554 4788  -2.062  0.8760
# see - be_annoyed          -0.065793651 0.02715554 4788  -2.423  0.6440
# inform - know             -0.035039683 0.02715554 4788  -1.290  0.9993
# inform - be_annoyed       -0.044841270 0.02715554 4788  -1.651  0.9849
# know - be_annoyed         -0.009801587 0.02715554 4788  -0.361  1.0000


## EXPLORATORY SOCIO ANALYSIS (AGE, GENDER)
# age: age of participant
# gender: gender of participant
# speakerGender: gender of speaker of utterance
# subjectGender: gender of subject of attitude predicate

# certainty rating by predicate, fact and speaker gender
means = t %>%
  group_by(verb, fact_type, VeridicalityMean, speakerGender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))

cols = data.frame(V=levels(means$Verb))
cols$VeridicalityGroup = as.factor(ifelse(cols$V %in% c("be_annoyed", "know", "discover", "reveal", "see", "establish", "be_right"), "E", ifelse(cols$V %in% c("pretend", "think", "suggest", "say", "hear"), "NE", "V")))
#cols$Colors =  ifelse(cols$VeridicalityGroup == "E", brewer.pal(3,"Paired")[2], ifelse(cols$VeridicalityGroup == "NE", brewer.pal(3,"Paired")[1],brewer.pal(3,"Paired")[3]))
cols$Colors =  ifelse(cols$VeridicalityGroup == "E", "blue", 
                      ifelse(cols$VeridicalityGroup == "NE", "brown", "green"))

ggplot(means, aes(x=Verb, y=Mean, color=fact_type, shape=speakerGender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/speakergender.pdf")

# certainty rating by fact and speaker gender and participant gender
means = t %>%
  group_by(fact_type, speakerGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=fact_type, fill=speakerGender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Fact type") +
  facet_wrap(~gender) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/speakergender-collapsed.pdf")

# certainty rating by fact and subject gender and participant gender
means = t %>%
  group_by(fact_type, subjectGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=fact_type, fill=subjectGender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Fact type") +
  facet_wrap(~gender) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/subjectgender-collapsed.pdf")

# certainty rating by speaker gender, subject gender and participant gender
means = t %>%
  group_by(fact_type, subjectGender, speakerGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=fact_type, fill=gender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Fact type") +
  facet_grid(speakerGender~subjectGender) + 
  ggtitle("Rows: speaker gender -- Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectandspeakergender-collapsed.pdf")

# certainty rating by fact (continuous), speaker gender, subject gender and participant gender
means = t %>%
  group_by(PriorMean, subjectGender, speakerGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=PriorMean, color=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  geom_smooth(method="lm") +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Prior probability of eventuality") +
  facet_grid(speakerGender~subjectGender) + 
  ggtitle("Rows: speaker gender -- Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectandspeakergender-collapsed-continuuous.pdf")

# certainty rating by subject gender and participant gender
means = t %>%
  group_by(PriorMean, subjectGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=PriorMean, color=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  geom_smooth(method="lm") +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Prior probability of eventuality") +
  facet_wrap(~subjectGender) + 
  ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectgender-collapsed-continuuous.pdf")

# certainty rating by age (3 intervals), subject gender and participant gender
means = t %>%
  mutate(BinnedAge = cut_interval(age, n=3)) %>%
  group_by(fact_type, subjectGender, gender, BinnedAge) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=BinnedAge, color=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Participant age") +
  facet_grid(fact_type~subjectGender) + 
  ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectgenderage-collapsed-continuuous.pdf")

# certainty rating by age (continous)
means = t %>%
  group_by(age) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) #%>%
  #filter(!is.na(gender)) %>%
  #droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=age))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  geom_smooth(method='lm') +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Participant age") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/age-continuuous-collapsed.pdf")

# certainty rating by participant gender
means = t %>%
  group_by(gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Participant gender") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/gender-collapsed.pdf")


### MIXED EFFECTS ANALYSIS ###
library(lme4)
library(languageR)
centered = cbind(t, myCenter(t[,c("fact_type","VeridicalityMean","PriorMean")]))
contrasts(t$fact_type)

# analysis with categorical fact type (dispreferred because of information loss in treating prior probability as simply "high" vs "low")
m = lmer(response ~ cfact_type * cVeridicalityMean + (1+cfact_type|workerid) + (1+cVeridicalityMean|content) + (1+cfact_type|verb),data=centered)
summary(m) # main effects of fact type, but no veridicality effect nor interaction (with this random effects structure -- without random verb effects, there's a veridicality effect)
table(t$fact_type, t$verb)

ranef(m)

# analysis with continuous prior probability of eventuality (reported in SALT paper)
library(lmerTest)
m = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1+cPriorMean|content) + (1+cPriorMean|verb) + (1|fact),data=centered)
summary(m) 

# analysis that doesn't include prior slope for content (JT, January 2018)
m2 = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1|content) + (1+cPriorMean|verb) + (1|fact),data=centered)
summary(m2)

### now lets look at projective contents only (also reported in SALT paper)
p = droplevels(subset(t,t$verb != "pretend" & t$verb != "be_right" & t$verb != "suggest" & t$verb != "say" & t$verb != "prove" & t$verb != "think" & t$verb != "confirm" & t$verb != "establish" & t$verb != "demonstrate"))
nrow(p) #2783 / 11 projective predicates = 253 Turkers
table(p$verb)
str(p$verb)
str(p$response)
table(p$content)

cp = cbind(p, myCenter(p[,c("fact_type","VeridicalityMean","PriorMean")]))
contrasts(cp$fact_type)
head(cp)

# analysis with categorical fact type
m = lmer(response ~ cfact_type * cVeridicalityMean + (1+cfact_type|workerid) + (1+cVeridicalityMean|content) + (1+cfact_type|verb),data=cp)
summary(m) #main effect of prior, but not of veridicality or interaction

# predict projectivity from veridicality of verb, continuous prior probability of eventuality
m = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1+cPriorMean|content) + (1+cPriorMean|verb) + (1|fact),data=cp)
summary(m)  #main effect of prior, but not of veridicality or interaction


