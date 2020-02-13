# Prior probability work
# 5-projectivity-no-fact

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
theme_set(theme_bw())

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #7800 = 300 participants x 26 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #5.91
median(d$Answer.time_in_minutes) #5.17

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes)
nrow(d) #7800

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #52 missing values (2 Turkers)
table(d$age) #20-71
median(d$age,na.rm=TRUE) #36
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#136 female, 157 male, 2 other, 5 undeclared

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- droplevels(subset(d, (d$language != "" & d$language != "Gujarati" & d$language != "spanish" & d$language != "1" & d$language != "Chinese" & d$language != "Russian" & d$language != "Spanish")))
length(unique(d$workerid)) #292 (8 Turkers excluded)

# American English
length(which(is.na(d$american))) #26 (1 person didn't respond)
table(d$american) # 4 declared non-American English
d <- droplevels(subset(d, d$american == "y"))
length(unique(d$workerid)) #287 (5 Turkers excluded, 13 excluded in total for language reasons)

## exclude turkers who completed the experiment too quickly??
# times = d %>% 
#   select(Answer.time_in_minutes,workerid) %>% 
#   unique() %>%
#   mutate(VerySlow = Answer.time_in_minutes > mean(Answer.time_in_minutes) + 2*sd(Answer.time_in_minutes)) %>%
#   filter(!VerySlow) %>%
#   mutate(TooFast = Answer.time_in_minutes < mean(Answer.time_in_minutes) - 2*sd(Answer.time_in_minutes))
# summary(times)

## exclude Turkers based on non-projecting controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "control")
c <- droplevels(c)
nrow(c) #1722 / 6 controls = 287 Turkers

# group mean on controls
round(mean(c$response),2) #.14

ggplot(c, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")
ggsave(f="../graphs/raw-responses-to-controls.pdf",height=4,width=6.5)

# group means on individual controls
means = aggregate(response ~ contentNr, data=c, FUN="mean")
means
# contentNr response
# 1  control1    0.127
# 2  control2    0.149
# 3  control3    0.129
# 4  control4    0.123
# 5  control5    0.168
# 6  control6    0.169

# Turkers with response means on controls more than 2sd above group mean
c.means = aggregate(response~workerid, data=c, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=c, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=c, FUN="ci.high")$response
c.means

c.g <- c.means[c.means$response > (mean(c.means$response) + 2*sd(c.means$response)),]
c.g
unique(length(c.g$workerid)) #16 Turkers gave high responses
mean(c.g$response) #.67
min(c.g$response)

# how many unique Turkers did badly on the controls?
outliers <- droplevels(subset(c, c$workerid %in% c.g$workerid))
nrow(outliers) #96 / 6 control items = 16 Turkers
outliers[,c("workerid","response")]

# look at the responses to the controls that these "outlier" Turkers did
ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
ggsave(f="../graphs/raw-responses-to-controls-by-outliers.pdf",height=6,width=10)

# mean response by outliers
o.means = aggregate(response~workerid, data=outliers, FUN="mean")
o.means$YMin = o.means$response - aggregate(response~workerid, data=outliers, FUN="ci.low")$response
o.means$YMax = o.means$response + aggregate(response~workerid, data=outliers, FUN="ci.high")$response
o.means

# responses here are supposed to be low but these Turkers
# gave consistently high responses across the control items

# exclude the 16 Turkers identified above
d <- droplevels(subset(d, !(d$workerid %in% outliers$workerid)))
length(unique(d$workerid)) #271 Turkers remain (287 - 16)

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(verb != "control") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 5 turkers consistently clicked on roughly the same point on the scale

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()

# exclude the 5 Turkers identified above
d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #266 Turkers remain

# exclude workers with factive mean smaller than or equal to control mean
# fcmeans = d %>%
#   filter(verb %in% c("control","be_annoyed","see","discover","reveal","know")) %>%
#   mutate(ItemType = ifelse(verb == "control","control","factive")) %>%
#   group_by(workerid,ItemType) %>%
#   summarize(Mean = mean(response)) %>%
#   spread(ItemType,Mean) %>%
#   mutate(FCDiff = factive - control)
# 
# negfcdiffworkers = fcmeans[fcmeans$FCDiff <= 0,]$workerid
# length(negfcdiffworkers) # 2 turkers

# exclude the 2 Turkers identified above
# d <- droplevels(subset(d, !(d$workerid %in% negfcdiffworkers)))
# length(unique(d$workerid)) #264 Turkers remain


# posfcdiffs = fcmeans %>%
#   filter(FCDiff > 0)
# 
# SD = sd(posfcdiffs$FCDiff)
# Mean = mean(posfcdiffs$FCDiff)
# Threshold = .1#Mean - 2.5*SD
# 
# posfcdiffs = posfcdiffs %>%
#   mutate(SmallDiff = FCDiff < Threshold) 
# summary(posfcdiffs)
# 
# smallfcdiffworkers = posfcdiffs[posfcdiffs$SmallDiff,]$workerid

# resulting group mean on controls
c <- droplevels(subset(d, d$verb == "control"))
round(mean(c$response),2) #.11

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #6916 / 26 items = 266 participants

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #6916

# change cd verb names to match veridicality names
cd = cd %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# age info
table(cd$age) #20-71
length(which(is.na(cd$age))) # 0 missing values
median(cd$age,na.rm=TRUE) #36

cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#118 female, 143 male, 2 other, 3 undeclared

# how many ratings per predicate and per predicate-clause combination?

# target data (20 items per Turker) 
names(cd)
table(cd$verb)
t <- droplevels(subset(cd, cd$verb != "MC"))
nrow(t) #5320 / 20 items = 266 Turkers

names(t)
tmp <- as.data.frame(table(t$verb))
min(tmp$Freq) #266
max(tmp$Freq) #266
mean(tmp$Freq) #266
# 266 because 266 Turkers and each Turker saw each predicate once

table(t$content)
t$predicateClause <- interaction(t$verb,t$content)
tmp <- as.data.frame(table(t$predicateClause))
head(tmp)
min(tmp$Freq) #4
max(tmp$Freq) #25
mean(tmp$Freq) #13.3

# plots ----

# mean projectivity by predicate, including the main clause controls
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
means
print(as_tibble(means), n = 22)
levels(means$verb)

# define colors for the predicates
cols = data.frame(V=levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = factor(x=
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(means$verb  %in% c("MC"),"MC","V")))),levels=rev(c("F","V","VNF","NF","MC")))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)


# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup, shape=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray70",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","main clause\ncontrols")),name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","main clause\ncontrols")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-projectivity-by-predicate-variability.pdf",height=4.5,width=7)

# mean projectivity by predicate, including the main clause controls (3-way distinction for Stuttgart job talk Dec 2019)
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
means
levels(means$verb)

# define colors for the predicates
cols = data.frame(V=levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"NF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = as.factor(
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"NF",
                       ifelse(means$verb  %in% c("MC"),"MC","V")))))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)


# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  #geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean certainty rating \n (higher is more projective)") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/3-way-means-projectivity-by-predicate-variability.pdf",height=4,width=7)

# boxplot
ggplot(cd, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Certainty rating")+
  xlab("Predicate")
ggsave("../graphs/boxplot-projectivity.pdf",height=4,width=8)

# are the responses bi-modal, as suspected by Dotlacil and wondered by Katsos?
names(cd)

means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means

cd$verb <-factor(cd$verb, levels=levels(means$Verb))

ggplot(cd, aes(x=verb, y=response)) +
  geom_point(position="jitter") +
  #geom_boxplot(colour = "grey50") +
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Certainty ratings") +
  xlab("Predicate") 
ggsave("../graphs/raw-ratings-by-predicate.pdf",height=6,width=10)


# do the responses for each predicate-clause combination differ a lot?
names(cd)
t <- droplevels(subset(cd, cd$verb != "MC"))
nrow(t) #5660 / 20 items = 283 Turkers
names(t)
table(t$content)

means = t %>%
  group_by(verb,content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means

t$verb <-factor(t$verb, levels=levels(means$Verb))

ggplot(means, aes(x=content, y=Mean)) +
  geom_point(position="jitter") +
  #geom_boxplot(colour = "grey50") +
  scale_alpha(range = c(.3,1)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  facet_grid(. ~ verb) +
  ylab("Certainty ratings") +
  xlab("Predicate") 
ggsave("../graphs/raw-ratings-by-predicate-and-content.pdf",height=6,width=20)

# median rating by predicates
medians = cd %>%
  group_by(verb) %>%
  summarize(Median = median(response),Quantile1 = quantile(response,1),Quantile2 = quantile(response,0.75),Quantile4 = quantile(response,0.25),Quantile5 = quantile(response,0))
options(tibble.print_max = Inf)
options(tibble.width = Inf)
medians

# plot mean projectivity by mean veridicality (2 measures) ----

head(t)
means = t %>%
  group_by(verb) %>%
  summarize(mean_proj = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(projMin = mean_proj - CILow, projMax = mean_proj + CIHigh)
#View(means)

# get veridicality means (2 measures)
infMeans <- read.csv(file="../../4-veridicality3/data/inference_means.csv", header=T, sep=",")
colnames(infMeans) <- c("verb","mean_inf","infMin","infMax")
infMeans <- droplevels(subset(infMeans, infMeans$verb != "entailing C" & infMeans$verb != "non-ent. C"))
infMeans
#View(infMeans)

contrMeans <- read.csv(file="../../2-veridicality2/data/veridicality_means.csv",header=T,sep=",")
colnames(contrMeans) <- c("verb","mean_contr","contrMin","contrMax")
contrMeans <- droplevels(subset(contrMeans, contrMeans$verb != "non-contrad. C" & contrMeans$verb != "contradictory C"))
#View(contrMeans)

merged <- means %>%
  left_join(infMeans,by=c("verb")) %>%
  left_join(contrMeans,by="verb")

cols = data.frame(V=levels(t$verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))
cols

ggplot(merged, aes(x=mean_proj,y=mean_inf)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating (higher = more projective)", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean inference rating", limits = c(0,1),
                    breaks=c(0,0.25, 0.50, 0.75, 1.00))
  #ylab("Mean inference rating") 
ggsave(file="../graphs/projection-by-inference.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=mean_inf)) +
  # geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating (higher = more projective)", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean inference rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00))
#ylab("Mean inference rating") 
ggsave(file="../graphs/projection-by-inference-nolabels.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=mean_contr)) +
  # geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=contrMin,ymax=contrMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean contradictoriness rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00))
#ylab("Mean contradictoriness rating") 
ggsave(file="../graphs/projection-by-contradictoriness-nolabels.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=mean_contr)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=contrMin,ymax=contrMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean contradictoriness rating", limits = c(0,1),
                   breaks=c(0,0.25, 0.50, 0.75, 1.00))
  #ylab("Mean contradictoriness rating") 
ggsave(file="../graphs/projection-by-contradictoriness.pdf",width=4.2,height=3.5)

# plots with binary entailment 
merged$infEnt <- "no"
merged$infEnt <- ifelse(merged$verb == "be_right" | merged$verb == "see" | merged$verb == "discover" | 
                          merged$verb == "prove" | merged$verb == "confirm","yes","no")
merged$contrEnt <- "no"
merged$contrEnt <- ifelse(merged$verb == "be_right","yes","no")

ggplot(merged, aes(x=mean_proj,y=infEnt)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax,height = .15),color="gray50",alpha=.5) +
  #geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating (higher = more projective)", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  ylab("Entailed") 
ggsave(file="../graphs/projection-by-inferenceEntailment.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=contrEnt)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax,height = .15),color="gray50",alpha=.5) +
  #geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  ylab("Entailed") 
ggsave(file="../graphs/projection-by-contradictorinessEntailment.pdf",width=4.2,height=3.5)

# plot by-participant variability
cd$PresumedVerbType = as.factor(
  ifelse(cd$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "factive", 
         ifelse(cd$verb %in% c("pretend", "think", "suggest", "say"), "plain non-factive", 
                ifelse(cd$verb %in% c("be_right","demonstrate"),"veridical non-factive",
                       ifelse(cd$verb %in% c("MC"),"MC","projective non-factive")))))

means = cd %>%
  group_by(PresumedVerbType,workerid) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) 
nrow(means)
head(means)

factives = means %>%
  filter(PresumedVerbType == "factive") %>%
  mutate(Participant = fct_reorder(as.factor(workerid),Mean))

means$Participant = factor(x=as.character(means$workerid),levels=levels(factives$Participant))
cd$Participant = factor(x=as.character(cd$workerid),levels=levels(factives$Participant))

# get rid of error bars for all but the "factives"
means[means$PresumedVerbType != "factive",]$YMin = means[means$PresumedVerbType != "factive",]$Mean
means[means$PresumedVerbType != "factive",]$YMax = means[means$PresumedVerbType != "factive",]$Mean

ggplot(means, aes(x=Participant, y=Mean, fill=PresumedVerbType)) +
  geom_point(shape=21, data=cd, aes(y=response,fill=PresumedVerbType), alpha=.1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  ylab("Mean certainty rating") +
  xlab("Participant") +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust=1,vjust=1 )) 
ggsave("../graphs/means-projectivity-by-participant.pdf",height=4,width=25)

## models ----
library(emmeans)
library(lme4)
library(brms)

# create item as combination of predicate and complement clause
cd$item = as.factor(paste(cd$verb,cd$content))

# LME model predicting rating from predicate
table(cd$verb)
cd$verb <- relevel(cd$verb, ref = "MC")
m = lmer(response ~ verb + (1+verb|workerid) + (1|content), data = cd, REML=F)
summary(m)

# reorder verb by mean
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))
means
levels(means$verb)

cd$verb <- factor(cd$verb, levels = unique(levels(means$verb)))
levels(cd$verb) 

# pairwise comparison
str(cd$response)
str(cd$verb)
cd$verb <- as.factor(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)
model = lmer(response ~ verb + (1|workerid) + (1|item), data=cd, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                      estimate         SE     df t.ratio p.value
# MC - pretend              -0.042020639 0.01916977  49.97  -2.192  0.8098
# MC - be_right             -0.071607572 0.01916501  50.13  -3.736  0.0542
# MC - think                -0.083783582 0.01916492  50.07  -4.372  0.0089
# MC - suggest              -0.111608001 0.01912903  50.69  -5.834  0.0001
# MC - say                  -0.133890950 0.01912899  50.72  -6.999  <.0001
# MC - prove                -0.193307193 0.01914129  50.50 -10.099  <.0001
# MC - confirm              -0.231812154 0.01917434  50.04 -12.090  <.0001
# MC - establish            -0.251595754 0.01913785  50.56 -13.146  <.0001
# MC - demonstrate          -0.381538510 0.01913109  50.66 -19.943  <.0001
# MC - announce             -0.471695123 0.01912825  50.73 -24.660  <.0001
# MC - confess              -0.527021744 0.01916157  50.13 -27.504  <.0001
# MC - admit                -0.544807503 0.01912812  50.71 -28.482  <.0001
# MC - reveal               -0.592362151 0.01912462  50.78 -30.974  <.0001
# MC - acknowledge          -0.613560616 0.01916529  50.13 -32.014  <.0001
# MC - hear                 -0.634910506 0.01912981  50.72 -33.190  <.0001
# MC - discover             -0.667312302 0.01913513  50.63 -34.874  <.0001
# MC - inform               -0.693266524 0.01915643  50.26 -36.190  <.0001
# MC - see                  -0.697664502 0.01914503  50.43 -36.441  <.0001
# MC - know                 -0.752791183 0.01913805  50.55 -39.335  <.0001
# MC - be_annoyed           -0.773522289 0.01915574  50.30 -40.381  <.0001
# pretend - be_right        -0.029586933 0.02175318 352.08  -1.360  0.9989
# pretend - think           -0.041762942 0.02175315 351.11  -1.920  0.9404
# pretend - suggest         -0.069587361 0.02172153 363.43  -3.204  0.1553
# pretend - say             -0.091870311 0.02172148 363.91  -4.229  0.0051
# pretend - prove           -0.151286554 0.02173230 359.55  -6.961  <.0001
# pretend - confirm         -0.189791515 0.02176139 349.96  -8.721  <.0001
# pretend - establish       -0.209575115 0.02172928 360.73  -9.645  <.0001
# pretend - demonstrate     -0.339517871 0.02172331 362.84 -15.629  <.0001
# pretend - announce        -0.429674484 0.02172086 363.99 -19.782  <.0001
# pretend - confess         -0.485001104 0.02175018 352.32 -22.299  <.0001
# pretend - admit           -0.502786864 0.02172074 363.80 -23.148  <.0001
# pretend - reveal          -0.550341512 0.02171763 365.20 -25.341  <.0001
# pretend - acknowledge     -0.571539977 0.02175347 351.98 -26.274  <.0001
# pretend - hear            -0.592889867 0.02172222 363.73 -27.294  <.0001
# pretend - discover        -0.625291662 0.02172689 361.93 -28.780  <.0001
# pretend - inform          -0.651245884 0.02174563 354.69 -29.948  <.0001
# pretend - see             -0.655643862 0.02173562 358.16 -30.164  <.0001
# pretend - know            -0.710770543 0.02172942 360.62 -32.710  <.0001
# pretend - be_annoyed      -0.731501649 0.02174507 355.26 -33.640  <.0001
# be_right - think          -0.012176009 0.02174894 353.91  -0.560  1.0000
# be_right - suggest        -0.040000428 0.02171732 366.37  -1.842  0.9600
# be_right - say            -0.062283378 0.02171730 366.84  -2.868  0.3344
# be_right - prove          -0.121699621 0.02172813 362.42  -5.601  <.0001
# be_right - confirm        -0.160204582 0.02175720 352.72  -7.363  <.0001
# be_right - establish      -0.179988182 0.02172509 363.62  -8.285  <.0001
# be_right - demonstrate    -0.309930938 0.02171914 365.74 -14.270  <.0001
# be_right - announce       -0.400087551 0.02171664 366.95 -18.423  <.0001
# be_right - confess        -0.455414171 0.02174594 355.15 -20.942  <.0001
# be_right - admit          -0.473199931 0.02171652 366.75 -21.790  <.0001
# be_right - reveal         -0.520754579 0.02171338 368.20 -23.983  <.0001
# be_right - acknowledge    -0.541953044 0.02174927 354.77 -24.918  <.0001
# be_right - hear           -0.563302934 0.02171802 366.67 -25.937  <.0001
# be_right - discover       -0.595704730 0.02172270 364.85 -27.423  <.0001
# be_right - inform         -0.621658951 0.02174146 357.50 -28.593  <.0001
# be_right - see            -0.626056929 0.02173140 361.04 -28.809  <.0001
# be_right - know           -0.681183610 0.02172529 363.46 -31.354  <.0001
# be_right - be_annoyed     -0.701914716 0.02174087 358.10 -32.285  <.0001
# think - suggest           -0.027824419 0.02171720 365.42  -1.281  0.9995
# think - say               -0.050107369 0.02171718 365.89  -2.307  0.7508
# think - prove             -0.109523612 0.02172804 361.46  -5.041  0.0001
# think - confirm           -0.148028572 0.02175712 351.80  -6.804  <.0001
# think - establish         -0.167812173 0.02172500 362.66  -7.724  <.0001
# think - demonstrate       -0.297754928 0.02171905 364.77 -13.709  <.0001
# think - announce          -0.387911542 0.02171658 365.95 -17.862  <.0001
# think - confess           -0.443238162 0.02174589 354.19 -20.383  <.0001
# think - admit             -0.461023921 0.02171643 365.78 -21.229  <.0001
# think - reveal            -0.508578569 0.02171334 367.18 -23.422  <.0001
# think - acknowledge       -0.529777035 0.02174921 353.82 -24.358  <.0001
# think - hear              -0.551126924 0.02171787 365.74 -25.377  <.0001
# think - discover          -0.583528720 0.02172260 363.89 -26.863  <.0001
# think - inform            -0.609482942 0.02174134 356.58 -28.033  <.0001
# think - see               -0.613880920 0.02173129 360.11 -28.249  <.0001
# think - know              -0.669007601 0.02172518 362.52 -30.794  <.0001
# think - be_annoyed        -0.689738707 0.02174078 357.16 -31.726  <.0001
# suggest - say             -0.022282950 0.02168553 379.04  -1.028  1.0000
# suggest - prove           -0.081699192 0.02169639 374.37  -3.766  0.0285
# suggest - confirm         -0.120204153 0.02172556 364.09  -5.533  <.0001
# suggest - establish       -0.139987753 0.02169334 375.64  -6.453  <.0001
# suggest - demonstrate     -0.269930509 0.02168737 377.89 -12.446  <.0001
# suggest - announce        -0.360087123 0.02168490 379.13 -16.605  <.0001
# suggest - confess         -0.415413743 0.02171425 366.68 -19.131  <.0001
# suggest - admit           -0.433199502 0.02168475 378.95 -19.977  <.0001
# suggest - reveal          -0.480754150 0.02168168 380.41 -22.173  <.0001
# suggest - acknowledge     -0.501952616 0.02171755 366.31 -23.113  <.0001
# suggest - hear            -0.523302505 0.02168625 378.86 -24.131  <.0001
# suggest - discover        -0.555704301 0.02169097 376.91 -25.619  <.0001
# suggest - inform          -0.581658523 0.02170974 369.17 -26.793  <.0001
# suggest - see             -0.586056501 0.02169969 372.89 -27.008  <.0001
# suggest - know            -0.641183182 0.02169352 375.49 -29.556  <.0001
# suggest - be_annoyed      -0.661914288 0.02170912 369.83 -30.490  <.0001
# say - prove               -0.059416243 0.02169635 374.86  -2.739  0.4254
# say - confirm             -0.097921204 0.02172555 364.55  -4.507  0.0016
# say - establish           -0.117704804 0.02169334 376.11  -5.426  <.0001
# say - demonstrate         -0.247647560 0.02168737 378.36 -11.419  <.0001
# say - announce            -0.337804173 0.02168484 379.66 -15.578  <.0001
# say - confess             -0.393130793 0.02171422 367.16 -18.105  <.0001
# say - admit               -0.410916553 0.02168471 379.46 -18.950  <.0001
# say - reveal              -0.458471201 0.02168165 380.91 -21.146  <.0001
# say - acknowledge         -0.479669666 0.02171752 366.79 -22.087  <.0001
# say - hear                -0.501019556 0.02168617 379.41 -23.103  <.0001
# say - discover            -0.533421352 0.02169092 377.43 -24.592  <.0001
# say - inform              -0.559375573 0.02170970 369.66 -25.766  <.0001
# say - see                 -0.563773551 0.02169966 373.39 -25.981  <.0001
# say - know                -0.618900232 0.02169349 375.98 -28.529  <.0001
# say - be_annoyed          -0.639631338 0.02170909 370.31 -29.464  <.0001
# prove - confirm           -0.038504961 0.02173640 360.15  -1.771  0.9731
# prove - establish         -0.058288561 0.02170419 371.50  -2.686  0.4651
# prove - demonstrate       -0.188231317 0.02169820 373.73  -8.675  <.0001
# prove - announce          -0.278387930 0.02169568 374.99 -12.831  <.0001
# prove - confess           -0.333714550 0.02172508 362.71 -15.361  <.0001
# prove - admit             -0.351500310 0.02169558 374.77 -16.201  <.0001
# prove - reveal            -0.399054958 0.02169251 376.20 -18.396  <.0001
# prove - acknowledge       -0.420253423 0.02172836 362.36 -19.341  <.0001
# prove - hear              -0.441603313 0.02169707 374.69 -20.353  <.0001
# prove - discover          -0.474005109 0.02170182 372.75 -21.842  <.0001
# prove - inform            -0.499959330 0.02172053 365.19 -23.018  <.0001
# prove - see               -0.504357308 0.02171049 368.84 -23.231  <.0001
# prove - know              -0.559483989 0.02170436 371.35 -25.777  <.0001
# prove - be_annoyed        -0.580215095 0.02171994 365.81 -26.713  <.0001
# confirm - establish       -0.019783600 0.02173331 361.39  -0.910  1.0000
# confirm - demonstrate     -0.149726356 0.02172736 363.49  -6.891  <.0001
# confirm - announce        -0.239882969 0.02172486 364.68 -11.042  <.0001
# confirm - confess         -0.295209590 0.02175419 352.98 -13.570  <.0001
# confirm - admit           -0.312995349 0.02172471 364.51 -14.407  <.0001
# confirm - reveal          -0.360549997 0.02172167 365.86 -16.599  <.0001
# confirm - acknowledge     -0.381748462 0.02175744 352.66 -17.546  <.0001
# confirm - hear            -0.403098352 0.02172625 364.40 -18.554  <.0001
# confirm - discover        -0.435500148 0.02173092 362.60 -20.041  <.0001
# confirm - inform          -0.461454369 0.02174966 355.33 -21.217  <.0001
# confirm - see             -0.465852348 0.02173966 358.81 -21.429  <.0001
# confirm - know            -0.520979028 0.02173343 361.29 -23.971  <.0001
# confirm - be_annoyed      -0.541710134 0.02174904 355.95 -24.907  <.0001
# establish - demonstrate   -0.129942756 0.02169515 375.00  -5.989  <.0001
# establish - announce      -0.220099369 0.02169266 376.24 -10.146  <.0001
# establish - confess       -0.275425990 0.02172202 363.93 -12.680  <.0001
# establish - admit         -0.293211749 0.02169254 376.04 -13.517  <.0001
# establish - reveal        -0.340766397 0.02168946 377.49 -15.711  <.0001
# establish - acknowledge   -0.361964862 0.02172535 363.54 -16.661  <.0001
# establish - hear          -0.383314752 0.02169403 375.97 -17.669  <.0001
# establish - discover      -0.415716548 0.02169871 374.07 -19.159  <.0001
# establish - inform        -0.441670769 0.02171751 366.39 -20.337  <.0001
# establish - see           -0.446068748 0.02170745 370.08 -20.549  <.0001
# establish - know          -0.501195428 0.02170132 372.61 -23.095  <.0001
# establish - be_annoyed    -0.521926534 0.02171688 367.05 -24.033  <.0001
# demonstrate - announce    -0.090156613 0.02168671 378.48  -4.157  0.0068
# demonstrate - confess     -0.145483234 0.02171609 366.04  -6.699  <.0001
# demonstrate - admit       -0.163268993 0.02168659 378.27  -7.529  <.0001
# demonstrate - reveal      -0.210823641 0.02168350 379.75  -9.723  <.0001
# demonstrate - acknowledge -0.232022106 0.02171937 365.68 -10.683  <.0001
# demonstrate - hear        -0.253371996 0.02168808 378.19 -11.683  <.0001
# demonstrate - discover    -0.285773792 0.02169275 376.29 -13.174  <.0001
# demonstrate - inform      -0.311728014 0.02171154 368.55 -14.358  <.0001
# demonstrate - see         -0.316125992 0.02170150 372.26 -14.567  <.0001
# demonstrate - know        -0.371252672 0.02169533 374.84 -17.112  <.0001
# demonstrate - be_annoyed  -0.391983778 0.02171092 369.20 -18.055  <.0001
# announce - confess        -0.055326620 0.02171357 367.26  -2.548  0.5718
# announce - admit          -0.073112380 0.02168406 379.57  -3.372  0.0981
# announce - reveal         -0.120667028 0.02168102 381.01  -5.566  <.0001
# announce - acknowledge    -0.141865493 0.02171682 366.93  -6.533  <.0001
# announce - hear           -0.163215383 0.02168556 379.49  -7.526  <.0001
# announce - discover       -0.195617179 0.02169026 377.55  -9.019  <.0001
# announce - inform         -0.221571400 0.02170904 369.77 -10.206  <.0001
# announce - see            -0.225969378 0.02169903 373.47 -10.414  <.0001
# announce - know           -0.281096059 0.02169282 376.11 -12.958  <.0001
# announce - be_annoyed     -0.301827165 0.02170843 370.42 -13.904  <.0001
# confess - admit           -0.017785759 0.02171345 367.07  -0.819  1.0000
# confess - reveal          -0.065340407 0.02171039 368.45  -3.010  0.2478
# confess - acknowledge     -0.086538873 0.02174624 355.04  -3.979  0.0134
# confess - hear            -0.107888762 0.02171498 366.96  -4.968  0.0002
# confess - discover        -0.140290558 0.02171962 365.17  -6.459  <.0001
# confess - inform          -0.166244780 0.02173840 357.79  -7.648  <.0001
# confess - see             -0.170642758 0.02172837 361.32  -7.853  <.0001
# confess - know            -0.225769439 0.02172222 363.77 -10.393  <.0001
# confess - be_annoyed      -0.246500545 0.02173779 358.41 -11.340  <.0001
# admit - reveal            -0.047554648 0.02168088 380.81  -2.193  0.8226
# admit - acknowledge       -0.068753113 0.02171675 366.70  -3.166  0.1709
# admit - hear              -0.090103003 0.02168545 379.27  -4.155  0.0068
# admit - discover          -0.122504799 0.02169011 377.37  -5.648  <.0001
# admit - inform            -0.148459021 0.02170898 369.52  -6.839  <.0001
# admit - see               -0.152856999 0.02169890 373.28  -7.044  <.0001
# admit - know              -0.207983680 0.02169272 375.88  -9.588  <.0001
# admit - be_annoyed        -0.228714786 0.02170827 370.26 -10.536  <.0001
# reveal - acknowledge      -0.021198465 0.02171367 368.09  -0.976  1.0000
# reveal - hear             -0.042548355 0.02168234 380.77  -1.962  0.9275
# reveal - discover         -0.074950151 0.02168705 378.81  -3.456  0.0768
# reveal - inform           -0.100904373 0.02170584 370.99  -4.649  0.0009
# reveal - see              -0.105302351 0.02169580 374.73  -4.854  0.0003
# reveal - know             -0.160429031 0.02168964 377.33  -7.397  <.0001
# reveal - be_annoyed       -0.181160138 0.02170524 371.63  -8.346  <.0001
# acknowledge - hear        -0.021349890 0.02171823 366.62  -0.983  1.0000
# acknowledge - discover    -0.053751686 0.02172293 364.79  -2.474  0.6289
# acknowledge - inform      -0.079705907 0.02174171 357.42  -3.666  0.0400
# acknowledge - see         -0.084103885 0.02173165 360.97  -3.870  0.0199
# acknowledge - know        -0.139230566 0.02172545 363.46  -6.409  <.0001
# acknowledge - be_annoyed  -0.159961672 0.02174108 358.05  -7.358  <.0001
# hear - discover           -0.032401796 0.02169165 377.25  -1.494  0.9963
# hear - inform             -0.058356018 0.02171044 369.47  -2.688  0.4634
# hear - see                -0.062753996 0.02170037 373.22  -2.892  0.3186
# hear - know               -0.117880676 0.02169422 375.79  -5.434  <.0001
# hear - be_annoyed         -0.138611782 0.02170981 370.14  -6.385  <.0001
# discover - inform         -0.025954222 0.02171514 367.61  -1.195  0.9998
# discover - see            -0.030352200 0.02170507 371.33  -1.398  0.9985
# discover - know           -0.085478881 0.02169893 373.88  -3.939  0.0154
# discover - be_annoyed     -0.106209987 0.02171446 368.32  -4.891  0.0003
# inform - see              -0.004397978 0.02172387 363.74  -0.202  1.0000
# inform - know             -0.059524659 0.02171765 366.27  -2.741  0.4237
# inform - be_annoyed       -0.080255765 0.02173323 360.85  -3.693  0.0366
# see - know                -0.055126681 0.02170760 369.95  -2.540  0.5784
# see - be_annoyed          -0.075857787 0.02172323 364.40  -3.492  0.0691
# know - be_annoyed         -0.020731106 0.02171705 366.90  -0.955  1.0000

# JD CODE STARTS HERE ----
# TL;DR: all verbs are different from main clause (non-projecting) controls (called "control")
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
table(cd$verb)

# Bayesian mixed effects regression to test whether ratings differ by predicate from good controls
cd$workerid = as.factor(as.character(cd$workerid))
cd$content = as.factor(as.character(cd$content))

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response)) +
  geom_histogram()

# set reference level to main clause controls
d = cd %>%
  droplevels() %>%
  mutate(verb = fct_relevel(verb,"control"))
table(d$verb)

# JT commented the following code, to prevent accidental re-runs
# # zoib model without random effects
zoib_model <- bf(
  response ~ verb, # beta distribution???s mean
  zoi ~ verb, # zero-one inflation (alpha); ie, probability of a binary rating as a function of verb
  phi ~ verb, # beta distribution's precision  
  coi ~ verb, # conditional one-inflation
  family = zero_one_inflated_beta()
) 

# fit model
m <- brm(
  formula = zoib_model,
  data = d,
  cores = 4#,
  # file = here::here("zoib-ex")
)
  # no need to run this multiple times:
saveRDS(m,file="../data/zoib-model.rds")

# # zoib model with random effects
zoib_model <- bf(
  response ~ verb, # beta distribution's mean
  zoi ~ verb + (1+verb|workerid) + (1+verb|content), # zero-one inflation (alpha); ie, probability of a binary rating as a function of verb
  phi ~ verb, # beta distribution's precision  
  coi ~ verb + (1+verb|workerid) + (1+verb|content), # conditional one-inflation
  family = zero_one_inflated_beta()
) 

# fit model
m <- brm(
  formula = zoib_model,
  data = d,
  cores = 4#,
  # file = here::here("zoib-ex")
)
# no need to run this multiple times:
saveRDS(m,file="../data/zoib-model-mixed.rds")

# load ZOIB model ----
m <- readRDS(file="../data/zoib-model.rds") # no random effects
m <- readRDS(file="../data/zoib-model-mixed.rds") # random effects

summary(m) # see summary printed below

# transform each of the posterior samples, and then re-calculate the summaries on original scale
posterior_samples(m, pars = "b_")[,1:4] %>% 
  mutate_at(c("b_phi_Intercept"), exp) %>% 
  mutate_at(vars(-"b_phi_Intercept"), plogis) %>% 
  posterior_summary() %>% 
  as.data.frame() %>% 
  rownames_to_column("Parameter") %>% 
  kable(digits = 2) 

# |Parameter       | Estimate| Est.Error| Q2.5| Q97.5|
#   |:---------------|--------:|---------:|----:|-----:|
#   |b_Intercept     |     0.17|      0.01| 0.16|  0.18|
#   |b_phi_Intercept |     3.77|      0.16| 3.46|  4.11|
#   |b_zoi_Intercept |     0.27|      0.01| 0.25|  0.29|
#   |b_coi_Intercept |     0.00|      0.00| 0.00|  0.01|

# The .17 and 3.77 values are the mean and precision of the beta distribution that characterizes the 
# controls that are not zeroes and ones -- this is a distribution skewed towards 0
# The .27 value is the probability that an observation will be either 0 or 1, and of these 
# 27% endpoint values, 0% (last value) are ones. So: as expected, the MC controls are heavily 0-skewed, 
# see also this histogram:
ggplot(d[d$verb=="control",], aes(x=response)) +
  geom_histogram()

# in principle, we can ask for each verb whether it differs from the controls, as follows:
h <- c("pretend - control" = "plogis(Intercept + verbpretend) = plogis(Intercept)")
hypothesis(m, h) # even the least projecty is more projecty than control

# plot estimated mu parameter
plot(
  marginal_effects(m, dpar = "mu"), 
  points = TRUE, 
  point_args = list(width = .05, shape = 1)
)

# > summary(m)
# Family: zero_one_inflated_beta 
# Links: mu = logit; phi = log; zoi = logit; coi = logit 
# Formula: response ~ verb 
# phi ~ verb
# zoi ~ verb
# coi ~ verb
# Data: d (Number of observations: 6916) 
# Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup samples = 4000
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
# Intercept                -1.62      0.04    -1.69    -1.55        660 1.01
# phi_Intercept             1.33      0.04     1.24     1.41        738 1.01
# zoi_Intercept            -1.01      0.06    -1.12    -0.90       1486 1.00
# coi_Intercept            -6.37      1.23    -9.54    -4.61        159 1.03
# verbacknowledge           2.31      0.09     2.13     2.49       2036 1.00
# verbadmit                 2.16      0.08     1.99     2.32       2172 1.00
# verbannounce              1.87      0.08     1.71     2.03       2327 1.00
# verbannoyed               3.16      0.09     2.97     3.34       2243 1.00
# verbbe_right_that         0.39      0.09     0.22     0.57       2413 1.00
# verbconfess               2.15      0.08     1.99     2.31       1924 1.00
# verbconfirm               1.14      0.08     0.99     1.30       1909 1.00
# verbdemonstrate           1.60      0.08     1.44     1.76       2077 1.00
# verbdiscover              2.57      0.09     2.39     2.74       2472 1.00
# verbestablish             1.15      0.08     0.99     1.31       1913 1.00
# verbhear                  2.40      0.09     2.23     2.57       2444 1.00
# verbinform_Sam            2.65      0.09     2.48     2.83       2133 1.00
# verbknow                  3.11      0.10     2.92     3.30       2014 1.00
# verbpretend               0.32      0.09     0.14     0.49       1937 1.00
# verbprove                 0.91      0.08     0.75     1.06       1780 1.00
# verbreveal                2.32      0.09     2.15     2.49       1985 1.00
# verbsay                   0.71      0.08     0.54     0.88       1907 1.00
# verbsee                   2.68      0.09     2.50     2.86       2603 1.00
# verbsuggest               0.65      0.08     0.49     0.82       1628 1.00
# verbthink                 0.42      0.08     0.26     0.57       1970 1.00
# phi_verbacknowledge      -0.74      0.09    -0.92    -0.56       1966 1.00
# phi_verbadmit            -0.66      0.09    -0.83    -0.49       1951 1.00
# phi_verbannounce         -0.62      0.09    -0.79    -0.44       1674 1.00
# phi_verbannoyed          -0.14      0.11    -0.35     0.08       1644 1.00
# phi_verbbe_right_that    -0.21      0.11    -0.41    -0.00       2002 1.00
# phi_verbconfess          -0.69      0.09    -0.87    -0.51       1577 1.00
# phi_verbconfirm          -0.51      0.09    -0.69    -0.34       1867 1.00
# phi_verbdemonstrate      -0.72      0.09    -0.89    -0.55       1676 1.00
# phi_verbdiscover         -0.53      0.10    -0.73    -0.34       1712 1.00
# phi_verbestablish        -0.69      0.09    -0.86    -0.52       1841 1.00
# phi_verbhear             -0.66      0.10    -0.85    -0.47       1972 1.00
# phi_verbinform_Sam       -0.56      0.10    -0.76    -0.36       1803 1.00
# phi_verbknow             -0.23      0.11    -0.45    -0.02       1842 1.00
# phi_verbpretend          -0.18      0.10    -0.39     0.02       1775 1.00
# phi_verbprove            -0.30      0.09    -0.48    -0.11       1773 1.00
# phi_verbreveal           -0.67      0.09    -0.86    -0.49       1911 1.00
# phi_verbsay              -0.45      0.10    -0.64    -0.26       1623 1.00
# phi_verbsee              -0.54      0.10    -0.74    -0.35       1766 1.00
# phi_verbsuggest          -0.24      0.10    -0.43    -0.05       1691 1.00
# phi_verbthink            -0.03      0.10    -0.23     0.16       1951 1.00
# zoi_verbacknowledge      -0.89      0.19    -1.26    -0.52       3119 1.00
# zoi_verbadmit            -1.11      0.21    -1.54    -0.71       3420 1.00
# zoi_verbannounce         -0.96      0.19    -1.35    -0.60       3160 1.00
# zoi_verbannoyed          -0.17      0.16    -0.48     0.13       3136 1.00
# zoi_verbbe_right_that    -0.54      0.17    -0.87    -0.21       3174 1.00
# zoi_verbconfess          -1.19      0.21    -1.61    -0.79       3244 1.00
# zoi_verbconfirm          -1.00      0.20    -1.40    -0.62       3232 1.00
# zoi_verbdemonstrate      -1.28      0.22    -1.72    -0.86       3790 1.00
# zoi_verbdiscover         -0.62      0.17    -0.97    -0.29       2895 1.00
# zoi_verbestablish        -1.20      0.21    -1.63    -0.80       3503 1.00
# zoi_verbhear             -0.54      0.17    -0.88    -0.22       3442 1.00
# zoi_verbinform_Sam       -0.62      0.17    -0.98    -0.29       2808 1.00
# zoi_verbknow             -0.34      0.16    -0.67    -0.03       2551 1.00
# zoi_verbpretend          -0.32      0.16    -0.64    -0.01       3192 1.00
# zoi_verbprove            -1.18      0.21    -1.61    -0.79       3648 1.00
# zoi_verbreveal           -0.93      0.19    -1.30    -0.56       2976 1.00
# zoi_verbsay              -1.07      0.20    -1.47    -0.67       3372 1.00
# zoi_verbsee              -0.39      0.16    -0.72    -0.07       2981 1.00
# zoi_verbsuggest          -0.74      0.18    -1.11    -0.39       3539 1.00
# zoi_verbthink            -0.93      0.19    -1.32    -0.57       3508 1.00
# coi_verbacknowledge       8.91      1.39     6.67    12.19        227 1.02
# coi_verbadmit             7.38      1.30     5.36    10.59        183 1.02
# coi_verbannounce          7.24      1.30     5.25    10.53        161 1.03
# coi_verbannoyed          11.12      1.83     8.28    15.54        263 1.02
# coi_verbbe_right_that     2.03      1.73    -1.36     5.56        294 1.01
# coi_verbconfess           6.77      1.30     4.79    10.10        176 1.02
# coi_verbconfirm           4.31      1.34     2.15     7.53        191 1.02
# coi_verbdemonstrate       6.45      1.30     4.46     9.73        172 1.02
# coi_verbdiscover          9.16      1.40     6.98    12.59        194 1.02
# coi_verbestablish         5.47      1.30     3.45     8.67        185 1.02
# coi_verbhear              8.38      1.32     6.36    11.57        166 1.02
# coi_verbinform_Sam       10.76      1.79     7.95    14.91        340 1.02
# coi_verbknow              9.42      1.40     7.20    12.72        210 1.02
# coi_verbpretend         -26.43     27.84   -98.77     1.90        751 1.00
# coi_verbprove             4.15      1.39     1.78     7.48        188 1.02
# coi_verbreveal            7.78      1.30     5.75    11.08        181 1.02
# coi_verbsay               2.51      1.72    -0.83     6.18        277 1.02
# coi_verbsee               9.35      1.39     7.16    12.71        195 1.02
# coi_verbsuggest           2.16      1.78    -1.42     5.85        348 1.01
# coi_verbthink           -26.14     27.76  -102.44     2.14       1114 1.00