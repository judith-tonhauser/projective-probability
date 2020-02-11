# Prior probability work
# 2-veridicality2 -- Contradictoriness ratings
# Sally: "Dan knows that Sophia got a tattoo, but she didn't."
# Is Sally's utterance contradictory?

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dichromat)
library(brms)
library(knitr)
theme_set(theme_bw())

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #8400 = 300 participants x 28 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #6.4
median(d$Answer.time_in_minutes) #5.2

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,asses,american,gender,comments,Answer.time_in_minutes)
nrow(d) #8400

# look at Turkers' comments
unique(d$comments)

# how did Turkers assess their performance?
table(d$asses)
# Did you understand the task?
#Confused   No      Yes 
#700        364     7280

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) #18-72
median(d$age,na.rm=TRUE) #35 
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 137 female, 162 male, 1 other

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "" & d$language != "chinese" & d$language != "Portuguese"))
d = droplevels(d)
length(unique(d$workerid)) #295 (5 Turkers excluded)

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #281 (19 Turkers excluded for language reasons)

## exclude Turkers based on controls
names(d)
table(d$contentNr)
table(d$verb)

# make relevant data subsets
# control_bad: contradictory controls (YES = 1)
c.bad <- subset(d, d$verb == "control_bad")
c.bad <- droplevels(c.bad)
nrow(c.bad) #1124 / 4 contradictory controls = 281 Turkers

# control_good: noncontradictory controls (NO = 0)
c.good <- subset(d, d$verb == "control_good")
c.good <- droplevels(c.good)
nrow(c.good) #1124

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2248

# group mean on contradictory controls
round(mean(c.bad$response),2) #.94

# group mean on noncontradictory controls
round(mean(c.good$response),2) #.08

ggplot(c.bad, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.bad$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")

ggplot(c.good, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.good$response, n = 10)) +
  ylab("Response") +
  xlab("Participant")

# group means on individual controls
means = aggregate(response ~ contentNr, data=c.bad, FUN="mean")
means
#    contentNr  response
#1 control_bad1 0.9397509
#2 control_bad2 0.9323132
#3 control_bad3 0.9256940
#4 control_bad4 0.9566548

means = aggregate(response ~ contentNr, data=c.good, FUN="mean")
means
#contentNr   response
#1 control_good1 0.16964413 "Zack believes that I'm married, but I'm actually single"
#2 control_good2 0.05313167
#3 control_good3 0.05120996
#4 control_good4 0.05811388

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2248 / 8 items = 281 Turkers

# Turkers with response means on noncontradictory controls (0) more than 2sd above group mean
cg.means = aggregate(response~workerid, data=c.good, FUN="mean")
cg.means$YMin = cg.means$response - aggregate(response~workerid, data=c.good, FUN="ci.low")$response
cg.means$YMax = cg.means$response + aggregate(response~workerid, data=c.good, FUN="ci.high")$response
cg.means

c.g <- cg.means[cg.means$response > (mean(cg.means$response) + 2*sd(cg.means$response)),]
c.g
unique(length(c.g$workerid)) #12 Turkers gave high responses
mean(c.g$response) #.6

# Turkers with response means on contradictory controls (1) more than 2sd below group mean
cb.means = aggregate(response~workerid, data=c.bad, FUN="mean")
cb.means$YMin = cb.means$response - aggregate(response~workerid, data=c.bad, FUN="ci.low")$response
cb.means$YMax = cb.means$response + aggregate(response~workerid, data=c.bad, FUN="ci.high")$response
cb.means

c.b <- cb.means[cb.means$response < (mean(cb.means$response) - 2*sd(cb.means$response)),]
c.b
unique(length(c.b$workerid)) #15 Turkers gave low responses
mean(c.b$response) #.58

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid | c$workerid %in% c.b$workerid)
outliers = droplevels(outliers)
nrow(outliers) #144 / 8 control items = 18 Turkers
table(outliers$workerid)

# look at the responses to the controls that these "outlier" Turkers did
# outliers to noncontradictory items (0)
outliers.g <- subset(c.good, c.good$workerid %in% c.g$workerid)
outliers.g = droplevels(outliers.g)
nrow(outliers.g) #48 / 4 control items = 12 Turkers

ggplot(outliers.g, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be 0 (noncontradictory) but these 9 Turkers
# gave consistently high responses across the control items

# outliers to contradictory items (1)
outliers.b <- subset(c.bad, c.bad$workerid %in% c.b$workerid)
outliers.b = droplevels(outliers.b)
nrow(outliers.b) #60 / 4 control items = 15 Turkers

ggplot(outliers.b, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.b$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be 1 (contradictory) but these 7 Turkers
# gave consistently low responses across the control items 

# exclude the 18 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #263 Turkers remain (18 excluded for problems with control items)

# exclude turkers who always clicked on roughly the same point on the scale 
# for the target items
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(verb != "control_good" & verb != "control_bad") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))
head(variances)
summary(variances)

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 0 turkers consistently clicked on roughly the same point on the scale


# new group means
c.bad <- droplevels(subset(d, d$verb == "control_bad"))
nrow(c.bad) #1052 / 4 controls = 263 Turkers
c.good <- droplevels(subset(d, d$verb == "control_good"))
nrow(c.good) #1052
round(mean(c.bad$response),2) #.96
round(mean(c.good$response),2) #.06

# clean data = cd
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #7364 / 28 items = 263 participants

# load clean data for analysis ----
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# age info
table(cd$age) #18-72
length(which(is.na(cd$age))) #0 missing values
median(cd$age,na.rm=TRUE) #36
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 126 female, 136 male, 1 other

# change the name of the predicates
table(cd$verb)
cd$verb <- gsub("be_right_that","be_right",cd$verb)
cd$verb <- gsub("inform_Sam","inform",cd$verb)
cd$verb <- gsub("annoyed","be_annoyed",cd$verb)
cd$verb <- gsub("control_good","non-contrd. C",cd$verb)
cd$verb <- gsub("control_bad","contradictory C",cd$verb)

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "non-contrd. C" & cd$verb != "contradictory C")
t <- droplevels(t)
nrow(t) #5260 / 20 = 263 Turkers

# target data plus good (entailing) controls
te <- droplevels(subset(cd,cd$verb != "non-contrd. C"))
nrow(te) #6312

# how many ratings per predicate and per predicate-clause combination?
names(t)
tmp <- as.data.frame(table(t$verb))
min(tmp$Freq) 
max(tmp$Freq) 
mean(tmp$Freq)
# 263 because 263 Turkers and each Turker saw each predicate once

table(t$content)
t$predicateClause <- interaction(t$verb,t$content)
tmp <- as.data.frame(table(t$predicateClause))
head(tmp)
min(tmp$Freq) #3
max(tmp$Freq) #22
mean(tmp$Freq) #13.2

cd$item <- paste(cd$verb,cd$content,sep="-")
table(cd$item)

# mean contradictoriness by verb
mean = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response)) %>%
  select(verb,Mean) %>%
  tbl_df %>% 
  print(n=22)

# mean contradictoriness by item
means = cd %>%
  group_by(item) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(item,Mean,YMin,YMax)
means = as.data.frame(means)
means

write.csv(means, file="../data/veridicality_item_means.csv",row.names=F,quote=F)

## plots ----

# plot of means with participant ratings (4-way distinction for factivity paper)
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means
levels(means$verb)

cd$verb <-factor(cd$verb, levels=levels(means$verb))

# define colors for the predicates
cols = data.frame(V=levels(cd$verb))
cols

levels(cols$V)
#cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("non-contrd. C","contradictory C"),"MC","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
levels(cols$VeridicalityGroup)


subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)

means$VeridicalityGroup = as.factor(
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(means$verb  %in% c("non-contrd. C","contradictory C"),"control","V")))))

ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("black","darkorchid","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Mean contradictoriness rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-contradictoriness-by-predicate-variability.pdf",height=4,width=7)

# plot of means with participant ratings (3-way distinction for Tuebingen talk)
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means
levels(means$verb)

cd$verb <-factor(cd$verb, levels=levels(means$verb))

# define colors for the predicates
cols = data.frame(V=levels(cd$verb))
cols

levels(cols$V)
cols$V <- factor(cols$V, levels = levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"NF",
                       ifelse(cols$V %in% c("non-contrd. C","contradictory C"),"MC","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
levels(cols$VeridicalityGroup)


subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)

means$VeridicalityGroup = as.factor(
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"NF",
                       ifelse(means$verb  %in% c("non-contrd. C","contradictory C"),"MC","V")))))

ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Mean contradictoriness rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-contradictoriness-by-predicate-variability-3way.pdf",height=4,width=7)



# also used in MIT talk
# boxplot of contradictoriness by predicate, collapsing over complement clauses
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(verb,Mean,YMin,YMax)
means = as.data.frame(means)
means
View(means)

write.csv(means, file="../data/veridicality_means.csv",row.names=F,quote=F)

# te$verb <-factor(te$verb, levels=means[order(means$Mean), "verb"])

cd$verb <-factor(cd$verb, levels=levels(means$Verb))

cols = data.frame(V=levels(cd$verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF","V"))))
cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "blue", 
                      ifelse(cols$VeridicalityGroup == "NF", "brown", 
                             ifelse(cols$VeridicalityGroup == "VNF","cornflowerblue","black")))

ggplot(cd, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))
ggsave("../graphs/boxplot-veridicality.pdf",height=4,width=8)



# means for semfest talk
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means

cd$verb <-factor(cd$verb, levels=levels(means$Verb))

cols = data.frame(V=levels(cd$verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("non-contrd. C","contradictory C"),"control","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "control","black","tomato1"))))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) %>%
  mutate(Verb = fct_reorder(as.factor(verb),Mean))

means$VeridicalityGroup = as.factor(
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(means$verb  %in% c("non-contrd. C","contradictory C"),"control","V")))))

# means for semfest talk

ggplot(means, aes(x=Verb, y=Mean, fill=VeridicalityGroup)) +
  #geom_point(color="black", size=4) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("black","darkorchid","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-projectivity-by-predicate-variability.pdf",height=4,width=7)






# plot of contradictoriness by predicate and complement clauses
agr_verb = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

agr_subj = t %>%
  group_by(content, verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_verb, aes(x=verb, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_point(data=agr_subj, aes(color=content)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate")
ggsave("../graphs/veridicality-means-byitem.pdf",height=4,width=8)

# plot of contradictoriness by complement clause
agr_content = t %>%
  group_by(content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_content, aes(x=content, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Content")
ggsave("../graphs/veridicality-means-bycontent.pdf",height=8,width=6)

summary(t)

# plot contradictoriness rating by participant
variances = t %>%
  group_by(workerid) %>%
  summarise(VeriVar = var(response),
            VeriMean=mean(response),
            Veri.ci.low=ci.low(response),
            Veri.ci.high=ci.high(response))

variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,VeriMean),y=VeriMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean contradictoriness rating")
ggsave("../graphs/veridicality-subjmeans.pdf",height=3,width=6.5)

# plot contradictoriness rating by participant for entailing predicates only
table(t$verb)
entailing = droplevels(subset(t,t$verb == "know" | t$verb == "be_right" 
                              | t$verb == "be_annoyed" | t$verb == "reveal" 
                              | t$verb == "discover" | t$verb == "see" | t$verb == "establish"))
nrow(entailing) #1883 = 269 participants x 7 entailing verbs

variances = entailing %>%
  group_by(workerid) %>%
  summarise(VeriVar = var(response),
            VeriMean=mean(response),
            Veri.ci.low=ci.low(response),
            Veri.ci.high=ci.high(response))

variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,VeriMean),y=VeriMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean contradictoriness rating")
ggsave("../graphs/veridicality-entailing-subjmeans.pdf",height=3,width=6.5)

# plot veridicality rating by age (continous)
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
  ylab("Mean veridicality rating") +
  xlab("Participant age") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/age-continuuous-collapsed.pdf")

# veridicality rating by participant gender
means = t %>%
  group_by(gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

means

ggplot(means, aes(y=Mean, x=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean veridicality rating") +
  xlab("Participant gender") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/gender-collapsed.pdf")

## models -----
library(emmeans)
library(lme4)
library(languageR)
library(brms)
str(cd$response)
str(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)
cd$verb <- as.factor(as.character(cd$verb))

table(cd$verb)
table(cd$content)
# create item as combination of verb and content of complement
cd$item = as.factor(paste(cd$verb, cd$content))
table(cd$item)

# predict inference rating from predicate, with entailing controls as reference level, to see which 
# CC is entailed
cd$verb <- relevel(cd$verb, ref = "contradictory C")

## Bayesian models ----
run_model <- function(expr, path, reuse = TRUE) {
  path <- paste0(path, ".Rds")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}

fit <- run_model(brm(response ~ verb + (1|workerid) + (1|item), data=cd, family=gaussian()), path = "../models/predict-response-from-verb-no-slopes.Rds")
tmp <- readRDS('../models/predict-response-from-verb-no-slopes.Rds.Rds')
summary(tmp) # converged

fit <- run_model(brm(response ~ verb + (verb|workerid) + (1|item), data=cd, family=gaussian()), path = "../models/predict-response-from-verb-with-slope.Rds")
tmp <- readRDS('../models/predict-response-from-verb-with-slope.Rds.Rds')
summary(tmp) # converged


## pairwise comparison to see which predicates differ from one another
library(lsmeans)
library(lme4)
str(t$response)
str(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
model = lmer(response ~ verb + (1|workerid), data=t, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# contrast                       estimate         SE   df t.ratio p.value
# think - pretend           -0.0459851301 0.02217568 5111  -2.074  0.8704
# think - hear              -0.0664312268 0.02217568 5111  -2.996  0.2341
# think - suggest           -0.0847583643 0.02217568 5111  -3.822  0.0192
# think - say               -0.1945353160 0.02217568 5111  -8.772  <.0001
# think - announce          -0.2778438662 0.02217568 5111 -12.529  <.0001
# think - inform            -0.2972118959 0.02217568 5111 -13.403  <.0001
# think - reveal            -0.4565799257 0.02217568 5111 -20.589  <.0001
# think - be_annoyed        -0.4630483271 0.02217568 5111 -20.881  <.0001
# think - acknowledge       -0.4948698885 0.02217568 5111 -22.316  <.0001
# think - confess           -0.4952788104 0.02217568 5111 -22.334  <.0001
# think - admit             -0.5370631970 0.02217568 5111 -24.219  <.0001
# think - demonstrate       -0.5457992565 0.02217568 5111 -24.613  <.0001
# think - establish         -0.5464684015 0.02217568 5111 -24.643  <.0001
# think - confirm           -0.5908921933 0.02217568 5111 -26.646  <.0001
# think - discover          -0.6081412639 0.02217568 5111 -27.424  <.0001
# think - see               -0.6419702602 0.02217568 5111 -28.949  <.0001
# think - know              -0.6611895911 0.02217568 5111 -29.816  <.0001
# think - prove             -0.6810408922 0.02217568 5111 -30.711  <.0001
# think - be_right          -0.7620074349 0.02217568 5111 -34.362  <.0001
# pretend - hear            -0.0204460967 0.02217568 5111  -0.922  1.0000
# pretend - suggest         -0.0387732342 0.02217568 5111  -1.748  0.9723
# pretend - say             -0.1485501859 0.02217568 5111  -6.699  <.0001
# pretend - announce        -0.2318587361 0.02217568 5111 -10.456  <.0001
# pretend - inform          -0.2512267658 0.02217568 5111 -11.329  <.0001
# pretend - reveal          -0.4105947955 0.02217568 5111 -18.516  <.0001
# pretend - be_annoyed      -0.4170631970 0.02217568 5111 -18.807  <.0001
# pretend - acknowledge     -0.4488847584 0.02217568 5111 -20.242  <.0001
# pretend - confess         -0.4492936803 0.02217568 5111 -20.261  <.0001
# pretend - admit           -0.4910780669 0.02217568 5111 -22.145  <.0001
# pretend - demonstrate     -0.4998141264 0.02217568 5111 -22.539  <.0001
# pretend - establish       -0.5004832714 0.02217568 5111 -22.569  <.0001
# pretend - confirm         -0.5449070632 0.02217568 5111 -24.572  <.0001
# pretend - discover        -0.5621561338 0.02217568 5111 -25.350  <.0001
# pretend - see             -0.5959851301 0.02217568 5111 -26.876  <.0001
# pretend - know            -0.6152044610 0.02217568 5111 -27.742  <.0001
# pretend - prove           -0.6350557621 0.02217568 5111 -28.637  <.0001
# pretend - be_right        -0.7160223048 0.02217568 5111 -32.289  <.0001
# hear - suggest            -0.0183271375 0.02217568 5111  -0.826  1.0000
# hear - say                -0.1281040892 0.02217568 5111  -5.777  <.0001
# hear - announce           -0.2114126394 0.02217568 5111  -9.534  <.0001
# hear - inform             -0.2307806691 0.02217568 5111 -10.407  <.0001
# hear - reveal             -0.3901486989 0.02217568 5111 -17.594  <.0001
# hear - be_annoyed         -0.3966171004 0.02217568 5111 -17.885  <.0001
# hear - acknowledge        -0.4284386617 0.02217568 5111 -19.320  <.0001
# hear - confess            -0.4288475836 0.02217568 5111 -19.339  <.0001
# hear - admit              -0.4706319703 0.02217568 5111 -21.223  <.0001
# hear - demonstrate        -0.4793680297 0.02217568 5111 -21.617  <.0001
# hear - establish          -0.4800371747 0.02217568 5111 -21.647  <.0001
# hear - confirm            -0.5244609665 0.02217568 5111 -23.650  <.0001
# hear - discover           -0.5417100372 0.02217568 5111 -24.428  <.0001
# hear - see                -0.5755390335 0.02217568 5111 -25.954  <.0001
# hear - know               -0.5947583643 0.02217568 5111 -26.820  <.0001
# hear - prove              -0.6146096654 0.02217568 5111 -27.715  <.0001
# hear - be_right           -0.6955762082 0.02217568 5111 -31.367  <.0001
# suggest - say             -0.1097769517 0.02217568 5111  -4.950  0.0001
# suggest - announce        -0.1930855019 0.02217568 5111  -8.707  <.0001
# suggest - inform          -0.2124535316 0.02217568 5111  -9.580  <.0001
# suggest - reveal          -0.3718215613 0.02217568 5111 -16.767  <.0001
# suggest - be_annoyed      -0.3782899628 0.02217568 5111 -17.059  <.0001
# suggest - acknowledge     -0.4101115242 0.02217568 5111 -18.494  <.0001
# suggest - confess         -0.4105204461 0.02217568 5111 -18.512  <.0001
# suggest - admit           -0.4523048327 0.02217568 5111 -20.396  <.0001
# suggest - demonstrate     -0.4610408922 0.02217568 5111 -20.790  <.0001
# suggest - establish       -0.4617100372 0.02217568 5111 -20.821  <.0001
# suggest - confirm         -0.5061338290 0.02217568 5111 -22.824  <.0001
# suggest - discover        -0.5233828996 0.02217568 5111 -23.602  <.0001
# suggest - see             -0.5572118959 0.02217568 5111 -25.127  <.0001
# suggest - know            -0.5764312268 0.02217568 5111 -25.994  <.0001
# suggest - prove           -0.5962825279 0.02217568 5111 -26.889  <.0001
# suggest - be_right        -0.6772490706 0.02217568 5111 -30.540  <.0001
# say - announce            -0.0833085502 0.02217568 5111  -3.757  0.0243
# say - inform              -0.1026765799 0.02217568 5111  -4.630  0.0007
# say - reveal              -0.2620446097 0.02217568 5111 -11.817  <.0001
# say - be_annoyed          -0.2685130112 0.02217568 5111 -12.108  <.0001
# say - acknowledge         -0.3003345725 0.02217568 5111 -13.543  <.0001
# say - confess             -0.3007434944 0.02217568 5111 -13.562  <.0001
# say - admit               -0.3425278810 0.02217568 5111 -15.446  <.0001
# say - demonstrate         -0.3512639405 0.02217568 5111 -15.840  <.0001
# say - establish           -0.3519330855 0.02217568 5111 -15.870  <.0001
# say - confirm             -0.3963568773 0.02217568 5111 -17.873  <.0001
# say - discover            -0.4136059480 0.02217568 5111 -18.651  <.0001
# say - see                 -0.4474349442 0.02217568 5111 -20.177  <.0001
# say - know                -0.4666542751 0.02217568 5111 -21.044  <.0001
# say - prove               -0.4865055762 0.02217568 5111 -21.939  <.0001
# say - be_right            -0.5674721190 0.02217568 5111 -25.590  <.0001
# announce - inform         -0.0193680297 0.02217568 5111  -0.873  1.0000
# announce - reveal         -0.1787360595 0.02217568 5111  -8.060  <.0001
# announce - be_annoyed     -0.1852044610 0.02217568 5111  -8.352  <.0001
# announce - acknowledge    -0.2170260223 0.02217568 5111  -9.787  <.0001
# announce - confess        -0.2174349442 0.02217568 5111  -9.805  <.0001
# announce - admit          -0.2592193309 0.02217568 5111 -11.689  <.0001
# announce - demonstrate    -0.2679553903 0.02217568 5111 -12.083  <.0001
# announce - establish      -0.2686245353 0.02217568 5111 -12.113  <.0001
# announce - confirm        -0.3130483271 0.02217568 5111 -14.117  <.0001
# announce - discover       -0.3302973978 0.02217568 5111 -14.895  <.0001
# announce - see            -0.3641263941 0.02217568 5111 -16.420  <.0001
# announce - know           -0.3833457249 0.02217568 5111 -17.287  <.0001
# announce - prove          -0.4031970260 0.02217568 5111 -18.182  <.0001
# announce - be_right       -0.4841635688 0.02217568 5111 -21.833  <.0001
# inform - reveal           -0.1593680297 0.02217568 5111  -7.187  <.0001
# inform - be_annoyed       -0.1658364312 0.02217568 5111  -7.478  <.0001
# inform - acknowledge      -0.1976579926 0.02217568 5111  -8.913  <.0001
# inform - confess          -0.1980669145 0.02217568 5111  -8.932  <.0001
# inform - admit            -0.2398513011 0.02217568 5111 -10.816  <.0001
# inform - demonstrate      -0.2485873606 0.02217568 5111 -11.210  <.0001
# inform - establish        -0.2492565056 0.02217568 5111 -11.240  <.0001
# inform - confirm          -0.2936802974 0.02217568 5111 -13.243  <.0001
# inform - discover         -0.3109293680 0.02217568 5111 -14.021  <.0001
# inform - see              -0.3447583643 0.02217568 5111 -15.547  <.0001
# inform - know             -0.3639776952 0.02217568 5111 -16.413  <.0001
# inform - prove            -0.3838289963 0.02217568 5111 -17.309  <.0001
# inform - be_right         -0.4647955390 0.02217568 5111 -20.960  <.0001
# reveal - be_annoyed       -0.0064684015 0.02217568 5111  -0.292  1.0000
# reveal - acknowledge      -0.0382899628 0.02217568 5111  -1.727  0.9756
# reveal - confess          -0.0386988848 0.02217568 5111  -1.745  0.9728
# reveal - admit            -0.0804832714 0.02217568 5111  -3.629  0.0379
# reveal - demonstrate      -0.0892193309 0.02217568 5111  -4.023  0.0089
# reveal - establish        -0.0898884758 0.02217568 5111  -4.053  0.0079
# reveal - confirm          -0.1343122677 0.02217568 5111  -6.057  <.0001
# reveal - discover         -0.1515613383 0.02217568 5111  -6.835  <.0001
# reveal - see              -0.1853903346 0.02217568 5111  -8.360  <.0001
# reveal - know             -0.2046096654 0.02217568 5111  -9.227  <.0001
# reveal - prove            -0.2244609665 0.02217568 5111 -10.122  <.0001
# reveal - be_right         -0.3054275093 0.02217568 5111 -13.773  <.0001
# be_annoyed - acknowledge  -0.0318215613 0.02217568 5111  -1.435  0.9972
# be_annoyed - confess      -0.0322304833 0.02217568 5111  -1.453  0.9967
# be_annoyed - admit        -0.0740148699 0.02217568 5111  -3.338  0.0952
# be_annoyed - demonstrate  -0.0827509294 0.02217568 5111  -3.732  0.0266
# be_annoyed - establish    -0.0834200743 0.02217568 5111  -3.762  0.0239
# be_annoyed - confirm      -0.1278438662 0.02217568 5111  -5.765  <.0001
# be_annoyed - discover     -0.1450929368 0.02217568 5111  -6.543  <.0001
# be_annoyed - see          -0.1789219331 0.02217568 5111  -8.068  <.0001
# be_annoyed - know         -0.1981412639 0.02217568 5111  -8.935  <.0001
# be_annoyed - prove        -0.2179925651 0.02217568 5111  -9.830  <.0001
# be_annoyed - be_right     -0.2989591078 0.02217568 5111 -13.481  <.0001
# acknowledge - confess     -0.0004089219 0.02217568 5111  -0.018  1.0000
# acknowledge - admit       -0.0421933086 0.02217568 5111  -1.903  0.9371
# acknowledge - demonstrate -0.0509293680 0.02217568 5111  -2.297  0.7371
# acknowledge - establish   -0.0515985130 0.02217568 5111  -2.327  0.7157
# acknowledge - confirm     -0.0960223048 0.02217568 5111  -4.330  0.0025
# acknowledge - discover    -0.1132713755 0.02217568 5111  -5.108  0.0001
# acknowledge - see         -0.1471003717 0.02217568 5111  -6.633  <.0001
# acknowledge - know        -0.1663197026 0.02217568 5111  -7.500  <.0001
# acknowledge - prove       -0.1861710037 0.02217568 5111  -8.395  <.0001
# acknowledge - be_right    -0.2671375465 0.02217568 5111 -12.046  <.0001
# confess - admit           -0.0417843866 0.02217568 5111  -1.884  0.9425
# confess - demonstrate     -0.0505204461 0.02217568 5111  -2.278  0.7499
# confess - establish       -0.0511895911 0.02217568 5111  -2.308  0.7289
# confess - confirm         -0.0956133829 0.02217568 5111  -4.312  0.0027
# confess - discover        -0.1128624535 0.02217568 5111  -5.089  0.0001
# confess - see             -0.1466914498 0.02217568 5111  -6.615  <.0001
# confess - know            -0.1659107807 0.02217568 5111  -7.482  <.0001
# confess - prove           -0.1857620818 0.02217568 5111  -8.377  <.0001
# confess - be_right        -0.2667286245 0.02217568 5111 -12.028  <.0001
# admit - demonstrate       -0.0087360595 0.02217568 5111  -0.394  1.0000
# admit - establish         -0.0094052045 0.02217568 5111  -0.424  1.0000
# admit - confirm           -0.0538289963 0.02217568 5111  -2.427  0.6406
# admit - discover          -0.0710780669 0.02217568 5111  -3.205  0.1382
# admit - see               -0.1049070632 0.02217568 5111  -4.731  0.0004
# admit - know              -0.1241263941 0.02217568 5111  -5.597  <.0001
# admit - prove             -0.1439776952 0.02217568 5111  -6.493  <.0001
# admit - be_right          -0.2249442379 0.02217568 5111 -10.144  <.0001
# demonstrate - establish   -0.0006691450 0.02217568 5111  -0.030  1.0000
# demonstrate - confirm     -0.0450929368 0.02217568 5111  -2.033  0.8889
# demonstrate - discover    -0.0623420074 0.02217568 5111  -2.811  0.3481
# demonstrate - see         -0.0961710037 0.02217568 5111  -4.337  0.0024
# demonstrate - know        -0.1153903346 0.02217568 5111  -5.203  <.0001
# demonstrate - prove       -0.1352416357 0.02217568 5111  -6.099  <.0001
# demonstrate - be_right    -0.2162081784 0.02217568 5111  -9.750  <.0001
# establish - confirm       -0.0444237918 0.02217568 5111  -2.003  0.9017
# establish - discover      -0.0616728625 0.02217568 5111  -2.781  0.3691
# establish - see           -0.0955018587 0.02217568 5111  -4.307  0.0028
# establish - know          -0.1147211896 0.02217568 5111  -5.173  <.0001
# establish - prove         -0.1345724907 0.02217568 5111  -6.068  <.0001
# establish - be_right      -0.2155390335 0.02217568 5111  -9.720  <.0001
# confirm - discover        -0.0172490706 0.02217568 5111  -0.778  1.0000
# confirm - see             -0.0510780669 0.02217568 5111  -2.303  0.7324
# confirm - know            -0.0702973978 0.02217568 5111  -3.170  0.1518
# confirm - prove           -0.0901486989 0.02217568 5111  -4.065  0.0076
# confirm - be_right        -0.1711152416 0.02217568 5111  -7.716  <.0001
# discover - see            -0.0338289963 0.02217568 5111  -1.525  0.9940
# discover - know           -0.0530483271 0.02217568 5111  -2.392  0.6674
# discover - prove          -0.0728996283 0.02217568 5111  -3.287  0.1100
# discover - be_right       -0.1538661710 0.02217568 5111  -6.939  <.0001
# see - know                -0.0192193309 0.02217568 5111  -0.867  1.0000
# see - prove               -0.0390706320 0.02217568 5111  -1.762  0.9700
# see - be_right            -0.1200371747 0.02217568 5111  -5.413  <.0001
# know - prove              -0.0198513011 0.02217568 5111  -0.895  1.0000
# know - be_right           -0.1008178439 0.02217568 5111  -4.546  0.0010
# prove - be_right          -0.0809665428 0.02217568 5111  -3.651  0.0352

## pairwise comparison to see which predicates differ from one another
## including the contradictory control stimuli

library(lsmeans)
library(lme4)

str(te$response)
str(te$verb)
str(te$workerid)
te$workerid <- as.factor(te$workerid)

means = te %>%
  group_by(verb) %>%
  summarize(Mean = mean(response)) %>%
  select(verb,Mean)
means = as.data.frame(means)

te$verb <-factor(te$verb, levels=means[order(means$Mean), "verb"])

model = lmer(response ~ verb + (1|workerid), data=te, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                           estimate         SE   df t.ratio p.value
# think - pretend               -0.0459851301 0.02149525 6187  -2.139  0.8546
# think - hear                  -0.0664312268 0.02149525 6187  -3.091  0.1995
# think - suggest               -0.0847583643 0.02149525 6187  -3.943  0.0133
# think - say                   -0.1945353160 0.02149525 6187  -9.050  <.0001
# think - announce              -0.2778438662 0.02149525 6187 -12.926  <.0001
# think - inform                -0.2972118959 0.02149525 6187 -13.827  <.0001
# think - reveal                -0.4565799257 0.02149525 6187 -21.241  <.0001
# think - be_annoyed            -0.4630483271 0.02149525 6187 -21.542  <.0001
# think - acknowledge           -0.4948698885 0.02149525 6187 -23.022  <.0001
# think - confess               -0.4952788104 0.02149525 6187 -23.041  <.0001
# think - admit                 -0.5370631970 0.02149525 6187 -24.985  <.0001
# think - demonstrate           -0.5457992565 0.02149525 6187 -25.392  <.0001
# think - establish             -0.5464684015 0.02149525 6187 -25.423  <.0001
# think - confirm               -0.5908921933 0.02149525 6187 -27.489  <.0001
# think - discover              -0.6081412639 0.02149525 6187 -28.292  <.0001
# think - see                   -0.6419702602 0.02149525 6187 -29.866  <.0001
# think - know                  -0.6611895911 0.02149525 6187 -30.760  <.0001
# think - prove                 -0.6810408922 0.02149525 6187 -31.683  <.0001
# think - be_right              -0.7620074349 0.02149525 6187 -35.450  <.0001
# think - contradictory C       -0.7817286245 0.01699349 6187 -46.002  <.0001
# pretend - hear                -0.0204460967 0.02149525 6187  -0.951  1.0000
# pretend - suggest             -0.0387732342 0.02149525 6187  -1.804  0.9690
# pretend - say                 -0.1485501859 0.02149525 6187  -6.911  <.0001
# pretend - announce            -0.2318587361 0.02149525 6187 -10.787  <.0001
# pretend - inform              -0.2512267658 0.02149525 6187 -11.688  <.0001
# pretend - reveal              -0.4105947955 0.02149525 6187 -19.102  <.0001
# pretend - be_annoyed          -0.4170631970 0.02149525 6187 -19.403  <.0001
# pretend - acknowledge         -0.4488847584 0.02149525 6187 -20.883  <.0001
# pretend - confess             -0.4492936803 0.02149525 6187 -20.902  <.0001
# pretend - admit               -0.4910780669 0.02149525 6187 -22.846  <.0001
# pretend - demonstrate         -0.4998141264 0.02149525 6187 -23.252  <.0001
# pretend - establish           -0.5004832714 0.02149525 6187 -23.283  <.0001
# pretend - confirm             -0.5449070632 0.02149525 6187 -25.350  <.0001
# pretend - discover            -0.5621561338 0.02149525 6187 -26.153  <.0001
# pretend - see                 -0.5959851301 0.02149525 6187 -27.726  <.0001
# pretend - know                -0.6152044610 0.02149525 6187 -28.620  <.0001
# pretend - prove               -0.6350557621 0.02149525 6187 -29.544  <.0001
# pretend - be_right            -0.7160223048 0.02149525 6187 -33.311  <.0001
# pretend - contradictory C     -0.7357434944 0.01699349 6187 -43.296  <.0001
# hear - suggest                -0.0183271375 0.02149525 6187  -0.853  1.0000
# hear - say                    -0.1281040892 0.02149525 6187  -5.960  <.0001
# hear - announce               -0.2114126394 0.02149525 6187  -9.835  <.0001
# hear - inform                 -0.2307806691 0.02149525 6187 -10.736  <.0001
# hear - reveal                 -0.3901486989 0.02149525 6187 -18.150  <.0001
# hear - be_annoyed             -0.3966171004 0.02149525 6187 -18.451  <.0001
# hear - acknowledge            -0.4284386617 0.02149525 6187 -19.932  <.0001
# hear - confess                -0.4288475836 0.02149525 6187 -19.951  <.0001
# hear - admit                  -0.4706319703 0.02149525 6187 -21.895  <.0001
# hear - demonstrate            -0.4793680297 0.02149525 6187 -22.301  <.0001
# hear - establish              -0.4800371747 0.02149525 6187 -22.332  <.0001
# hear - confirm                -0.5244609665 0.02149525 6187 -24.399  <.0001
# hear - discover               -0.5417100372 0.02149525 6187 -25.201  <.0001
# hear - see                    -0.5755390335 0.02149525 6187 -26.775  <.0001
# hear - know                   -0.5947583643 0.02149525 6187 -27.669  <.0001
# hear - prove                  -0.6146096654 0.02149525 6187 -28.593  <.0001
# hear - be_right               -0.6955762082 0.02149525 6187 -32.360  <.0001
# hear - contradictory C        -0.7152973978 0.01699349 6187 -42.092  <.0001
# suggest - say                 -0.1097769517 0.02149525 6187  -5.107  0.0001
# suggest - announce            -0.1930855019 0.02149525 6187  -8.983  <.0001
# suggest - inform              -0.2124535316 0.02149525 6187  -9.884  <.0001
# suggest - reveal              -0.3718215613 0.02149525 6187 -17.298  <.0001
# suggest - be_annoyed          -0.3782899628 0.02149525 6187 -17.599  <.0001
# suggest - acknowledge         -0.4101115242 0.02149525 6187 -19.079  <.0001
# suggest - confess             -0.4105204461 0.02149525 6187 -19.098  <.0001
# suggest - admit               -0.4523048327 0.02149525 6187 -21.042  <.0001
# suggest - demonstrate         -0.4610408922 0.02149525 6187 -21.449  <.0001
# suggest - establish           -0.4617100372 0.02149525 6187 -21.480  <.0001
# suggest - confirm             -0.5061338290 0.02149525 6187 -23.546  <.0001
# suggest - discover            -0.5233828996 0.02149525 6187 -24.349  <.0001
# suggest - see                 -0.5572118959 0.02149525 6187 -25.923  <.0001
# suggest - know                -0.5764312268 0.02149525 6187 -26.817  <.0001
# suggest - prove               -0.5962825279 0.02149525 6187 -27.740  <.0001
# suggest - be_right            -0.6772490706 0.02149525 6187 -31.507  <.0001
# suggest - contradictory C     -0.6969702602 0.01699349 6187 -41.014  <.0001
# say - announce                -0.0833085502 0.02149525 6187  -3.876  0.0172
# say - inform                  -0.1026765799 0.02149525 6187  -4.777  0.0004
# say - reveal                  -0.2620446097 0.02149525 6187 -12.191  <.0001
# say - be_annoyed              -0.2685130112 0.02149525 6187 -12.492  <.0001
# say - acknowledge             -0.3003345725 0.02149525 6187 -13.972  <.0001
# say - confess                 -0.3007434944 0.02149525 6187 -13.991  <.0001
# say - admit                   -0.3425278810 0.02149525 6187 -15.935  <.0001
# say - demonstrate             -0.3512639405 0.02149525 6187 -16.341  <.0001
# say - establish               -0.3519330855 0.02149525 6187 -16.373  <.0001
# say - confirm                 -0.3963568773 0.02149525 6187 -18.439  <.0001
# say - discover                -0.4136059480 0.02149525 6187 -19.242  <.0001
# say - see                     -0.4474349442 0.02149525 6187 -20.816  <.0001
# say - know                    -0.4666542751 0.02149525 6187 -21.710  <.0001
# say - prove                   -0.4865055762 0.02149525 6187 -22.633  <.0001
# say - be_right                -0.5674721190 0.02149525 6187 -26.400  <.0001
# say - contradictory C         -0.5871933086 0.01699349 6187 -34.554  <.0001
# announce - inform             -0.0193680297 0.02149525 6187  -0.901  1.0000
# announce - reveal             -0.1787360595 0.02149525 6187  -8.315  <.0001
# announce - be_annoyed         -0.1852044610 0.02149525 6187  -8.616  <.0001
# announce - acknowledge        -0.2170260223 0.02149525 6187 -10.096  <.0001
# announce - confess            -0.2174349442 0.02149525 6187 -10.115  <.0001
# announce - admit              -0.2592193309 0.02149525 6187 -12.059  <.0001
# announce - demonstrate        -0.2679553903 0.02149525 6187 -12.466  <.0001
# announce - establish          -0.2686245353 0.02149525 6187 -12.497  <.0001
# announce - confirm            -0.3130483271 0.02149525 6187 -14.564  <.0001
# announce - discover           -0.3302973978 0.02149525 6187 -15.366  <.0001
# announce - see                -0.3641263941 0.02149525 6187 -16.940  <.0001
# announce - know               -0.3833457249 0.02149525 6187 -17.834  <.0001
# announce - prove              -0.4031970260 0.02149525 6187 -18.757  <.0001
# announce - be_right           -0.4841635688 0.02149525 6187 -22.524  <.0001
# announce - contradictory C    -0.5038847584 0.01699349 6187 -29.652  <.0001
# inform - reveal               -0.1593680297 0.02149525 6187  -7.414  <.0001
# inform - be_annoyed           -0.1658364312 0.02149525 6187  -7.715  <.0001
# inform - acknowledge          -0.1976579926 0.02149525 6187  -9.195  <.0001
# inform - confess              -0.1980669145 0.02149525 6187  -9.214  <.0001
# inform - admit                -0.2398513011 0.02149525 6187 -11.158  <.0001
# inform - demonstrate          -0.2485873606 0.02149525 6187 -11.565  <.0001
# inform - establish            -0.2492565056 0.02149525 6187 -11.596  <.0001
# inform - confirm              -0.2936802974 0.02149525 6187 -13.663  <.0001
# inform - discover             -0.3109293680 0.02149525 6187 -14.465  <.0001
# inform - see                  -0.3447583643 0.02149525 6187 -16.039  <.0001
# inform - know                 -0.3639776952 0.02149525 6187 -16.933  <.0001
# inform - prove                -0.3838289963 0.02149525 6187 -17.856  <.0001
# inform - be_right             -0.4647955390 0.02149525 6187 -21.623  <.0001
# inform - contradictory C      -0.4845167286 0.01699349 6187 -28.512  <.0001
# reveal - be_annoyed           -0.0064684015 0.02149525 6187  -0.301  1.0000
# reveal - acknowledge          -0.0382899628 0.02149525 6187  -1.781  0.9729
# reveal - confess              -0.0386988848 0.02149525 6187  -1.800  0.9697
# reveal - admit                -0.0804832714 0.02149525 6187  -3.744  0.0277
# reveal - demonstrate          -0.0892193309 0.02149525 6187  -4.151  0.0059
# reveal - establish            -0.0898884758 0.02149525 6187  -4.182  0.0052
# reveal - confirm              -0.1343122677 0.02149525 6187  -6.248  <.0001
# reveal - discover             -0.1515613383 0.02149525 6187  -7.051  <.0001
# reveal - see                  -0.1853903346 0.02149525 6187  -8.625  <.0001
# reveal - know                 -0.2046096654 0.02149525 6187  -9.519  <.0001
# reveal - prove                -0.2244609665 0.02149525 6187 -10.442  <.0001
# reveal - be_right             -0.3054275093 0.02149525 6187 -14.209  <.0001
# reveal - contradictory C      -0.3251486989 0.01699349 6187 -19.134  <.0001
# be_annoyed - acknowledge      -0.0318215613 0.02149525 6187  -1.480  0.9970
# be_annoyed - confess          -0.0322304833 0.02149525 6187  -1.499  0.9964
# be_annoyed - admit            -0.0740148699 0.02149525 6187  -3.443  0.0749
# be_annoyed - demonstrate      -0.0827509294 0.02149525 6187  -3.850  0.0189
# be_annoyed - establish        -0.0834200743 0.02149525 6187  -3.881  0.0168
# be_annoyed - confirm          -0.1278438662 0.02149525 6187  -5.948  <.0001
# be_annoyed - discover         -0.1450929368 0.02149525 6187  -6.750  <.0001
# be_annoyed - see              -0.1789219331 0.02149525 6187  -8.324  <.0001
# be_annoyed - know             -0.1981412639 0.02149525 6187  -9.218  <.0001
# be_annoyed - prove            -0.2179925651 0.02149525 6187 -10.141  <.0001
# be_annoyed - be_right         -0.2989591078 0.02149525 6187 -13.908  <.0001
# be_annoyed - contradictory C  -0.3186802974 0.01699349 6187 -18.753  <.0001
# acknowledge - confess         -0.0004089219 0.02149525 6187  -0.019  1.0000
# acknowledge - admit           -0.0421933086 0.02149525 6187  -1.963  0.9294
# acknowledge - demonstrate     -0.0509293680 0.02149525 6187  -2.369  0.7085
# acknowledge - establish       -0.0515985130 0.02149525 6187  -2.400  0.6855
# acknowledge - confirm         -0.0960223048 0.02149525 6187  -4.467  0.0015
# acknowledge - discover        -0.1132713755 0.02149525 6187  -5.270  <.0001
# acknowledge - see             -0.1471003717 0.02149525 6187  -6.843  <.0001
# acknowledge - know            -0.1663197026 0.02149525 6187  -7.738  <.0001
# acknowledge - prove           -0.1861710037 0.02149525 6187  -8.661  <.0001
# acknowledge - be_right        -0.2671375465 0.02149525 6187 -12.428  <.0001
# acknowledge - contradictory C -0.2868587361 0.01699349 6187 -16.881  <.0001
# confess - admit               -0.0417843866 0.02149525 6187  -1.944  0.9354
# confess - demonstrate         -0.0505204461 0.02149525 6187  -2.350  0.7223
# confess - establish           -0.0511895911 0.02149525 6187  -2.381  0.6997
# confess - confirm             -0.0956133829 0.02149525 6187  -4.448  0.0016
# confess - discover            -0.1128624535 0.02149525 6187  -5.251  <.0001
# confess - see                 -0.1466914498 0.02149525 6187  -6.824  <.0001
# confess - know                -0.1659107807 0.02149525 6187  -7.718  <.0001
# confess - prove               -0.1857620818 0.02149525 6187  -8.642  <.0001
# confess - be_right            -0.2667286245 0.02149525 6187 -12.409  <.0001
# confess - contradictory C     -0.2864498141 0.01699349 6187 -16.856  <.0001
# admit - demonstrate           -0.0087360595 0.02149525 6187  -0.406  1.0000
# admit - establish             -0.0094052045 0.02149525 6187  -0.438  1.0000
# admit - confirm               -0.0538289963 0.02149525 6187  -2.504  0.6054
# admit - discover              -0.0710780669 0.02149525 6187  -3.307  0.1123
# admit - see                   -0.1049070632 0.02149525 6187  -4.880  0.0002
# admit - know                  -0.1241263941 0.02149525 6187  -5.775  <.0001
# admit - prove                 -0.1439776952 0.02149525 6187  -6.698  <.0001
# admit - be_right              -0.2249442379 0.02149525 6187 -10.465  <.0001
# admit - contradictory C       -0.2446654275 0.01699349 6187 -14.398  <.0001
# demonstrate - establish       -0.0006691450 0.02149525 6187  -0.031  1.0000
# demonstrate - confirm         -0.0450929368 0.02149525 6187  -2.098  0.8753
# demonstrate - discover        -0.0623420074 0.02149525 6187  -2.900  0.3081
# demonstrate - see             -0.0961710037 0.02149525 6187  -4.474  0.0015
# demonstrate - know            -0.1153903346 0.02149525 6187  -5.368  <.0001
# demonstrate - prove           -0.1352416357 0.02149525 6187  -6.292  <.0001
# demonstrate - be_right        -0.2162081784 0.02149525 6187 -10.058  <.0001
# demonstrate - contradictory C -0.2359293680 0.01699349 6187 -13.884  <.0001
# establish - confirm           -0.0444237918 0.02149525 6187  -2.067  0.8896
# establish - discover          -0.0616728625 0.02149525 6187  -2.869  0.3287
# establish - see               -0.0955018587 0.02149525 6187  -4.443  0.0017
# establish - know              -0.1147211896 0.02149525 6187  -5.337  <.0001
# establish - prove             -0.1345724907 0.02149525 6187  -6.261  <.0001
# establish - be_right          -0.2155390335 0.02149525 6187 -10.027  <.0001
# establish - contradictory C   -0.2352602230 0.01699349 6187 -13.844  <.0001
# confirm - discover            -0.0172490706 0.02149525 6187  -0.802  1.0000
# confirm - see                 -0.0510780669 0.02149525 6187  -2.376  0.7035
# confirm - know                -0.0702973978 0.02149525 6187  -3.270  0.1244
# confirm - prove               -0.0901486989 0.02149525 6187  -4.194  0.0049
# confirm - be_right            -0.1711152416 0.02149525 6187  -7.961  <.0001
# confirm - contradictory C     -0.1908364312 0.01699349 6187 -11.230  <.0001
# discover - see                -0.0338289963 0.02149525 6187  -1.574  0.9934
# discover - know               -0.0530483271 0.02149525 6187  -2.468  0.6339
# discover - prove              -0.0728996283 0.02149525 6187  -3.391  0.0877
# discover - be_right           -0.1538661710 0.02149525 6187  -7.158  <.0001
# discover - contradictory C    -0.1735873606 0.01699349 6187 -10.215  <.0001
# see - know                    -0.0192193309 0.02149525 6187  -0.894  1.0000
# see - prove                   -0.0390706320 0.02149525 6187  -1.818  0.9665
# see - be_right                -0.1200371747 0.02149525 6187  -5.584  <.0001
# see - contradictory C         -0.1397583643 0.01699349 6187  -8.224  <.0001
# know - prove                  -0.0198513011 0.02149525 6187  -0.924  1.0000
# know - be_right               -0.1008178439 0.02149525 6187  -4.690  0.0005
# know - contradictory C        -0.1205390335 0.01699349 6187  -7.093  <.0001
# prove - be_right              -0.0809665428 0.02149525 6187  -3.767  0.0256
# prove - contradictory C       -0.1006877323 0.01699349 6187  -5.925  <.0001
# be_right - contradictory C    -0.0197211896 0.01699349 6187  -1.161  0.9999
# 
# P value adjustment: tukey method for comparing a family of 21 estimates

# JD CODE STARTS HERE ----
# TL;DR: all verbs are different from contradictory, i.e., entailing, controls
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# Bayesian mixed effects regression to test whether ratings differ by predicate from contradictory controls
# contradictory controls are called control_bad
cd$workerid = as.factor(as.character(cd$workerid))

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response)) +
  geom_histogram()

# exclude non-contracictory controls (called control_good) from analysis -- they're not relevant, right? (JT: right)
d = cd %>%
  filter(verb != "control_good") %>%
  droplevels() %>%
  mutate(verb = fct_relevel(verb,"control_bad"))

# zoib model
# zoib_model <- bf(
#   response ~ verb, # beta distribution???s mean
#   phi ~ verb, # beta distribution???s precision
#   zoi ~ verb, # zero-one inflation (alpha); ie, probability of a binary rating as a function of verb
#   coi ~ verb, # conditional one-inflation
#   family = zero_one_inflated_beta()
# )
# 
# # fit model
# m <- brm(
#   formula = zoib_model,
#   data = d,
#   cores = 4#,
#   # file = here::here("zoib-ex")
# )
# save model (no need to run this multiple times); JT commented this out to prevent accidential save of something else
#saveRDS(m,file="../data/zoib-model.rds")

# load ZOIB model ----
m <- readRDS(file="../data/zoib-model.rds")

summary(m) # see summary printed below

# transform each of the posterior samples, and then re-calculate the summaries on original scale
posterior_samples(m, pars = "b_")[,1:4] %>% 
  mutate_at(c("b_phi_Intercept"), exp) %>% 
  mutate_at(vars(-"b_phi_Intercept"), plogis) %>% 
  posterior_summary() %>% 
  as.data.frame() %>% 
  rownames_to_column("Parameter") %>% 
  kable(digits = 2) 

# this gives us the summary of the contradictory control condition:
# |Parameter       | Estimate| Est.Error| Q2.5| Q97.5|
#   |:---------------|--------:|---------:|----:|-----:|
#   |b_Intercept     |     0.93|      0.00| 0.92|  0.93|
#   |b_phi_Intercept |     9.25|      0.60| 8.13| 10.50|
#   |b_zoi_Intercept |     0.40|      0.02| 0.37|  0.43|
#   |b_coi_Intercept |     0.99|      0.01| 0.97|  0.99|

# The .93 and 9.25 values are the mean and precision of the beta distribution that characterizes 
# the contradictory controls that are not zeroes and ones -- this is a distribution heavily skewed towards 1
# The .40 value is the probability that an observation will be either 0 or 1, and of these 40% endpoint values, 
# 99% (last value) are ones. So: as expected, the contradictory controls are heavily 1-skewed, see also this histogram:
ggplot(d[d$verb=="control_bad",], aes(x=response)) +
  geom_histogram()

# in principle, we can ask for each verb whether it differs from the contradictory controls, as follows:
# JT: I changed the verb here from "acknowledge" to "be_right", as the verb that is most like the contradictory
# controls 
#h <- c("acknowledge - control_bad" = "plogis(Intercept + verbacknowledge) = plogis(Intercept)")
h <- c("be_right_that - control_bad" = "plogis(Intercept + verbbe_right_that) = plogis(Intercept)")
hypothesis(m, h) 
# for be_right
# JT: I think this says that the expected value under the hypothesis lies outside the 95% credible interval

# for acknowledge:
# The expected value under the hypothesis lies outside the 95% credible interval. 
# **Can we use this for pairwise comparisons?**

# plot estimated mu parameter
plot(
  marginal_effects(m, dpar = "mu"), 
  points = TRUE, 
  point_args = list(width = .05, shape = 1)
)


# > summary(m) # see summary printed below
# Family: zero_one_inflated_beta 
# Links: mu = logit; phi = log; zoi = logit; coi = logit 
# Formula: response ~ verb 
# phi ~ verb
# zoi ~ verb
# coi ~ verb
# Data: d (Number of observations: 6312) 
# Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup samples = 4000
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
# Intercept                 2.56      0.05     2.47     2.65       1336 1.00
# phi_Intercept             2.22      0.06     2.10     2.35       1228 1.00
# zoi_Intercept            -0.39      0.06    -0.52    -0.27       2970 1.00
# coi_Intercept             4.32      0.41     3.58     5.22       1220 1.01
# verbacknowledge          -2.11      0.10    -2.30    -1.92       2943 1.00
# verbadmit                -1.94      0.10    -2.12    -1.75       3113 1.00
# verbannounce             -2.68      0.10    -2.88    -2.49       3496 1.00
# verbannoyed              -2.05      0.10    -2.24    -1.86       3148 1.00
# verbbe_right_that        -0.23      0.10    -0.43    -0.03       3504 1.00
# verbconfess              -2.02      0.10    -2.21    -1.84       3187 1.00
# verbconfirm              -1.63      0.10    -1.82    -1.43       2847 1.00
# verbdemonstrate          -1.90      0.10    -2.08    -1.70       2938 1.00
# verbdiscover             -1.59      0.10    -1.80    -1.40       3147 1.00
# verbestablish            -1.91      0.10    -2.10    -1.73       2754 1.00
# verbhear                 -3.31      0.10    -3.51    -3.11       3000 1.00
# verbinform_Sam           -2.63      0.10    -2.81    -2.43       3011 1.00
# verbknow                 -1.42      0.10    -1.62    -1.22       3166 1.00
# verbpretend              -3.33      0.10    -3.53    -3.12       3459 1.00
# verbprove                -1.24      0.11    -1.43    -1.03       3232 1.00
# verbreveal               -2.23      0.10    -2.42    -2.03       3264 1.00
# verbsay                  -2.86      0.10    -3.07    -2.66       3572 1.00
# verbsee                  -1.43      0.10    -1.63    -1.24       2855 1.00
# verbsuggest              -3.24      0.10    -3.43    -3.04       3052 1.00
# verbthink                -3.62      0.11    -3.83    -3.41       3504 1.00
# phi_verbacknowledge      -1.79      0.10    -1.98    -1.58       2268 1.00
# phi_verbadmit            -1.74      0.11    -1.95    -1.53       2399 1.00
# phi_verbannounce         -2.07      0.10    -2.27    -1.88       2282 1.00
# phi_verbannoyed          -1.77      0.10    -1.98    -1.57       2157 1.00
# phi_verbbe_right_that    -0.14      0.14    -0.42     0.12       2995 1.00
# phi_verbconfess          -1.76      0.10    -1.96    -1.56       2130 1.00
# phi_verbconfirm          -1.59      0.11    -1.81    -1.38       2269 1.00
# phi_verbdemonstrate      -1.74      0.11    -1.95    -1.54       2429 1.00
# phi_verbdiscover         -1.57      0.11    -1.78    -1.34       2482 1.00
# phi_verbestablish        -1.77      0.10    -1.97    -1.56       2283 1.00
# phi_verbhear             -1.79      0.11    -2.00    -1.58       2326 1.00
# phi_verbinform_Sam       -2.01      0.10    -2.20    -1.81       2231 1.00
# phi_verbknow             -1.59      0.11    -1.82    -1.37       2520 1.00
# phi_verbpretend          -1.86      0.11    -2.07    -1.65       2481 1.00
# phi_verbprove            -1.32      0.12    -1.56    -1.09       2649 1.00
# phi_verbreveal           -1.91      0.10    -2.11    -1.71       2070 1.00
# phi_verbsay              -2.05      0.10    -2.24    -1.85       2342 1.00
# phi_verbsee              -1.40      0.11    -1.62    -1.18       2284 1.00
# phi_verbsuggest          -1.78      0.11    -1.99    -1.57       2181 1.00
# phi_verbthink            -1.60      0.12    -1.83    -1.38       2382 1.00
# zoi_verbacknowledge      -1.20      0.17    -1.54    -0.86       4740 1.00
# zoi_verbadmit            -1.11      0.17    -1.45    -0.79       5360 1.00
# zoi_verbannounce         -0.77      0.16    -1.08    -0.45       5087 1.00
# zoi_verbannoyed          -1.14      0.17    -1.47    -0.81       5361 1.00
# zoi_verbbe_right_that    -0.24      0.14    -0.53     0.03       5142 1.00
# zoi_verbconfess          -1.19      0.18    -1.55    -0.85       5020 1.00
# zoi_verbconfirm          -1.06      0.17    -1.39    -0.74       5005 1.00
# zoi_verbdemonstrate      -1.04      0.17    -1.38    -0.72       4978 1.00
# zoi_verbdiscover         -0.85      0.16    -1.18    -0.55       5226 1.00
# zoi_verbestablish        -1.22      0.18    -1.58    -0.87       5152 1.00
# zoi_verbhear             -0.65      0.15    -0.96    -0.35       5323 1.00
# zoi_verbinform_Sam       -0.96      0.17    -1.30    -0.64       5281 1.00
# zoi_verbknow             -0.81      0.16    -1.12    -0.51       5192 1.00
# zoi_verbpretend          -0.57      0.15    -0.86    -0.27       4649 1.00
# zoi_verbprove            -0.72      0.16    -1.03    -0.42       4956 1.00
# zoi_verbreveal           -1.09      0.18    -1.45    -0.76       4562 1.00
# zoi_verbsay              -0.79      0.16    -1.10    -0.47       5259 1.00
# zoi_verbsee              -0.86      0.16    -1.18    -0.54       4817 1.00
# zoi_verbsuggest          -0.96      0.16    -1.28    -0.65       4838 1.00
# zoi_verbthink            -0.55      0.15    -0.86    -0.26       4808 1.00
# coi_verbacknowledge      -3.04      0.55    -4.16    -1.98       1830 1.00
# coi_verbadmit            -2.81      0.56    -3.95    -1.68       1714 1.00
# coi_verbannounce         -4.75      0.48    -5.76    -3.90       1520 1.01
# coi_verbannoyed          -3.64      0.52    -4.71    -2.66       1696 1.00
# coi_verbbe_right_that    -0.29      0.89    -1.85     1.66       2636 1.00
# coi_verbconfess          -3.51      0.53    -4.58    -2.51       1456 1.00
# coi_verbconfirm          -2.89      0.54    -3.98    -1.88       1765 1.00
# coi_verbdemonstrate      -2.72      0.57    -3.88    -1.68       1674 1.00
# coi_verbdiscover         -2.42      0.57    -3.55    -1.31       1834 1.00
# coi_verbestablish        -2.76      0.57    -3.88    -1.67       1833 1.00
# coi_verbhear             -6.41      0.55    -7.53    -5.40       1774 1.00
# coi_verbinform_Sam       -4.70      0.50    -5.73    -3.78       1518 1.00
# coi_verbknow             -1.55      0.68    -2.85    -0.15       2287 1.00
# coi_verbpretend          -7.03      0.62    -8.34    -5.91       1985 1.00
# coi_verbprove            -1.13      0.77    -2.57     0.47       2349 1.00
# coi_verbreveal           -3.28      0.53    -4.35    -2.26       1657 1.00
# coi_verbsay              -5.68      0.52    -6.77    -4.75       1591 1.00
# coi_verbsee              -1.57      0.69    -2.90    -0.12       2217 1.00
# coi_verbsuggest          -7.34      0.79    -9.04    -5.95       2493 1.00
# coi_verbthink            -7.31      0.68    -8.77    -6.09       2287 1.00