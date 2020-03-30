# Factives paper
# 2-veridicality2 (contradictoriness ratings, continuous task)
# graphs.R

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


# load clean data ----
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
nrow(cd) #7364 / 28 items = 263 participants

# plotting slider ratings suggests we should not use a linear regression model because of the slier endpoint bunching
ggplot(cd, aes(x=response)) +
  geom_histogram(bins=50) +
  ylim(c(0,2000)) +
  xlab("Rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/bunching.pdf",width=3.4,height=2)


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

# recode control labels to be more readable
table(cd$verb)

cd = cd %>%
  mutate(verb = fct_recode(verb,"non-contrad."="non-contrd. C","contradictory"="contradictory C"))

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
                       ifelse(cols$V %in% c("non-contrad.","contradictory"),"MC","V")))))

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

means$VeridicalityGroup = factor(x=
                                   ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
                                          ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                                                 ifelse(means$verb  %in% c("be_right","demonstrate"),"VNF",
                                                        ifelse(means$verb  %in% c("contradictory","non-contrad."),"control","V")))),levels=rev(c("F","V","VNF","NF","control")))

ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup,shape=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  #scale_fill_manual(values=c("black","darkorchid","gray60","tomato1","dodgerblue")) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(legend.position="bottom") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Mean contradictoriness rating") +
  xlab("Predicate")
ggsave("../graphs/means-contradictoriness-by-predicate-variability.pdf",height=4.5,width=7)

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
