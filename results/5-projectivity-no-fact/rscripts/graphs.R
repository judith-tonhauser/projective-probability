# Factives paper
# 5-projectivity-no-fact (certainty ratings, continuous task)
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(viridis)
library(gplots) # just to get named to hex color converter
theme_set(theme_bw())

# load clean data 
cd = read.csv("../data/cd.csv")
nrow(cd) #6916

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

# plotting slider ratings suggests we should not use a linear regression model because of the slider endpoint bunching
ggplot(cd, aes(x=response)) +
  geom_histogram(bins=50) +
  ylim(c(0,2000)) +
  xlab("Rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=0.1))
ggsave("../graphs/bunching.pdf",width=3.4,height=2)

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
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid",
                      ifelse(cols$VeridicalityGroup == "NF", "gray60",
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


# col2hex(c("darkorchid","gray60","dodgerblue","tomato1"))
# check colors for how they look under different colorblindness types: 
# https://davidmathlogic.com/colorblind/#%239932CC-%23999999-%231E90FF-%23FF6347


# in case you want a different set of cb-friendly colors:
# basecolors = viridis(n=5,option="D")#,start=.1,end=1)
# colors = c(basecolors[5],basecolors[2],basecolors[3],basecolors[4],basecolors[1])

# "magma" (or "A")
# "inferno" (or "B")
# "plasma" (or "C")
# "viridis" (or "D")
# "cividis" (or "E")
# "rocket" (or "F")
# "mako" (or "G")
# "turbo" (or "H")

# cols$Colors =  ifelse(cols$VeridicalityGroup == "F", colors[5], 
#                       ifelse(cols$VeridicalityGroup == "NF", colors[2], 
#                              ifelse(cols$VeridicalityGroup == "VNF", colors[3],
#                                     ifelse(cols$VeridicalityGroup == "MC",colors[1],colors[4]))))
# cols$Colors

cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = factor(x=
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be right","demonstrate"),"VNF",
                       ifelse(means$verb  %in% c("MC"),"MC","V")))),levels=rev(c("F","V","VNF","NF","MC")))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)


# Figure 2 Language paper color -----
# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean)) +
  # geom_point(shape=21,fill="gray70",data=subjmeans, alpha=.1, color="gray40") +
  geom_violin(data=subjmeans,scale="width",color="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax, fill=VeridicalityGroup, shape=VeridicalityGroup),width=0.1,color="black") +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","main clause\ncontrols")),name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","main clause\ncontrols")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-projectivity-by-predicate-variability.pdf",height=4.5,width=7)
ggsave("../../../papers/factives-paper/Language-figures/color/Figure2.pdf",height=4.5,width=7)

# Figure 2 Language paper black and white -----
ggplot(means, aes(x=verb, y=Mean)) +
  geom_violin(data=subjmeans,scale="width",color="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax, fill=VeridicalityGroup, shape=VeridicalityGroup),width=0.1,color="black") +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","main clause\ncontrols")),name="Predicate type") +
  scale_fill_manual(values=rev(gray.colors(5,start=0,end=1)),labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","main clause\ncontrols")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-projectivity-by-predicate-variability-bw.pdf",height=4.5,width=7)
ggsave("../../../papers/factives-paper/Language-figures/bw/Figure2.pdf",height=4.5,width=7)

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
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be right","demonstrate"),"NF",
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
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be right","demonstrate"),"NF",
                       ifelse(means$verb  %in% c("MC"),"MC","V")))))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)


# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/3-way-means-projectivity-by-predicate-variability.pdf",height=4,width=7)


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
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be right","demonstrate"),"VNF",
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
merged$infEnt <- ifelse(merged$verb == "be right" | merged$verb == "see" | merged$verb == "discover" | 
                          merged$verb == "prove" | merged$verb == "confirm","yes","no")
merged$contrEnt <- "no"
merged$contrEnt <- ifelse(merged$verb == "be right","yes","no")

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
  ifelse(cd$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "factive", 
         ifelse(cd$verb %in% c("pretend", "think", "suggest", "say"), "plain nonfactive", 
                ifelse(cd$verb %in% c("be right","demonstrate"),"veridical nonfactive",
                       ifelse(cd$verb %in% c("MC"),"MC","projective nonfactive")))))

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

