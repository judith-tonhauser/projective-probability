# Prior paper Exp 1 (prior and projection, within-participant design)
# experiment investigating prior and projection
# contents of complements of 20 predicates
# graphs-for-talks.R

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

# load clean spread data
d = read_csv("../data/cd.csv")
nrow(d) #7436 / 26 = 286 turkers (data already in spread format)

summary(d)
table(d$prior_type) # half is high_prior, half is low_prior

table(d$block_proj) # projection block more frequently occurred second

t = d %>% 
  filter(short_trigger != "MC") %>% 
  droplevels()
nrow(t) #5720 / 286 = 20 rows per participant

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

summary(t)
table(t$prior_fact) 
length(unique(t$prior_fact)) #40 facts
table(t$eventItem) 
length(unique(t$eventItem)) #20 clauses
str(t$prior)

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
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=as.character(cols$Colors))) +
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

