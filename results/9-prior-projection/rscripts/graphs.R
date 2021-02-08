# Prior paper Exp 1 (prior and projection, within-participant design)
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
table(t$prior_fact) 
length(unique(t$prior_fact)) #40 facts
table(t$eventItem) 
length(unique(t$eventItem)) #20 clauses
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
  mutate(eventItem = fct_relevel(eventItem,levels(high$eventItem))) %>% 
  mutate(prior_type = fct_relevel(prior_type,"low_prior"))
means

subjmeans = t %>%
  group_by(eventItem,workerid,prior_type) %>%
  summarize(Mean = mean(prior)) %>% 
  ungroup() %>% 
  mutate(prior_type = fct_relevel(as.factor(as.character(prior_type)),"low_prior"))
subjmeans$eventItem <- factor(subjmeans$eventItem, levels = unique(levels(means$eventItem)))
levels(subjmeans$eventItem)
names(subjmeans)

ggplot(means, aes(x=eventItem, y=Mean, color=prior_type,shape=prior_type,fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Fact",labels=c("lower probability", "higher probability"), values=c("#56B4E9","#E69F00")) +
  coord_flip() +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean prior probability rating") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)


# Fig 3: plot projection by prior type collapsing over predicate (with main clause content) ----
nrow(d)
table(d$prior_type)

# mean projectivity by predicate and prior type, with main clause controls
proj.means = d %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
proj.means

# order predicates by high_prior
high = proj.means %>%
  filter(prior_type != "low_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))
high
levels(high$short_trigger)

proj.means = proj.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(high$short_trigger)))
levels(proj.means$short_trigger)

# change factor levels for prior_type for plotting
proj.means = proj.means %>%
  mutate(prior_type = fct_relevel(prior_type, "main_clause", "low_prior", "high_prior"))
levels(proj.means$prior_type)

# to plot MC in different color and shape, copy MC data to new data frame and 
# remove MC data, but not factor level, from proj.means
mc.data = droplevels(subset(proj.means, proj.means$verb == "MC"))
mc.data
#View(mc.data)

proj.means[proj.means$short_trigger == "MC",]$Mean <- NA
proj.means[proj.means$short_trigger == "MC",]$YMin <- NA
proj.means[proj.means$short_trigger == "MC",]$YMax <- NA

# to add participants' ratings
subjmeans = d %>%
  group_by(workerid,short_trigger,prior_type) %>%
  summarize(Mean = mean(projective))
subjmeans$short_trigger <- factor(subjmeans$short_trigger, levels = unique(levels(proj.means$short_trigger)))
levels(subjmeans$short_trigger)
subjmeans
subjmeans$prior_type <- as.factor(subjmeans$prior_type)

# change factor levels for prior_type for plotting
subjmeans = subjmeans %>%
  mutate(prior_type = fct_relevel(prior_type, "main_clause", "low_prior", "high_prior"))
levels(subjmeans$prior_type)
  
levels(proj.means$prior_type)
# [1] "main_clause"
# [2] "low_prior"  
# [3] "high_prior" 

ggplot(proj.means, aes(x=short_trigger, y=Mean, color=prior_type,fill=prior_type,shape=prior_type)) + 
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(24, 25, 21)),labels=rev(c("higher probability","lower probability","main clause")),name="Fact") +
  scale_fill_manual(values=rev(c("#E69F00","#56B4E9","black")),labels=rev(c("higher probability","lower probability","main clause")),name="Fact") +
  scale_color_manual(values=rev(c("#E69F00","#56B4E9","black")),labels=rev(c("higher probability","lower probability","main clause")),name="Fact") +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  geom_errorbar(aes(x=1,ymin=mc.data$YMin,ymax=mc.data$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(shape=20,size=4,aes(x=1,y=mc.data$Mean),color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=5,width=7)

#### Fig 4: plot projectivity by prior probability on a by-participant level (no MC content) ----
nrow(d) #7436 (all data, i.e., with main clause content)
nrow(t)  #5720 (target, i.e., without main clause content)

names(t)
table(t$short_trigger)

proj.means = t %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),Mean))  
proj.means
#View(proj.means)

nrow(proj.means) #40 (high_prior and low_prior for each of the 20 predicates)

high = proj.means %>%
  filter(prior_type == "high_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))

t = t %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(high$short_trigger)))
table(t$short_trigger)

# change factor levels for prior_type for plotting
t = t %>%
  mutate(prior_type = fct_relevel(prior_type, "low_prior", "high_prior"))
levels(t$prior_type)

ggplot(t, aes(x=prior, y=projective,color=prior_type)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~short_trigger)
ggsave(f="../graphs/projection-by-prior.pdf",height=7,width=7)


