# Prior paper Exp 2b
# Projection experiment with sensitivity to fact
# graphs.R

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

# load clean data
cd = read.csv("../data/cd.csv")
nrow(cd) #6916 / 266 = 26 ratings (20 target, 6 main clause controls)
summary(cd)

# change predicate names
cd = cd %>%
  mutate(verb=dplyr::recode(verb, annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(cd$verb)
table(cd$fact_type)
table(cd$verb,cd$fact_type)
str(cd$fact_type)
cd$fact_type <- as.character(cd$fact_type)

# fill in fact_type "main clause" for main clause content, so that not empty
cd[cd$verb == "control",]$fact_type <- "main clause"

# Fig 5 (projection by fact) ----

# mean projectivity by predicate and prior type, with main clause controls
proj.means = cd %>%
  group_by(verb,fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))  
proj.means

# order predicates by high_prior
high = proj.means %>%
  filter(fact_type != "factL") %>%
  mutate(verb = fct_reorder(verb,Mean))
high
levels(high$verb)

proj.means = proj.means %>%
  mutate(verb = fct_relevel(verb,levels(high$verb)))
levels(proj.means$verb)

# to plot MC in different color and shape, copy MC data to new data frame and 
# remove MC data, but not factor level, from proj.means
mc.data = droplevels(subset(proj.means, proj.means$verb == "control"))
mc.data

proj.means[proj.means$verb == "control",]$Mean <- NA
proj.means[proj.means$verb == "control",]$YMin <- NA
proj.means[proj.means$verb == "control",]$YMax <- NA

# to add participants' ratings
subjmeans = cd %>%
  group_by(workerid,verb,fact_type) %>%
  summarize(Mean = mean(response))
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(proj.means$verb)))
levels(subjmeans$verb)

subjmeans$fact_type <- as.factor(subjmeans$fact_type)
levels(subjmeans$fact_type)

levels(proj.means$fact_type)
proj.means$fact_type <- as.factor(proj.means$fact_type)

# change factor levels for fact_type for plotting
proj.means = proj.means %>%
  mutate(fact_type = fct_relevel(fact_type, "main clause", "factL", "factH"))
levels(proj.means$fact_type)

subjmeans = subjmeans %>%
  mutate(fact_type = fct_relevel(fact_type, "main clause", "factL", "factH"))
levels(subjmeans$fact_type)

ggplot(proj.means, aes(x=verb, y=Mean, color=fact_type,fill=fact_type,shape=fact_type)) + 
  geom_point(data=subjmeans,aes(fill=fact_type,color=fact_type),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=c(21, 25, 24),labels=c("main clause","lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("black","#56B4E9","#E69F00"),labels=c("main clause","lower probability","higher probability"),name="Fact") +
  scale_color_manual(values=c("black","#56B4E9","#E69F00"),labels=c("main clause","lower probability","higher probability"),name="Fact") +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0),labels= c("0",".2",".4",".6",".8","1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_errorbar(aes(x=1,ymin=mc.data$YMin,ymax=mc.data$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(shape=20,size=4,aes(x=1,y=mc.data$Mean),color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=5,width=7)


