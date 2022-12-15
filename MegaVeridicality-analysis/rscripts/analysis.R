# Analysis of veridicality and projection ratings from 
# White & Rawlins' MegaVeridicality I dataset 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

## preprocessing ----

# load raw data
# MV1: judgments of "did that thing happen?" for positive and negated predicates with "that" complements
mv1 = read.csv("../data/mega-veridicality-v1/mega-veridicality-v1.csv")

# MV2: judgments of "did that thing happen?" for pos/neg predicates with nonfinite complements
# not relevant for our paper or comparison
#mv2 = read.csv("../data/mega-veridicality-v2/mega-veridicality-v2.csv")

nrow(mv1) #21760
#nrow(mv2) #50260

### exclude nonAmerican English speakers
length(unique(mv1$participant)) #291
#length(unique(mv2$participant)) #635

table(mv1$nativeenglish)
#table(mv2$nativeenglish)

str(mv1$nativeenglish)

mv1 <- droplevels(subset(mv1, (mv1$nativeenglish != "False")))
#mv2 <- droplevels(subset(mv2, (mv2$nativeenglish != "False")))

length(unique(mv1$participant)) #290
#length(unique(mv2$participant)) #630

# exclude 3 rows in mv2 where their java script created NA in veridicality response
table(mv1$veridicality)
#table(mv2$veridicality)

#mv2 <- droplevels(subset(mv2,(mv2$veridicality != "")))

# continuing to work only with mv1

# recode their responses to numerical values
# MV has ratings on a 3-point Likert scale in response to "Did that thing happen?"
# "veridicality" codes the response options: "yes" (happened) "no" (didn't happen) "maybe" (maybe happened)

# create veridicality_num which codes "yes" as 1, "no" as -1 and "maybe" as 0
table(mv1$veridicality)
str(mv1$veridicality)

mv1$veridicality_num[mv1$veridicality == "maybe"] <- 0 
mv1$veridicality_num[mv1$veridicality == "no"] <- -1
mv1$veridicality_num[mv1$veridicality == "yes"] <- 1
str(mv1$veridicality_num)

# check that all is in order
table(mv1$veridicality)
table(mv1$veridicality_num)

# we had 20 predicates, their MV1 does not include "be right"
table(mv1$verb) 

# our 19 predicates 
our_preds <- c("be annoyed", "discover", "know", "reveal", "see", "pretend", "suggest", "say", "think", 
               "demonstrate", "acknowledge", "admit", "announce", "confess", "confirm", "establish", "hear", "inform", "prove")
our_preds
length(our_preds) #19

# rename their predicate "annoy" for comparison with our "be annoyed"
mv1 <- mv1 %>% 
  mutate(verb=recode(verb, annoy = "be annoyed"))

# create items for mv1 

table(mv1$verb) #verb
table(mv1$frame) #only that_S
table(mv1$voice) #active, passive
table(mv1$polarity) #negative, positive
table(mv1$conditional) #true, false

str(mv1$conditional)
str(mv1$conditional2)
mv1$conditional2 <- as.character(mv1$conditional)
mv1$conditional2[mv1$conditional2 == "True"] <- "conditional"
mv1$conditional2[mv1$conditional2 == "False"] <- "matrix"

table(mv1$conditional2) #conditional, matrix

mv1$item <- paste(mv1$verb,mv1$frame,mv1$voice,mv1$polarity,mv1$conditional2, sep = "-")
table(mv1$item)

# save data
write.csv(mv1, "../data/mv1.csv")
nrow(mv1) #21692

# Fig 17: veridicality ratings ----

# load data 
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692

# create relevant subset: ratings for positive nonconditional matrix sentences
mv1_tmp <- droplevels(subset(mv1, mv1$polarity == "positive" & mv1$conditional2 == "matrix"))
t <- table(mv1_tmp$verb)
min(t) #9
max(t) #20
mean(t) #10

length(unique(mv1_tmp$verb)) #517 verbs
length(unique(mv1_tmp$participant)) #159 participants gave ratings

# calculate mean for all 517 predicates
p_means = mv1_tmp %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
#View(p_means)

str(p_means$verb)
levels(p_means$verb) # sorted by veridicality mean (videotape, verify, upset are last)

# define colors 
cols = data.frame(V=levels(p_means$verb))
cols

str(cols$V)
cols$V <- factor(cols$V, levels = unique(levels(p_means$verb)))
levels(cols$V)
levels(p_means$verb) 

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", 
                       ifelse(cols$V %in% c("hear","acknowledge","confess","prove","confirm","establish","inform","announce","admit"),"V",
                              "X")))))
  
cols$VeridicalityGroup <- factor(cols$VeridicalityGroup, levels=c("X","NF","VNF","V","F"))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF", "dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "V", "tomato1", "black"))))


p_means$VeridicalityGroup = as.factor(
  ifelse(p_means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(p_means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(p_means$verb  %in% c("be right","demonstrate"),"VNF",
                       ifelse(p_means$verb  %in% c("hear","acknowledge","confess","prove","confirm","establish","inform","announce","admit"),"V",
                              "X")))))

p_means$VeridicalityGroup <- factor(p_means$VeridicalityGroup, levels=c("X","NF","VNF","V","F"))


#View(p_means)

# create data subsets for our 19 predicates
p_meansOUR <- droplevels(subset(p_means,p_means$verb %in% our_preds))
p_meansOUR
str(p_meansOUR$verb)
levels(p_meansOUR$verb) # sorted by veridicality mean (pretend...reveal)


# check to get shapes and colors to work out right
levels(p_means$VeridicalityGroup)
# "X"   "NF"  "VNF" "V"   "F"

size <- ifelse(p_means$VeridicalityGroup == "X", 1, 4)

# Figure 17 in color 
ggplot(p_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray",alpha=.4) +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=size,color="black") +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),
                     labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                     name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),
                    labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                    name="Predicate type") +
  theme(panel.grid.major.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none") +
  geom_text_repel(data=p_meansOUR,aes(x=verb,y=Mean,label=verb,
                                      color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.8) +
  scale_color_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60"))) +
  scale_y_continuous(limits = c(-1.1,1.1),breaks = c(-1,0,1)) +
  ylab("Mean veridicality rating") +
  xlab("Predicate") 
ggsave("../graphs/means-entailment-by-predicate.pdf",height=4,width=9)
ggsave("../../papers/factives-paper/Language-figures/color/Figure17.pdf",height=4,width=9)

# Figure 17 in bw 
ggplot(p_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray",alpha=.4) +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=size,color="black") +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),
                     labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                     name="Predicate type") +
  scale_fill_manual(values=rev(gray.colors(5,start=0,end=1)),
                    labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                    name="Predicate type") +
  theme(panel.grid.major.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none") +
  geom_text_repel(data=p_meansOUR,aes(x=verb,y=Mean,label=verb),segment.color="black",nudge_x=.2,nudge_y=-.8) +
  #scale_color_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60"))) +
  scale_y_continuous(limits = c(-1.1,1.1),breaks = c(-1,0,1)) +
  ylab("Mean veridicality rating") +
  xlab("Predicate") 
ggsave("../../papers/factives-paper/Language-figures/bw/Figure17.pdf",height=4,width=9)



# Fig 7: projection ratings ----

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692

# create data relevant to investigate projection (embedding is negation, conditional or both)
mv1_tmp <- droplevels(subset(mv1, mv1$polarity == "negative" | mv1$conditional2 == "conditional"))
t <- table(mv1_tmp$verb)
min(t)
mean(t)
max(t) #29-60 ratings per predicate under negation, cond or both
length(unique(mv1_tmp$verb)) #517 verbs
length(unique(mv1_tmp$participant)) #290 participants gave ratings

# calculate mean for all 517 predicates
p_means = mv1_tmp %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
levels(p_means$verb) # verbs sorted by projectivity mean (pretend...be_annoyed...resent)

# define colors 
cols = data.frame(V=levels(p_means$verb))
cols

str(cols$V)
cols$V <- factor(cols$V, levels = unique(levels(p_means$verb)))
levels(cols$V)
levels(p_means$verb) 

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", 
                       ifelse(cols$V %in% c("hear","acknowledge","confess","prove","confirm","establish","inform","announce","admit"),"V",
                              "X")))))

cols$VeridicalityGroup <- factor(cols$VeridicalityGroup, levels=c("X","NF","VNF","V","F"))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF", "dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "V", "tomato1", "black"))))


p_means$VeridicalityGroup = as.factor(
  ifelse(p_means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(p_means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(p_means$verb  %in% c("be right","demonstrate"),"VNF",
                       ifelse(p_means$verb  %in% c("hear","acknowledge","confess","prove","confirm","establish","inform","announce","admit"),"V",
                              "X")))))

p_means$VeridicalityGroup <- factor(p_means$VeridicalityGroup, levels=c("X","NF","VNF","V","F"))


#View(p_means)

# create data subsets for our 19 predicates
p_meansOUR <- droplevels(subset(p_means,p_means$verb %in% our_preds))
p_meansOUR
str(p_meansOUR$verb)
levels(p_meansOUR$verb) # sorted by projectivity mean (pretend...be annoyed)


# check to get shapes and colors to work out right
levels(p_means$VeridicalityGroup)
# "X"   "NF"  "VNF" "V"   "F"

size <- ifelse(p_means$VeridicalityGroup == "X", 1, 4)

# Figure 7 in color 
ggplot(p_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray",alpha=.4) +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=size,color="black") +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),
                     labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                     name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),
                    labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                    name="Predicate type") +
  theme(text = element_text(size=12),panel.grid.major.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none") +
  geom_text_repel(data=p_meansOUR,aes(x=verb,y=Mean,label=verb,
                                      color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.8) +
  scale_color_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60"))) +
  scale_y_continuous(limits = c(-1.1,1.1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate") 
ggsave("../graphs/means-projection-by-predicate.pdf",height=4,width=9)
ggsave("../../papers/factives-paper/Language-figures/color/Figure7.pdf",height=4,width=9)

# Figure 7 in bw 
ggplot(p_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray",alpha=.4) +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=size,color="black") +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),
                     labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                     name="Predicate type") +
  scale_fill_manual(values=rev(gray.colors(5,start=0,end=1)),
                    labels=rev(c("factive","optionally\nfactive","veridical\nnonfactive","nonveridical\nnonfactive","predicate not in\nour experiments")),
                    name="Predicate type") +
  theme(text = element_text(size=12),panel.grid.major.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none") +
  geom_text_repel(data=p_meansOUR,aes(x=verb,y=Mean,label=verb),segment.color="black",nudge_x=.2,nudge_y=-.8) +
  #scale_color_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60"))) +
  scale_y_continuous(limits = c(-1.1,1.1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate") 
ggsave("../../papers/factives-paper/Language-figures/bw/Figure7.pdf",height=4,width=9)


# which predicates are veridical or factive according to definition (10b)? ----

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692

# entailment ratings
# predicates with only "yes" ratings have entailed CCs

mv1_ent <- droplevels(subset(mv1, mv1$polarity == "positive" & mv1$conditional2 == "matrix"))
head(mv1_ent)

t <- table(mv1_ent$verb,mv1_ent$veridicality)
t <- as.data.frame.matrix(t) 
t <- t %>% 
  rownames_to_column(var = "verb")
t$total <- t$maybe + t$no + t$yes
head(t)

ent <- droplevels(subset(t,t$yes == t$total))
head(ent)
names(ent)
nrow(ent) #97 predicates only have "yes" ratings

# projection ratings for these 97 predicates

mv1_proj <- droplevels(subset(mv1, mv1$polarity == "negative" | mv1$conditional2 == "conditional"))

proj <- droplevels(subset(mv1_proj, mv1_proj$verb %in% ent$verb))
length(unique(proj$verb)) #97

# look at projectivity ratings for these 97 veridical predicates

p_means = proj %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
levels(p_means$verb) # verbs sorted by projectivity mean (pretend...be_annoyed...resent)

str(p_means$verb)
p_means$verb <- as.character(p_means$verb)

mean(p_means$Mean) #0.6538571
min(p_means$Mean) #0.1333333
max(p_means$Mean) #0.9666667
sd(p_means$Mean) #0.1995803

# projection rating of at least .3
p_means[p_means$Mean >= .3,]$verb #92


View(p_means)

ggplot(p_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray") +
  geom_point(shape=16,stroke=.5,size=2.5,color="palegreen4") +
  #scale_fill_manual(values=c("gray60","dodgerblue","tomato1","darkorchid","palegreen4")) + 
  #geom_text_repel(data=p_meansOUR,aes(x=verb,y=Mean,label=verb,color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.5) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  #scale_color_manual(values=c(NF="gray60",VNF="dodgerblue",V="tomato1",F="darkorchid"),
                     #labels = c("nonveridical\nnonfactive","veridical\nnonfactive","optionally\nfactive","factive")) +
  scale_y_continuous(limits = c(-.7,1.1),breaks = c(-.5,0,1)) +
  #scale_alpha(range = c(.3,1)) +
  labs(color="Predicate type") +
  theme(legend.position="bottom") + 
  ylab("Mean projectivity rating") +
  xlab("Predicate") 
#ggsave("../graphs/means-projection-by-predicate.pdf",height=4,width=9)



# which predicates are veridical or factive according to their linking function? ----
# their linking function: majority rule

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692

# entailment ratings
mv1_ent <- droplevels(subset(mv1, mv1$polarity == "positive" & mv1$conditional2 == "matrix"))

t <- table(mv1_ent$verb,mv1_ent$veridicality)
t <- as.data.frame.matrix(t) 
t <- t %>% 
  rownames_to_column(var = "verb")
t$total <- t$maybe + t$no + t$yes
head(t)

# Taking the majority response boundaries as a guide, the vast majority of verb-frame pairs 
# are nonveridical (115 verbs), nonfactive veridical (177 verbs), or factive (199 verbs), 
# with far fewer being antiveridical in either positive or negative frames.

# majority response: majority "yes" means "veridical"?
t$veridical <- ifelse(t$yes > t$maybe & t$yes > t$no,"yes","no")
head(t)
table(t$veridical)
#no yes 
#157 360

# or stricter: majority response means more "yes" than "maybe" and "no" combined?
t$not.yes <- t$maybe + t$no
t$veridical_s <- ifelse(t$yes > t$not.yes,"yes","no")
table(t$veridical_s)
#no yes 
#168 349

# projection ratings ratings
mv1_proj <- droplevels(subset(mv1, mv1$polarity == "negative" | mv1$conditional2 == "conditional"))

t2 <- table(mv1_proj$verb,mv1_proj$veridicality)
t2 <- as.data.frame.matrix(t2) 
t2 <- t2 %>% 
  rownames_to_column(var = "verb")
t2$total <- t2$maybe + t2$no + t2$yes
head(t2)

# majority response: majority "yes" means "veridical"?
t2$projective <- ifelse(t2$yes > t2$maybe & t2$yes > t2$no,"yes","no")
head(t2)
table(t2$projective)
#no yes 
#299 218

# or stricter: majority response means more "yes" than "maybe" and "no" combined?
t2$not.yes <- t2$maybe + t2$no
t2$projective_s <- ifelse(t2$yes > t2$not.yes,"yes","no")
table(t2$projective_s)
#no yes 
#317 200

# combine the two dataframes
head(t)
head(t2)

t = t %>%
  select(verb,veridical,veridical_s)
t2 = t2 %>%
  select(verb,projective,projective_s)

d = t %>% 
  left_join(t2)

head(d)

# Taking the majority response boundaries as a guide, the vast majority of verb-frame pairs 
# are nonveridical (115 verbs), nonfactive veridical (177 verbs), or factive (199 verbs), 
# with far fewer being antiveridical in either positive or negative frames.


# which predicates are veridical nonfactive and which are factive?
# can't figure out how they got their numbers...

d$vnf <- ifelse(d$veridical == "yes" & d$projective == "no","yes","no")
table(d$vnf)
#no yes 
#368 149

d$vnf_s <- ifelse(d$veridical_s == "yes" & d$projective_s == "no","yes","no")
table(d$vnf_s)
#no yes 
#360 157

d$factive <- ifelse(d$veridical == "yes" & d$projective == "yes","yes","no")
table(d$factive)
#no yes 
#306 211

d$factive_s <- ifelse(d$veridical_s == "yes" & d$projective_s == "yes","yes","no")
table(d$factive_s)
#no yes 
#325 192

## comparison between our ratings and MegaVeridicality ratings ----

# load our continuous data 

# projection
d_proj = read.csv("../../results/5-projectivity-no-fact/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(d_proj$verb)

# inference diagnostic
d_inf = read.csv("../../results/4-veridicality3/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(d_inf$verb)

# contradictoriness diagnostics
d_contr = read.csv("../../results/2-veridicality2/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(d_contr$verb)

# for comparison, remove main clauses, controls and "be_right" from our data 
d_proj <- droplevels(subset(d_proj, d_proj$verb != "MC" & d_proj$verb != "be_right"))
d_inf <- droplevels(subset(d_inf, d_inf$verb != "control_good" & d_inf$verb != "control_bad" & d_inf$verb != "be_right"))
d_contr <- droplevels(subset(d_contr, d_contr$verb != "control_good" & d_contr$verb != "control_bad" & d_contr$verb != "be_right"))

# compare projectivity ratings

our_p_means = d_proj %>%
  group_by(verb) %>%
  summarize(MeanOUR = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinOUR = MeanOUR - CILow, YMaxOUR = MeanOUR + CIHigh) %>%
  select(-CILow,-CIHigh)
table(our_p_means$verb)
str(our_p_means$verb)
table(p_means$verb)
str(p_means$verb)

head(our_p_means)
head(p_means)

# make sure that the two verb factors have the same levels
combined <- sort(union(levels(our_p_means$verb), levels(p_means$verb)))
combined

pd = mutate(our_p_means, verb=factor(verb, levels=combined)) %>%
  left_join(mutate(p_means, verb=factor(verb, levels=combined))) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("demonstrate") ~ "VNF",
    TRUE ~ "V")))
str(pd$verb)

# define colors for the predicates
cols = data.frame(V=levels(pd$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(p_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(p_means$verb)),]$V, ordered = TRUE)
levels(cols$V)


ggplot(pd, aes(x=MeanOUR, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMinOUR,xmax=YMaxOUR),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label=verb),color=cols$Colors,size=3) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean 'did that happen?' rating") +
  xlab("Mean certainty rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/projectivity-comparison.pdf",height=4,width=4)

cor.test(pd$MeanOUR,pd$Mean,method="spearman")
length(unique(pd$verb)) #19 verbs, rho = .852, p-value < .001

corr_projectivity = pd %>%
  summarize(Cor=cor(MeanOUR,Mean,method="spearman"))
corr_projectivity #.852

# compare veridicality ratings I: our inference diagnostic

our_vinf_means = d_inf %>%
  group_by(verb) %>%
  summarize(MeanOUR = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinOUR = MeanOUR - CILow, YMaxOUR = MeanOUR + CIHigh) %>%
  select(-CILow,-CIHigh)
our_vinf_means 

# make sure that the two verb factors have the same levels
combined <- sort(union(levels(our_vinf_means$verb), levels(v_means$verb)))
combined

vinfd = mutate(our_vinf_means, verb=factor(verb, levels=combined)) %>%
  left_join(mutate(v_means, verb=factor(verb, levels=combined))) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("demonstrate") ~ "VNF",
    TRUE ~ "V")))
str(vinfd$verb)

# define colors for the predicates
cols = data.frame(V=levels(vinfd$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)
levels(cols$V)


ggplot(vinfd, aes(x=MeanOUR, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMinOUR,xmax=YMaxOUR),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label=verb),color=cols$Colors,size=3) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean 'did that happen?' rating") +
  xlab("Mean inference rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-inference-comparison.pdf",height=4,width=4)

cor.test(vinfd$MeanOUR,vinfd$Mean,method="spearman")
length(unique(vinfd$verb)) #19 verbs, rho = .596, p-value = 0.07126

corr_inference = vinfd %>% 
  summarize(Cor=cor(MeanOUR,Mean,method="spearman"))
corr_inference #.596

# compare veridicality ratings II: our contradictoriness diagnostic

our_vcontr_means = d_contr %>%
  group_by(verb) %>%
  summarize(MeanOUR = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinOUR = MeanOUR - CILow, YMaxOUR = MeanOUR + CIHigh) %>%
  select(-CILow,-CIHigh)
our_vcontr_means 

vcontrd = mutate(our_vcontr_means, verb=factor(verb, levels=combined)) %>%
  left_join(mutate(v_means, verb=factor(verb, levels=combined))) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("demonstrate") ~ "VNF",
    TRUE ~ "V")))

str(vcontrd$verb)

# define colors for the predicates
cols = data.frame(V=levels(vcontrd$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)
levels(cols$V)


ggplot(vcontrd, aes(x=MeanOUR, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMinOUR,xmax=YMaxOUR),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label=verb),color=cols$Colors,size=3) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean 'did that happen?' rating") +
  xlab("Mean contradictoriness rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-contradictoriness-comparison.pdf",height=4,width=4)

cor.test(vcontrd$MeanOUR,vcontrd$Mean,method="spearman")
length(unique(vcontrd$verb)) #rho = .514, p-value = 0.02439

corr_inference = vcontrd %>% 
  summarize(Cor=cor(MeanOUR,Mean,method="spearman"))
corr_inference #.514

# comparison to our data -----

# recode their responses to numerical values, for comparison with our ratings (0-1) 
# MV has veridicality ratings on a 3-point Likert scale in response to "Did that thing happen?"
# veridicality reflects response options: "yes" "no" "maybe"
# create veridicality_num which codes "yes" as 1, "no" as 0 and "maybe" as .5
table(mv1$veridicality)
table(mv2$veridicality)

str(mv1$veridicality)
mv1$veridicality_num <- as.numeric(mv1$veridicality)
mv2$veridicality_num <- as.numeric(mv2$veridicality)

table(mv1$veridicality_num)
table(mv2$veridicality_num)

mv1$veridicality_num[mv1$veridicality_num == 1] <- .5 #1 was maybe
mv1$veridicality_num[mv1$veridicality_num == 2] <- 0 #2 was no
mv1$veridicality_num[mv1$veridicality_num == 3] <- 1 #3 was yes

mv2$veridicality_num[mv2$veridicality_num == 1] <- .5
mv2$veridicality_num[mv2$veridicality_num == 2] <- 0
mv2$veridicality_num[mv2$veridicality_num == 3] <- 1

# create items for mv1 (mv2 has "sentence" for items)

table(mv1$verb) #verb
table(mv1$frame) #only that_S
table(mv1$voice) #active, passive
table(mv1$polarity) #negative, positive
table(mv1$conditional) #true, false

str(mv1$conditional)
str(mv1$conditional2)
mv1$conditional2 <- as.character(mv1$conditional)
mv1$conditional2[mv1$conditional2 == "True"] <- "conditional"
mv1$conditional2[mv1$conditional2 == "False"] <- "matrix"

table(mv1$conditional2) #conditional, matrix

mv1$item <- paste(mv1$verb,mv1$frame,mv1$voice,mv1$polarity,mv1$conditional2, sep = "-")
table(mv1$item)

## aggregate data so that Stuttgart seminar students can work with CSV file
# summary(mv1)
# head(mv1)
# 
# aMV1 = mv1 %>%
#   select(item, verb, frame, voice, polarity, conditional2, veridicality,veridicality_num) %>%
#   group_by(item,verb,frame,voice,polarity,conditional2) %>%
#   summarize(Mean = mean(veridicality_num), low.ci = ci.low(veridicality_num), high.ci = ci.high(veridicality_num), SD = sd(veridicality_num), C = n())
# aMV1 = as.data.frame(aMV1)
# nrow(aMV1) #2176
# View(aMV1)
# write.csv(aMV1,"../data/aMV1.csv", row.names = FALSE)
# 
# summary(mv2)
# head(mv2)
# 
# aMV2 = mv2 %>%
#   select(verb,frame,voice,polarity,sentence,conditional,veridicality_num) %>%
#   group_by(sentence,verb,frame,voice,polarity,conditional) %>%
#   summarize(Mean = mean(veridicality_num), low.ci = ci.low(veridicality_num), high.ci = ci.high(veridicality_num), SD = sd(veridicality_num), C = n())
# aMV2 = as.data.frame(aMV2)
# nrow(aMV2) #5026
# View(aMV2)
# write.csv(aMV2,"../data/aMV2.csv", row.names = FALSE)

# we investigated 20 predicates: 
# be annoyed, discover, know, reveal, see
# pretend, suggest, say, think, be right, demonstrate
# acknowledge, admit, announce, confess, confirm, establish, hear, inform, prove

# their MV1 does not include "be right"
table(mv1$verb) 

# 19 predicates to compare
our_preds <- c("be_annoyed", "discover", "know", "reveal", "see", "pretend", "suggest", "say", "think", 
               "demonstrate", "acknowledge", "admit", "announce", "confess", "confirm", "establish", "hear", "inform", "prove")
our_preds
length(our_preds) #19

# rename their predicate "annoy" for comparison with our "be annoyed"
mv1 <- mv1 %>% 
  mutate(verb=recode(verb, annoy = "be_annoyed"))

# subset the data: only our predicates
mv1_s <- droplevels(subset(mv1, mv1$verb %in% our_preds))
nrow(mv1_s) #757
table(mv1_s$verb,mv1_s$voice) #39-40 ratings per pred; inform, be_annoyed only in passive voice; all others only in active

summary(mv1_s)

table(mv1_s$verb)

## plots of their findings for our predicates ----

# plot veridicality ratings (only use positive nonconditional matrix sentences)

table(mv1_s$verb,mv1_s$conditional2,mv1_s$polarity) #9-10 ratings per positive matrix sentence

# create temporary subset: ratings for positive nonconditional matrix sentences
mv1_s_tmp <- droplevels(subset(mv1_s, mv1_s$polarity == "positive" & mv1_s$conditional2 == "matrix"))
length(unique(mv1_s_tmp$participant)) #119 participants gave ratings

v_means = mv1_s_tmp %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
v_means
levels(v_means$verb)

mv1_s_tmp$verb <-factor(mv1_s_tmp$verb, levels=levels(v_means$verb))

# define colors for the predicates
cols = data.frame(V=levels(v_means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)
levels(cols$V)

v_means$VeridicalityGroup = as.factor(
  ifelse(v_means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(v_means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(v_means$verb  %in% c("demonstrate"),"VNF","V"))))

subjmeans = mv1_s_tmp %>%
  group_by(verb,participant) %>%
  summarize(Mean = mean(veridicality_num)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(v_means$verb)))
levels(subjmeans$verb)

ggplot(v_means, aes(x=verb, y=Mean,fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Mean veridicality rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-veridicality-by-predicate2.pdf",height=4,width=7)


# plot projection ratings (embedding is negation, conditional or both)

table(mv1_s$verb,mv1_s$conditional2,mv1_s$polarity) #29-30 ratings per predicate under negation, cond or both

mv1_s_tmp <- droplevels(subset(mv1_s, mv1_s$polarity == "negative" | mv1_s$conditional2 == "conditional"))
length(unique(mv1_s_tmp$participant)) #246 participants gave ratings

p_means = mv1_s_tmp %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
levels(p_means$verb)

mv1_s_tmp$verb <-factor(mv1_s_tmp$verb, levels=levels(p_means$verb))

# define colors for the predicates
cols = data.frame(V=levels(p_means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(p_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(p_means$verb)),]$V, ordered = TRUE)
levels(cols$V)

p_means$VeridicalityGroup = as.factor(
  ifelse(p_means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(p_means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(p_means$verb  %in% c("demonstrate"),"VNF","V"))))

subjmeans = mv1_s_tmp %>%
  group_by(verb,participant) %>%
  summarize(Mean = mean(veridicality_num)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(p_means$verb)))
levels(subjmeans$verb)

ggplot(p_means, aes(x=verb, y=Mean,fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Mean projectivity rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-projection-by-predicate.pdf",height=4,width=7)

## comparison between our ratings and MegaVeridicality ratings ----

# load our continuous data 

# projection
d_proj = read.csv("../../results/5-projectivity-no-fact/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(d_proj$verb)

# inference diagnostic
d_inf = read.csv("../../results/4-veridicality3/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(d_inf$verb)

# contradictoriness diagnostics
d_contr = read.csv("../../results/2-veridicality2/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
table(d_contr$verb)

# for comparison, remove main clauses, controls and "be_right" from our data 
d_proj <- droplevels(subset(d_proj, d_proj$verb != "MC" & d_proj$verb != "be_right"))
d_inf <- droplevels(subset(d_inf, d_inf$verb != "control_good" & d_inf$verb != "control_bad" & d_inf$verb != "be_right"))
d_contr <- droplevels(subset(d_contr, d_contr$verb != "control_good" & d_contr$verb != "control_bad" & d_contr$verb != "be_right"))

# compare projectivity ratings

our_p_means = d_proj %>%
  group_by(verb) %>%
  summarize(MeanOUR = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinOUR = MeanOUR - CILow, YMaxOUR = MeanOUR + CIHigh) %>%
  select(-CILow,-CIHigh)
table(our_p_means$verb)
str(our_p_means$verb)
table(p_means$verb)
str(p_means$verb)

head(our_p_means)
head(p_means)

# make sure that the two verb factors have the same levels
combined <- sort(union(levels(our_p_means$verb), levels(p_means$verb)))
combined

pd = mutate(our_p_means, verb=factor(verb, levels=combined)) %>%
  left_join(mutate(p_means, verb=factor(verb, levels=combined))) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("demonstrate") ~ "VNF",
    TRUE ~ "V")))
str(pd$verb)

# define colors for the predicates
cols = data.frame(V=levels(pd$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(p_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(p_means$verb)),]$V, ordered = TRUE)
levels(cols$V)


ggplot(pd, aes(x=MeanOUR, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMinOUR,xmax=YMaxOUR),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label=verb),color=cols$Colors,size=3) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean 'did that happen?' rating") +
  xlab("Mean certainty rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/projectivity-comparison.pdf",height=4,width=4)

cor.test(pd$MeanOUR,pd$Mean,method="spearman")
length(unique(pd$verb)) #19 verbs, rho = .852, p-value < .001

corr_projectivity = pd %>%
  summarize(Cor=cor(MeanOUR,Mean,method="spearman"))
corr_projectivity #.852

# compare veridicality ratings I: our inference diagnostic

our_vinf_means = d_inf %>%
  group_by(verb) %>%
  summarize(MeanOUR = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinOUR = MeanOUR - CILow, YMaxOUR = MeanOUR + CIHigh) %>%
  select(-CILow,-CIHigh)
our_vinf_means 

# make sure that the two verb factors have the same levels
combined <- sort(union(levels(our_vinf_means$verb), levels(v_means$verb)))
combined

vinfd = mutate(our_vinf_means, verb=factor(verb, levels=combined)) %>%
  left_join(mutate(v_means, verb=factor(verb, levels=combined))) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("demonstrate") ~ "VNF",
    TRUE ~ "V")))
str(vinfd$verb)

# define colors for the predicates
cols = data.frame(V=levels(vinfd$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)
levels(cols$V)


ggplot(vinfd, aes(x=MeanOUR, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMinOUR,xmax=YMaxOUR),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label=verb),color=cols$Colors,size=3) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean 'did that happen?' rating") +
  xlab("Mean inference rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-inference-comparison.pdf",height=4,width=4)

cor.test(vinfd$MeanOUR,vinfd$Mean,method="spearman")
length(unique(vinfd$verb)) #19 verbs, rho = .596, p-value = 0.07126

corr_inference = vinfd %>% 
  summarize(Cor=cor(MeanOUR,Mean,method="spearman"))
corr_inference #.596

# compare veridicality ratings II: our contradictoriness diagnostic

our_vcontr_means = d_contr %>%
  group_by(verb) %>%
  summarize(MeanOUR = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinOUR = MeanOUR - CILow, YMaxOUR = MeanOUR + CIHigh) %>%
  select(-CILow,-CIHigh)
our_vcontr_means 

vcontrd = mutate(our_vcontr_means, verb=factor(verb, levels=combined)) %>%
  left_join(mutate(v_means, verb=factor(verb, levels=combined))) %>%
    mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("demonstrate") ~ "VNF",
    TRUE ~ "V")))

str(vcontrd$verb)

# define colors for the predicates
cols = data.frame(V=levels(vcontrd$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(v_means$verb)),]$V, ordered = TRUE)
levels(cols$V)


ggplot(vcontrd, aes(x=MeanOUR, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_errorbarh(aes(xmin=YMinOUR,xmax=YMaxOUR),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label=verb),color=cols$Colors,size=3) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean 'did that happen?' rating") +
  xlab("Mean contradictoriness rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-contradictoriness-comparison.pdf",height=4,width=4)

cor.test(vcontrd$MeanOUR,vcontrd$Mean,method="spearman")
length(unique(vcontrd$verb)) #rho = .514, p-value = 0.02439

corr_inference = vcontrd %>% 
  summarize(Cor=cor(MeanOUR,Mean,method="spearman"))
corr_inference #.514

# AUX for SALT 2022 paper ----
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692
table(mv1$conditional2,mv1$polarity)

# create new embedding variable
mv1 = mv1 %>%
  mutate(embedding = case_when(
    polarity == "negative" & conditional2 == "matrix"  ~ "n",
    polarity == "negative" & conditional2 == "conditional"  ~ "ncq",
    polarity == "positive" & conditional2 == "conditional"  ~ "cq",
    TRUE ~ "p"
  ))

# sanity check
table(mv1$embedding)

# subset to relevant embeddings
mv1 <- droplevels(subset(mv1, mv1$embedding != "p"))
table(mv1$embedding)

# verb per embedding
table(mv1$verb,mv1$embedding) #mostly 10 ratings per verb & embedding, sometimes 9 or 20

length(unique(mv1$participant)) #290 participants gave ratings

# get the relevant predicates to plot
# we had 20 predicates, their data does not include "be right"
our_preds <- c("be annoyed", "discover", "know", "reveal", "see", "pretend", "suggest", "say", "think", 
               "demonstrate", "acknowledge", "admit", "announce", "confess", "confirm", "establish", "hear", "inform", "prove")
our_preds
length(our_preds) #19

emotive = c("feel", "trust", "desire", "fear", "worry", "mourn", "grieve", "frighten", "envy", "scare", "anger",
            "freak_out", "frustrate", "petrify", "puzzle", "regret", "devastate", "disappoint", "embitter", "shame",
            "dismay", "be annoyed", "content", "detest", "disturb", "hate", "traumatize", "enjoy", "embarrass",
            "irritate", "amuse", "elate", "love", "pain", "upset", "bother", "resent")
length(emotive) #37

cognitive = c("presuppose", "think", "believe", "suppose", "find", "discover", "see", "remember", "know", "comprehend",
              "conceive", "contemplate", "disbelieve", "dispute", "doubt", "estimate", "figure", "figure_out", "gather",
              "learn", "maintain", "notice", "realize", "recognize", "rediscover", "reveal", "see", "hear",
              "understand")
length(cognitive) #29

# rename their predicate "annoy" for comparison with our "be annoyed"
mv1 <- mv1 %>% 
  mutate(verb=recode(verb, annoy = "be annoyed"))

all_verbs <- c(our_preds, emotive,cognitive)
all_verbs
length(unique(all_verbs)) #77

d <- droplevels(subset(mv1, mv1$verb %in% all_verbs))
length(unique(d$verb)) #77
#View(d)

# to order predicates by mean projection
tmp = d %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))

nrow(tmp) #77
levels(tmp$verb) # ordered by mean, not alphabetically
length(tmp$verb) #77

# calculate projection mean by predicate and embedding
means = d %>%
  group_by(verb,embedding) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

# relevel verb by overall mean
means$verb <- factor(means$verb, levels = unique(levels(tmp$verb)))
levels(p_means$verb)

# color code the verbs
cols = data.frame(verb=levels(means$verb))
cols
nrow(cols)

cols$type = as.factor(
  ifelse(cols$verb %in% cognitive, "cognitive", 
         ifelse(cols$verb %in% emotive, "emotive", "other")))
cols
         
cols$Colors =  ifelse(cols$type == "emotive", "#D55E00", 
                      ifelse(cols$type == "cognitive", "#5b43c4", "black"))
cols

ggplot(means, aes(x=verb, y=Mean, color = embedding, group = embedding)) +
  geom_text(aes(label = embedding, size = 150)) +
  geom_line(aes(color=embedding), size=.5) + 
  theme(#panel.grid.major.x = element_blank(), 
        axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none", size = "none") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=12)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/emotive-cognitive.pdf",height=4,width=9)

# compare emotive, cognitive, communicative, inferential ----
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692
table(mv1$conditional2,mv1$polarity)

# create new embedding variable
mv1 = mv1 %>%
  mutate(embedding = case_when(
    polarity == "negative" & conditional2 == "matrix"  ~ "n",
    polarity == "negative" & conditional2 == "conditional"  ~ "ncq",
    polarity == "positive" & conditional2 == "conditional"  ~ "cq",
    TRUE ~ "p"
  ))

# sanity check
table(mv1$embedding)

# subset to relevant embeddings
mv1 <- droplevels(subset(mv1, mv1$embedding != "p"))
table(mv1$embedding)

# verb per embedding
table(mv1$verb,mv1$embedding) #mostly 10 ratings per verb & embedding, sometimes 9 or 20

length(unique(mv1$participant)) #290 participants gave ratings

# get the relevant predicates to plot
table(mv1$verb)

emotive = c("trust", "desire", "fear", "worry", "mourn", "grieve", "frighten", "envy", "scare", "anger",
            "freak_out", "frustrate", "petrify", "puzzle", "regret", "devastate", "disappoint", "embitter", "shame",
            "dismay", "be annoyed", "content", "detest", "disturb", "hate", "traumatize", "enjoy", "embarrass",
            "irritate", "amuse", "elate", "love", "pain", "upset", "bother", "resent")
length(emotive) #37

cognitive = c("feel", "presuppose", "think", "believe", "suppose", "find", "discover", "see", "remember", "know", "comprehend",
              "conceive", "contemplate", "disbelieve", "dispute", "doubt", "estimate", "gather", "find_out", 
              "learn", "maintain", "notice", "realize", "recognize", "rediscover", "reveal", "see", "hear",
              "understand", "assume")
length(cognitive) #29

communicative = c("add", "advise", "alert", "argue", "announce", "assert", "assure", "babble", "bark", "cackle", "claim", 
                  "communicate", "comment", "confess", "confirm", "convey", "declare", "discuss", "elaborate", "email",
                  "express", "fax", "gab", "gloat", "gossip", "growl", "grunt", "gush", "hint", "inform", "insist", 
                  "lecture", "lie", "maintain", "mention", "mutter", "narrate", "note", "phone", "publicize", "publish",
                  "quip", "quote", "rant", "reassert", "recap", "reiterate", "repeat", "report", "say", "sing", "sob",
                  "state", "stress", "tell", "underline", "utter", "vow", "warn", "weep", "whimper", "whine", "whisper",
                  "wow", "yell")
length(communicative) #64

inferential = c("anticipate", "calculate", "compute", "conclude", "conjecture", "deduce", "derive", "establish", "estimate",
                "expect", "figure", "figure_out", "generalize", "infer", "piece_together", "pinpoint", "predict",
                "reason", "reason_out", "verify")
length(inferential) #20

# rename their predicate "annoy" for comparison with our "be annoyed"
mv1 <- mv1 %>% 
  mutate(verb=recode(verb, annoy = "be annoyed"))

all_verbs <- c(emotive,cognitive,communicative,inferential)
all_verbs
length(unique(all_verbs)) #147

d <- droplevels(subset(mv1, mv1$verb %in% all_verbs))
length(unique(d$verb)) #147
#View(d)

# by-predicate mean
mean.predicate = d %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))

nrow(mean.predicate) #147
levels(mean.predicate$verb) # ordered by mean, not alphabetically
length(mean.predicate$verb) #147

# by-type mean
d$type = as.factor(
  ifelse(d$verb %in% cognitive, "cognitive", 
         ifelse(d$verb %in% emotive, "emotive", 
                ifelse(d$verb %in% inferential, "inferential", "communicative"))))

table(d$type)
summary(d)

mean.type = d %>%
  group_by(type) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, type = fct_reorder(as.factor(type),Mean))

nrow(mean.type) #4

# color code the verbs
cols = data.frame(verb=levels(mean.predicate$verb))
cols
nrow(cols)

cols$type = as.factor(
  ifelse(cols$verb %in% cognitive, "cognitive", 
         ifelse(cols$verb %in% emotive, "emotive", 
                ifelse(cols$verb %in% inferential, "inferential", "communicative"))))
cols
table(cols$type)

cols$Colors =  ifelse(cols$type == "emotive", "#D55E00", 
                      ifelse(cols$type == "cognitive", "#5b43c4", 
                            ifelse(cols$type == "communicative", "gray", "green")))
cols

# add type to the dataset
mean.predicate$type = as.factor(
  ifelse(mean.predicate$verb %in% cognitive, "cognitive", 
         ifelse(mean.predicate$verb %in% emotive, "emotive", 
                ifelse(mean.predicate$verb %in% inferential, "inferential", "communicative"))))

mean.predicate$Colors =  ifelse(mean.predicate$type == "emotive", "#D55E00", 
                      ifelse(mean.predicate$type == "cognitive", "#5b43c4", 
                             ifelse(mean.predicate$type == "communicative", "gray", "green")))

ggplot(mean.predicate, aes(x=verb, y=Mean)) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  #scale_color_manual(values = colors) +
  #geom_line(aes(color=type), size=.5) + 
  geom_hline(yintercept=.3) +
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/by-predicate-and-type.pdf",height=4,width=13)

ggplot(mean.type, aes(x=type, y=Mean)) +
  geom_point(color = "black", size = 1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  #scale_color_manual(values = colors) +
  #geom_line(aes(color=type), size=.5) + 
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/by-predicate-type.pdf",height=3,width=5)

# SALT abstract 2023 plot ----
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692
table(mv1$conditional2,mv1$polarity)

# create new embedding variable
mv1 = mv1 %>%
  mutate(embedding = case_when(
    polarity == "negative" & conditional2 == "matrix"  ~ "n",
    polarity == "negative" & conditional2 == "conditional"  ~ "ncq",
    polarity == "positive" & conditional2 == "conditional"  ~ "cq",
    TRUE ~ "p"
  ))

# sanity check
table(mv1$embedding)

# subset to relevant embeddings (negation, conditional/question)
mv1 <- droplevels(subset(mv1, mv1$embedding != "p" & mv1$embedding != "ncq"))
table(mv1$embedding)

# subset to potentially relevant predicates
our_preds <- c("be annoyed", "discover", "know", "reveal", "see", "pretend", "suggest", "say", "think", 
               "demonstrate", "acknowledge", "admit", "announce", "confess", "confirm", "establish", "hear", "inform", "prove")

emotive = c("trust", "desire", "fear", "worry", "mourn", "grieve", "frighten", "envy", "scare", "anger",
                   "freak_out", "frustrate", "petrify", "puzzle", "regret", "devastate", "disappoint", "embitter", "shame",
                   "dismay", "be annoyed", "content", "detest", "disturb", "hate", "traumatize", "enjoy", "embarrass",
                   "irritate", "amuse", "elate", "love", "pain", "upset", "bother", "resent")

cognitive = c("feel", "presuppose", "think", "believe", "suppose", "find", "discover", "see", "remember", "know", "comprehend",
                     "conceive", "contemplate", "disbelieve", "dispute", "doubt", "estimate", "gather", "find_out", 
                     "learn", "maintain", "notice", "realize", "recognize", "rediscover", "reveal", "see", "hear",
                     "understand", "assume")

preds <- c(our_preds, emotive, cognitive)

d <- droplevels(subset(mv1, mv1$verb %in% select))
length(unique(d$verb)) #71

table(d$verb,d$embedding) #9-10 ratings per verb/embedding combination (20 for "worry", "freak out")

# by-predicate and embedding projection mean
means = d %>%
  group_by(verb,embedding) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))

# to order predicates by mean projection
tmp = d %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))

nrow(tmp) #71
levels(tmp$verb) # ordered by mean, not alphabetically
length(tmp$verb) #71

# relevel verb by overall mean
means$verb <- factor(means$verb, levels = unique(levels(tmp$verb)))
levels(means$verb)

# color code the verbs
cols = data.frame(verb=levels(means$verb))
cols
nrow(cols)

cols$type = as.factor(
  ifelse(cols$verb %in% cognitive, "cognitive", 
         ifelse(cols$verb %in% emotive, "emotive", "our")))
                
cols
table(cols$type) # if one of our predicates is cognitive or emotive, it is coded as that, not our
# use verb %in% our_preds for bold-facing in graph

cols$Colors =  ifelse(cols$type == "emotive", "#D55E00", 
                      ifelse(cols$type == "cognitive", "#5b43c4", "gray"))
cols

# add type to the dataset
means$type = as.factor(
  ifelse(means$verb %in% cognitive, "cognitive", 
         ifelse(means$verb %in% emotive, "emotive", "our")))
                

means$Colors =  ifelse(means$type == "emotive", "#D55E00", 
                                       ifelse(means$type == "cognitive", "#5b43c4", "gray"))
                                              


ggplot(means, aes(x=verb, y=Mean, color = embedding, group = embedding)) +
  geom_text(aes(label = embedding, size = 150)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  geom_line(aes(color=embedding), size=.5) + 
  theme(#panel.grid.major.x = element_blank(), 
    axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none", size = "none") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=12)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
#ggsave("../graphs/by-predicate-and-operator.pdf",height=4,width=13)

# identify suitable predicates to illustrate variation by embedding operator
#View(means)
str(means)

# include predicates with an at-ceiling mean under either embedding, or one of our predicates
means$n_ceiling = ifelse(means$embedding == "n" & means$Mean == 1, "true", "false") 
table(means$n_ceiling) #3 true

means$cq_ceiling = ifelse(means$embedding == "cq" & means$Mean == 1, "true", "false")
table(means$cq_ceiling) #22 true

our_preds #19

tmp <- droplevels(subset(means, means$n_ceiling == "true" | means$cq_ceiling == "true" | means$verb %in% our_preds))
nrow(tmp) #55
unique(tmp$verb) #35 verbs (not all with both embeddings)

means_select <- droplevels(subset(means, means$verb %in% tmp$verb))
nrow(means_select) #70 (= 2 embeddings x 35 verbs)

ggplot(means_select, aes(x=verb, y=Mean, color = embedding, group = embedding)) +
  geom_text(aes(label = embedding, size = 150)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  guides(size = "none") +
  theme(axis.text.x = element_text(face = ifelse(levels(means_select$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=12)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/by-predicate-and-operator.pdf",height=4,width=13)

# AUX for lexical semantics project ----
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692
table(mv1$conditional2,mv1$polarity)

# create new embedding variable
mv1 = mv1 %>%
  mutate(embedding = case_when(
    polarity == "negative" & conditional2 == "matrix"  ~ "n",
    polarity == "negative" & conditional2 == "conditional"  ~ "ncq",
    polarity == "positive" & conditional2 == "conditional"  ~ "cq",
    TRUE ~ "p"
  ))

# sanity check
table(mv1$embedding)

# subset to relevant embeddings
mv1.verid <- droplevels(subset(mv1, mv1$embedding == "p"))
mv1.proj <- droplevels(subset(mv1, mv1$embedding != "p"))
table(mv1.verid$embedding)
table(mv1.proj$embedding)

# verb per embedding
table(mv1.verid$verb,mv1$embedding)
table(mv1.proj$verb,mv1$embedding) #mostly 10 ratings per verb & embedding, sometimes 9 or 20

length(unique(mv1$participant)) #159 participants gave ratings

# get the relevant predicates to plot
table(mv1.proj$verb)
means = mv1 %>%
  group_by(verb) %>%
  summarize(Mean = mean(veridicality_num))
View(means)



evidential = c("discover", "dream", "establish", "feel", "figure_out", "figure", "find_out",
               "generalize", "hallucinate", "hear", "imagine", "infer", "learn", "listen", 
               "notice", "observe", "overhear", "perceive", "piece_together", "realize",
               "reason_out", "recognize", "see")

emotive = c("trust", "desire", "fear", "worry", "mourn", "grieve", "frighten", "envy", "scare", "anger",
            "freak_out", "frustrate", "petrify", "puzzle", "regret", "devastate", "disappoint", "embitter", "shame",
            "dismay", "be annoyed", "content", "detest", "disturb", "hate", "traumatize", "enjoy", "embarrass",
            "irritate", "amuse", "elate", "love", "pain", "upset", "bother", "resent")
length(emotive) #37

cognitive = c("feel", "presuppose", "think", "believe", "suppose", "find", "discover", "see", "remember", "know", "comprehend",
              "conceive", "contemplate", "disbelieve", "dispute", "doubt", "estimate", "gather", "find_out", 
              "learn", "maintain", "notice", "realize", "recognize", "rediscover", "reveal", "see", "hear",
              "understand", "assume")
length(cognitive) #29

communicative = c("add", "advise", "alert", "argue", "announce", "assert", "assure", "babble", "bark", "cackle", "claim", 
                  "communicate", "comment", "confess", "confirm", "convey", "declare", "discuss", "elaborate", "email",
                  "express", "fax", "gab", "gloat", "gossip", "growl", "grunt", "gush", "hint", "inform", "insist", 
                  "lecture", "lie", "maintain", "mention", "mutter", "narrate", "note", "phone", "publicize", "publish",
                  "quip", "quote", "rant", "reassert", "recap", "reiterate", "repeat", "report", "say", "sing", "sob",
                  "state", "stress", "tell", "underline", "utter", "vow", "warn", "weep", "whimper", "whine", "whisper",
                  "wow", "yell")
length(communicative) #64

inferential = c("anticipate", "calculate", "compute", "conclude", "conjecture", "deduce", "derive", "establish", "estimate",
                "expect", "figure", "figure_out", "generalize", "infer", "piece_together", "pinpoint", "predict",
                "reason", "reason_out", "verify")
length(inferential) #20

# chose here for plotting
all_verbs <- c(emotive,cognitive,communicative,inferential,evidential)
#all_verbs <- c(evidential)
all_verbs
length(unique(all_verbs)) #148

d.proj <- droplevels(subset(mv1.proj, mv1.proj$verb %in% all_verbs))
d.verid <- droplevels(subset(mv1.proj, mv1.proj$verb %in% all_verbs))
length(unique(d$verb)) #148
#View(d)

# by-predicate means
mean.proj = d.proj %>%
  group_by(verb) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb = fct_reorder(as.factor(verb),Mean.Proj))

mean.verid = d.verid %>%
  group_by(verb) %>%
  summarize(Mean.Verid = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Verid = Mean.Verid - CILow, YMax.Verid = Mean.Verid + CIHigh, verb = fct_reorder(as.factor(verb),mean.proj$Mean.Proj))

nrow(mean.proj)
nrow(mean.verid)
levels(mean.proj$verb) # ordered by mean, not alphabetically

# # by-type mean
# d$type = as.factor(
#   ifelse(d$verb %in% cognitive, "cognitive", 
#          ifelse(d$verb %in% emotive, "emotive", 
#                 ifelse(d$verb %in% inferential, "inferential", "communicative"))))
# 
# table(d$type)
# summary(d)
# 
# mean.type = d %>%
#   group_by(type) %>%
#   summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
#   mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, type = fct_reorder(as.factor(type),Mean))
# 
# nrow(mean.type) #4

# color code the verbs
cols = data.frame(verb=levels(mean.proj$verb))
cols
nrow(cols)

cols$type = as.factor(
  ifelse(cols$verb %in% cognitive, "cognitive", 
         ifelse(cols$verb %in% emotive, "emotive", 
                ifelse(cols$verb %in% inferential, "inferential", 
                       ifelse(cols$verb %in% communicative, "communicative", "evidential")))))
cols
table(cols$type)

cols$Colors =  ifelse(cols$type == "emotive", "#D55E00", 
                      ifelse(cols$type == "cognitive", "#5b43c4", 
                             ifelse(cols$type == "communicative", "gray", 
                                    ifelse(cols$type == "inferential", "green", "black"))))
cols

# add type to the dataset
mean.proj$type = as.factor(
  ifelse(mean.proj$verb %in% cognitive, "cognitive", 
         ifelse(mean.proj$verb %in% emotive, "emotive", 
                ifelse(mean.proj$verb %in% inferential, "inferential", 
                       ifelse(mean.proj$verb %in% "communicative", "communicative", "evidential")))))

mean.verid$type = as.factor(
  ifelse(mean.verid$verb %in% cognitive, "cognitive", 
         ifelse(mean.verid$verb %in% emotive, "emotive", 
                ifelse(mean.verid$verb %in% inferential, "inferential", 
                       ifelse(mean.verid$verb %in% "communicative", "communicative", "evidential")))))


mean.proj$Colors =  ifelse(mean.proj$type == "emotive", "#D55E00", 
                                ifelse(mean.proj$type == "cognitive", "#5b43c4", 
                                       ifelse(mean.proj$type == "communicative", "gray", 
                                              ifelse(mean.proj$type == "inferential", "green", "black"))))

mean.verid$Colors =  ifelse(mean.verid$type == "emotive", "#D55E00", 
                           ifelse(mean.verid$type == "cognitive", "#5b43c4", 
                                  ifelse(mean.verid$type == "communicative", "gray", 
                                         ifelse(mean.verid$type == "inferential", "green", "black"))))

tmp = mean.proj %>% 
  left_join(mean.verid)
View(tmp)

ggplot(tmp, aes(x=Mean.Verid, y=Mean.Proj)) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  #scale_color_manual(values = colors) +
  #geom_line(aes(color=type), size=.5) + 
  geom_hline(yintercept=.3) +
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/veridicality-by-predicate-and-type.pdf",height=4,width=13)
ggsave("../graphs/projection-for-evidential.pdf",height=4,width=13)






