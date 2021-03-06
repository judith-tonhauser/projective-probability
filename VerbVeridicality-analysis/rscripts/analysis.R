# Analysis of veridicality and projection ratings from 
# Ross & Pavlick VerbVeridicality dataset

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
library(splitstackshape)
theme_set(theme_bw())

## preprocessing ----

# load raw data
d = read.delim(file='../data/verb_veridicality_evaluation.tsv')
#View(d)
head(d)
names(d)

# "Our final dataset contains 137 verb types across 1,498 sentences (2,996 pairs)."
nrow(d) #1498 (1498 rows with veridicality + projection info)
length(unique(d$index)) #1498
length(unique(d$verb)) #124

# remove BERT columns for easier handling
d = d %>%
    select(-bert_neg_contradiction_prob,-bert_neg_entailment_prob,-bert_neg_neutral_prob,-bert_pos_contradiction_prob,-bert_pos_entailment_prob,-bert_pos_neutral_prob)
names(d)
head(d)
tail(d)

# reformat columns containing turker ratings
table(d$turker_pos_ratings)
table(d$turker_neg_ratings)

# veridicality ratings (positive ratings)
d <- d %>% 
  mutate(pos_rating = str_split(as.character(turker_pos_ratings), ",")) %>% 
  unnest(pos_rating) 
head(d)
tail(d)
nrow(d) # 4490 (but this should be 4494 = 1498 x 3)
length(unique(d$index)) # 1498
length(unique(d$verb)) #124

# factivity ratings (negative ratings)
d <- d %>% 
  mutate(neg_rating = str_split(as.character(turker_neg_ratings), ",")) %>% 
  unnest(neg_rating) 
head(d)
tail(d)
nrow(d) # 13470 (but this should be 13483 = 4494 * 3)
length(unique(d$index)) # 1498
length(unique(d$verb)) #124

# "We have raters label entailment on a 5-point likert scale in which -2 means that h is definitely not true 
# given p and 2 means that h is definitely true given p.
table(d$pos_rating)
table(d$neg_rating)

table(d$verb)

# of our 20 predicates, the following are included in this dataset

# not included: confess, establish, inform, be_annoyed, be_right
# both "see" and "saw" are included in their data

# our 19 predicates 
our_preds <- c("saw", "discover", "know", "reveal", "see", "pretend", "suggest", "say", "think", 
               "demonstrate", "acknowledge", "admit", "announce", "confirm", "hear", "prove")
our_preds
length(our_preds) #16 (15 of our preds + "saw)

# remove items with "to VP" embedded clause
table(d$task)
d <- droplevels(subset(d, d$task == "that"))

names(d)
length(unique(d$verb)) #78 verbs
length(unique(d$index)) #859 total items: each has sentence and its negated variant

# make ratings numeric
str(d$pos_rating)
d$pos_rating <- as.numeric(d$pos_rating)
d$neg_rating <- as.numeric(d$neg_rating)

# plot veridicality ratings ----

# calculate veridicality mean
v_means = d %>%
  group_by(verb) %>%
  summarize(Mean = mean(pos_rating), CILow = ci.low(pos_rating), CIHigh = ci.high(pos_rating)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
v_means
levels(v_means$verb) # verbs sorted by veridicality mean (...)

# predicates with mean ratings of 2 (indicating consistent 3 'definitely true' judgments), i.e., entailed CCs
v_means[v_means$Mean == 2,]$index #0

# at least two raters said 2, one only said 1
v_means[v_means$Mean > 1.66,]$verb
# explain give notice
table(v_means[v_means$Mean > 1.66,]$verb,v_means[v_means$Mean > 1.66,]$Mean) 
# these 3 preds have means between 1.66 and 1.7

# at least one rater said 2, the other two only 1
v_means[v_means$Mean > 1.33,]$verb
# 25 predicates, including "announce", "mention", "add", "note" (not obviously veridical)

d[d$verb == "give",]$sentence
d[d$verb == "explain",]$sentence

# at least as high as "know", i.e., 1.5
v_means[v_means$Mean >= 1.5,]$verb
#15 predicates 
# [1] admit       confirm     demonstrate
# [4] explain     give        know       
# [7] learn       note        notice     
# [10] prove       provide     realize    
# [13] remember    reveal      saw

# heterogeneous projection ratings of these predicates:
# verb           Mean  CILow CIHigh    YMin    YMax
# <fct>         <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
#   1 acknowledge  0.455  0.172  0.162   0.283   0.616 
# 3 admit        0.292  0.167  0.188   0.125   0.479 
# 18 confirm     -0.121  0.132  0.141  -0.253   0.0202
# 24 demonstrate -0.667  0.122  0.122  -0.789  -0.544 
# 31 explain      0.9    0.222  0.2     0.678   1.1   
# 36 give         1.14   0.222  0.190   0.921   1.33  
# 43 know         0.781  0.108  0.0973  0.673   0.879 
# 44 learn        0.364  0.162  0.162   0.202   0.525 
# 47 note         0      0.234  0.233  -0.234   0.233 
# 48 notice       1.06   0.148  0.148   0.907   1.20  
# 52 prove       -0.267  0.167  0.178  -0.433  -0.0889
# 53 provide     -0.2    0.245  0.233  -0.445   0.0333
# 54 realize      1.05   0.0948 0.105   0.954   1.15  
# 57 remember     0.784  0.124  0.131   0.660   0.915 
# 62 reveal      -0.194  0.222  0.222  -0.417   0.0278
# 63 saw          0.424  0.212  0.242   0.212   0.667 

# create data subsets for our predicates
v_meansOUR <- droplevels(subset(v_means,v_means$verb %in% our_preds))
v_meansOUR
levels(v_meansOUR$verb) #verbs sorted by projectivity mean (pretend...reveal)

v_meansOUR = v_meansOUR %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))

# define colors for our predicates
cols = data.frame(V=levels(v_meansOUR$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "saw", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)

cols = cols %>%
  mutate(V = fct_reorder(as.factor(V),v_meansOUR$Mean))

levels(cols$V) # sorted: 

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


v_meansOUR$VeridicalityGroup = as.factor(
  ifelse(v_meansOUR$verb %in% c("know", "discover", "reveal", "see", "saw", "be_annoyed"), "F", 
         ifelse(v_meansOUR$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(v_meansOUR$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(v_meansOUR$verb  %in% c("MC"),"MC","V")))))

v_means$VeridicalityGroup = as.factor(
  ifelse(v_means$verb %in% c("know", "discover", "reveal", "see", "saw", "be_annoyed"), "F", 
         ifelse(v_means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(v_means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(v_means$verb  %in% c("hear","acknowledge","confess","prove","confirm","establish","inform","announce","admit"),"V","X")))))

cols$Colors

v_meansOUR = v_meansOUR %>%
  mutate(VeridicalityGroup = fct_relevel(VeridicalityGroup, "NF","VNF","V","F"))
levels(v_meansOUR$VeridicalityGroup)
# "NF"  "VNF" "V"   "F" 

ggplot(v_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray") +
  geom_point(shape=16,stroke=.5,size=2.5,color="palegreen4") +
  #scale_fill_manual(values=c("gray60","dodgerblue","tomato1","darkorchid","palegreen4")) + 
  geom_text_repel(data=v_meansOUR,aes(x=verb,y=Mean,label=verb,color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.8) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_color_manual(values=c(NF="gray60",VNF="dodgerblue",V="tomato1",F="darkorchid"),
                     labels = c("non-veridical\nnon-factive","veridical\nnon-factive","optionally\nfactive","factive")) +
  scale_y_continuous(limits = c(-1,2),breaks = c(-1,0,1,2)) +
  #scale_alpha(range = c(.3,1)) +
  labs(color="Predicate type") +
  theme(legend.position="bottom") + 
  ylab("Mean veridicality rating") +
  xlab("Predicate") 
ggsave("../graphs/means-entailment-by-predicate.pdf",height=4,width=9)


# plot projection ratings  ----

p_means = d %>%
  group_by(verb) %>%
  summarize(Mean = mean(neg_rating), CILow = ci.low(neg_rating), CIHigh = ci.high(neg_rating)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
levels(p_means$verb) # verbs sorted by projection mean (...)

# create data subsets for our predicates
p_meansOUR <- droplevels(subset(p_means,p_means$verb %in% our_preds))
p_meansOUR
levels(p_meansOUR$verb) #verbs sorted by projectivity mean (pretend...reveal)

p_meansOUR = p_meansOUR %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))

# define colors for our predicates
cols = data.frame(V=levels(p_meansOUR$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "saw", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("demonstrate"),"VNF", "V"))))

levels(cols$V)

cols = cols %>%
  mutate(V = fct_reorder(as.factor(V),p_meansOUR$Mean))

levels(cols$V) # sorted: 

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue","tomato1")))


p_meansOUR$VeridicalityGroup = as.factor(
  ifelse(p_meansOUR$verb %in% c("know", "discover", "reveal", "see", "saw", "be_annoyed"), "F", 
         ifelse(p_meansOUR$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(p_meansOUR$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(p_meansOUR$verb  %in% c("MC"),"MC","V")))))

p_means$VeridicalityGroup = as.factor(
  ifelse(p_means$verb %in% c("know", "discover", "reveal", "see", "saw", "be_annoyed"), "F", 
         ifelse(p_means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(p_means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(p_means$verb  %in% c("hear","acknowledge","confess","prove","confirm","establish","inform","announce","admit"),"V","X")))))

cols$Colors

p_meansOUR = p_meansOUR %>%
  mutate(VeridicalityGroup = fct_relevel(VeridicalityGroup, "NF","VNF","V","F"))
levels(p_meansOUR$VeridicalityGroup)
# "NF"  "VNF" "V"   "F" 

ggplot(p_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray") +
  geom_point(shape=16,stroke=.5,size=2.5,color="palegreen4") +
  #scale_fill_manual(values=c("gray60","dodgerblue","tomato1","darkorchid","palegreen4")) + 
  geom_text_repel(data=p_meansOUR,aes(x=verb,y=Mean,label=verb,color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.8) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_color_manual(values=c(NF="gray60",VNF="dodgerblue",V="tomato1",F="darkorchid"),
                     labels = c("non-veridical\nnon-factive","veridical\nnon-factive","optionally\nfactive","factive")) +
  scale_y_continuous(limits = c(-1.3,2),breaks = c(-1,0,1,2)) +
  #scale_alpha(range = c(.3,1)) +
  labs(color="Predicate type") +
  theme(legend.position="bottom") + 
  ylab("Mean projection rating") +
  xlab("Predicate") 
ggsave("../graphs/means-projection-by-predicate.pdf",height=4,width=9)

