# experiment investigating prior and projection
# contents of complements of 20 predicates
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages for pre-processing data
library(tidyverse)
theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #15600 / 300 turkers = 52 trials 
head(d)
summary(d) #300 unique workerids

length(unique(d$workerid)) #300

# both blocks occurred first (randomization worked)
table(d$block,d$question_type)

# count of how often each Turker did the experiment
count = d %>%
  select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
count
# View(count)
# nobody did the pilot more than once (this was run with UniqueTurker)

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #300
nrow(ds) #300
head(ds)
summary(d) # experiment took 8.5 minutes (median), 9.2 minutes (mean)

# look at Turkers' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

mean(d$Answer.time_in_minutes) #9.17
median(d$Answer.time_in_minutes) #8.45

nrow(d) #15600

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) #18-82
median(d$age,na.rm=TRUE) #35.5
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#119 female, 179 male, 1 other, 1 undeclared

# no recoding of responses
#table(d$question_type,d$response)
#d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 57
d$trial = d$slide_number_in_experiment - 4
unique(d$trial) # trial numbers from 1 to 53 (27 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 52

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 

#exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(language != "United States") %>%
  droplevels()
length(unique(d$workerid)) #299 (1 Turker excluded)

# exclude non-American English speakers
length(unique(d$workerid))# 200
length(which(is.na(d$american))) #52 (one person didn't respond)
table(d$american) 

d <- d %>%
   filter(american == "Yes") %>%
   droplevels()
length(unique(d$workerid)) #297

#data from 3 Turkers excluded for language reasons

# exclude Turkers based on main clause controls in projection block
# same exclusion criterion as in XPRAG 2019 projection experiment
table(d$short_trigger,d$question_type)

# main clause controls in projection block
names(d)
table(d$question_type)

d.MC.Proj <- d %>%
  filter(short_trigger == "MC" & question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #1782 / 297 Turkers = 6 controls

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #0.24

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
p.means

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# Turkers with mean response more than 2 standard deviations above group mean
p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p #11 Turkers

# make data subset of just the outliers
outliers <- p.means %>%
  filter(workerid %in% p$workerid)
outliers = droplevels(outliers)
nrow(outliers) #11 outlier Turkers

# look at outlier responses
ggplot(outliers, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 286 remaining Turkers (11 Turkers excluded)

# remove data from Turkers with low variance (as in factives paper)

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance

variances = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))
variances

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 0 turkers consistently clicked on roughly the same point on the scale

# lvw = d %>%
#   filter(as.character(workerid) %in% lowvarworkers) %>%
#   droplevels() %>%
#   mutate(Participant = as.factor(as.character(workerid)))
# 
# ggplot(lvw,aes(x=Participant,y=response)) +
#   geom_point()
# 
# # no turkers excluded based on variance
# d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #286 Turkers remain

# age and gender info of remaining Turkers
length(which(is.na(d$age))) #0
table(d$age) #18-82
median(d$age,na.rm=TRUE) #35.5
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#116 female, 168 male, 1 other, 1 undeclared

# clean non-spread data
cd = d
head(cd)
nrow(cd) #14872

# spread the data

# rename prior column into prior_type
colnames(cd)[colnames(cd)=="prior"] = "prior_type"
# fill in main_clause for the prior_type of main clauses
cd[cd$short_trigger == "MC",]$prior_type <- "main_clause"

summary(cd)
table(cd$prior_type)
#high_prior   low_prior main_clause 
#5720        5720        3432

table(cd$question_type)
#prior projective 
#7436       7436 

# spread responses over separate columns for prior probability, projectivity and at-issueness
cd = cd %>%
  mutate(block_proj = ifelse(question_type=="projective"&block=="block1", "block1",
                             ifelse(question_type=="projective"&block=="block2", "block2",
                                    ifelse(question_type=="prior"&block=="block1", "block2", "block1")))) %>%
  select(content,question_type,short_trigger,response,workerid,prior_type,prior_fact,block_proj) %>% 
  spread(question_type,response)

summary(cd)
nrow(cd) #7436

# create eventItem
cd = cd %>%
  mutate(eventItem = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "Charley speaks Spanish",
                              ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "Danny ate the last cupcake",
                                     ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "Emily bought a car yesterday",
                                            ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "Emma studied on Saturday morning",
                                                   ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "Frank got a cat",
                                                          ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "Grace visited her sister",
                                                                 ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "Isabella ate a steak on Sunday",
                                                                        ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "Jackson ran 10 miles",
                                                                               ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "Jayden rented a car",
                                                                                      ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "Jon walks to work",
                                                                                             ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "Josh learned to ride a bike yesterday",
                                                                                                    ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "Josie went on vacation to France",
                                                                                                           ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "Julian dances salsa",
                                                                                                                  ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "Mary is pregnant",
                                                                                                                         ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "Mia drank 2 cocktails last night",
                                                                                                                                ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "Olivia sleeps until noon",
                                                                                                                                       ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "Owen shoveled snow last winter",
                                                                                                                                              ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "Sophia got a tattoo",
                                                                                                                                                     ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "Tony had a drink last night",
                                                                                                                                                            ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "Zoe calculated the tip",
                                                                                                                                                                   ifelse(prior_fact == "Muffins are sold at the bakery", "MC",
                                                                                                                                                                          ifelse(prior_fact == "Pizza is sold at the pizzeria", "MC",
                                                                                                                                                                                 ifelse(prior_fact == "Many children like ice cream", "MC",
                                                                                                                                                                                        ifelse(prior_fact == "Ballet is a type of dance", "MC",
                                                                                                                                                                                               ifelse(prior_fact == "Garages are used to store cars and other things", "MC",
                                                                                                                                                                                                      ifelse(prior_fact == "Hats are worn on the head", "MC",
                                                                                                                                                                                                             NA)))))))))))))))))))))))))))
table(cd$eventItem)

# write clean spread data to file
write.csv(cd, file = "../data/cd.csv")

