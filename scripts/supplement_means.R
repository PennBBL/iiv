# Supplement means notes
library(ggpubr)
library(gt)
library(data.table)
library(gridExtra)

# set functions
setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/input")
source("./scripts/summary_se_function.R")
source("./scripts/other_functions.R")

# set demos
demos <- read.csv("./data/old_demos.csv")
demos$Group <- as.factor(demos$Group)
demos$DX_graph <- as.factor(demos$DX_graph)
demos$Group_2 <- as.factor(demos$Group_2)

# save here
setwd("./iiv_scores")

# make data frames
reaction <- makeDF('reaction')
choice_reaction <- makeDF('choice_reaction')
simple_processing <- makeDF('simple_processing_speed')
attention <- makeDF('cpt')
complex_processing <- makeDF('complex_processing_speed')
abstract_matching <- makeDF('complex_reasoning')

#########
# set young demos
demos <- read.csv("../data/old_demos.csv")

demos_2 <- read.csv("../data/healthy_demos.csv")
CCI <- read.csv("../data/healthy_CCI.csv")
demos_2 <- merge(demos_2, CCI, by = "Subject", all = T)
colnames(demos_2) <- c("Subject", "age", "group", "Race", "Gender", "CCI")

# add to old demos
demos <- merge(demos, demos_2, all = TRUE)
demos$Group_2[demos$group == 1] <- 'HYA'
demos <- subset(demos, Group_2 == 'HYA' | Group_2 == 'HOA', 
                select = c(Subject, CCI, age, Race, Gender, Group_2, group))

# Create strings of the tasks in order to use them for the following function, makeDF
reaction_2 <- makeDF('reaction')
choice_reaction_2 <- makeDF('choice_reaction')
simple_processing_2 <- makeDF('simple_processing_speed')
attention_2 <- makeDF('cpt')
complex_processing_2 <- makeDF('complex_processing_speed')
abstract_matching_2 <- makeDF('complex_reasoning')

all_reaction <- merge(reaction, reaction_2, all = TRUE)
all_choice_reaction <- merge(choice_reaction, choice_reaction_2, all = TRUE)
all_simple_processing <- merge(simple_processing, simple_processing_2, all = TRUE)
all_attention <- merge(attention, attention_2, all = TRUE)
all_complex_processing <- merge(complex_processing, complex_processing_2, all = TRUE)
all_abstract_matching <- merge(abstract_matching, abstract_matching_2, all = TRUE)

# PC
pc_sum_choice_reaction <- summarySE(data = all_choice_reaction, 
                                    measurevar = "percent_correct", 
                                    groupvars = 'Group_2', na.rm = T)
pc_sum_choice_reaction$task <- 'Choice Reaction'

pc_sum_simple_processing <- summarySE(data = all_simple_processing, 
                                    measurevar = "percent_correct", 
                                    groupvars = 'Group_2', na.rm=T)
pc_sum_simple_processing$task <- 'Simple Processing'

pc_sum_attention <- summarySE(data = all_attention, 
                              measurevar = "percent_correct", 
                              groupvars = 'Group_2', na.rm = T)
pc_sum_attention$task <- 'Attention'

pc_sum_complex_processing <- summarySE(data = all_complex_processing, 
                                       measurevar = "percent_correct", 
                                       groupvars = 'Group_2', na.rm = T)
pc_sum_complex_processing$task <- 'Complex Processing'

pc_sum_abstract_matching <- summarySE(data = all_abstract_matching, 
                                      measurevar = "percent_correct", 
                                      groupvars = 'Group_2', na.rm = T)
pc_sum_abstract_matching$task <- 'Abstract Matching'

# RT
rt_sum_reaction <- summarySE(data = all_reaction, 
                             measurevar = "median_rt", 
                             groupvars = 'Group_2', na.rm = T)
rt_sum_reaction$task <- 'Motor Processing'

rt_sum_choice_reaction <- summarySE(data = all_choice_reaction, 
                                    measurevar = "rtcr", 
                                    groupvars = 'Group_2', na.rm = T)
rt_sum_choice_reaction$task <- 'Choice Reaction'

rt_sum_simple_processing <- summarySE(data = all_simple_processing, 
                                      measurevar = "rtcr", 
                                      groupvars = 'Group_2', na.rm = T)
rt_sum_simple_processing$task <- 'Simple Processing'

rt_sum_attention <- summarySE(data = all_attention, 
                              measurevar = "rtcr", 
                              groupvars = 'Group_2', na.rm = T)
rt_sum_attention$task <- 'Attention'

rt_sum_complex_processing <- summarySE(data = all_complex_processing, 
                                       measurevar = "rtcr", 
                                       groupvars = 'Group_2', na.rm = T)
rt_sum_complex_processing$task <- 'Complex Processing'

rt_sum_abstract_matching <- summarySE(data = all_abstract_matching, 
                                      measurevar = "rtcr", 
                                      groupvars = 'Group_2', na.rm = T)
rt_sum_abstract_matching$task <- 'Abstract Matching'

# IIV
iiv_sum_reaction <- summarySE(data = all_reaction, 
                              measurevar = "IIV", 
                              groupvars = 'Group_2', na.rm = T)
iiv_sum_reaction$task <- 'Motor Processing'

iiv_sum_choice_reaction <- summarySE(data = all_choice_reaction, 
                                     measurevar = "IIV", 
                                     groupvars = 'Group_2', na.rm = T)
iiv_sum_choice_reaction$task <- 'Choice Reaction'

iiv_sum_simple_processing <- summarySE(data = all_simple_processing, 
                                       measurevar = "IIV", 
                                       groupvars = 'Group_2', na.rm = T)
iiv_sum_simple_processing$task <- 'Simple Processing'

iiv_sum_attention <- summarySE(data = all_attention, 
                               measurevar = "IIV", 
                               groupvars = 'Group_2', na.rm = T)
iiv_sum_attention$task <- 'Attention'

iiv_sum_complex_processing <- summarySE(data = all_complex_processing, 
                                        measurevar = "IIV", 
                                        groupvars = 'Group_2', na.rm = T)
iiv_sum_complex_processing$task <- 'Complex Processing'

iiv_sum_abstract_matching <- summarySE(data = all_abstract_matching, 
                                       measurevar = "IIV", 
                                       groupvars = 'Group_2', na.rm = T)
iiv_sum_abstract_matching$task <- 'Abstract Matching'


library(knitr)
library(kableExtra)
library(dplyr)

pc <- rbind(pc_sum_attention, pc_sum_choice_reaction, pc_sum_simple_processing,
            pc_sum_complex_processing, pc_sum_abstract_matching)
pc <- na.omit(pc)
colnames(pc) <- c("Group", "N", "Mean", "SD", "SE", "CI", "Task")
rownames(pc) <- NULL

row <- seq(3, 15, 3)
pc <- pc %>%
  kable(digits = 3,caption = "Percent Correct Summary") %>%
  row_spec (row, extra_css = "border-bottom: 1px solid") %>%
  # column_spec(1:8, border_left = F, border_right = F) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# save here
save_kable(pc,'../../output/pc_supp_means.pdf')

#change median_rt to rtcr just to do rbind
colnames(rt_sum_reaction) <- c('Group_2', 'N', 'rtcr', 'sd', 'se', 'ci', 'task')
rt <- rbind(rt_sum_reaction, rt_sum_attention, rt_sum_choice_reaction,
            rt_sum_simple_processing, rt_sum_complex_processing,
            rt_sum_abstract_matching)
rt <- na.omit(rt)
colnames(rt) <- c("Group", "N", "Mean", "SD", "SE", "CI", "Task")
rownames(rt) <- NULL

row <- seq(3, 18, 3)
rt <- rt %>%
  kable(digits = 3, caption = "Reaction Time Summary") %>%
  row_spec(row, extra_css = "border-bottom: 1px solid") %>%
  # column_spec(1:8, border_left = F, border_right = F) %>%
  kable_classic(full_width = F, html_font = "Cambria")

save_kable(rt,'../../output/rt_supp_means.pdf')

iiv <- rbind(iiv_sum_reaction, iiv_sum_attention, iiv_sum_choice_reaction,
             iiv_sum_simple_processing, iiv_sum_complex_processing,
             iiv_sum_abstract_matching)
iiv <- na.omit(iiv)
colnames(iiv) <- c("Group", "N", "Mean", "SD", "SE", "CI", "Task")
rownames(iiv) <- NULL

row <- seq(3, 18, 3)
iiv <- iiv %>%
  kable(digits = 3, caption = "Intra-Individual Variability Summary") %>%
  row_spec(row, extra_css = "border-bottom: 1px solid") %>%
  # column_spec(1:8, border_left = F, border_right = F) %>%
  kable_classic(full_width = F, html_font = "Cambria")

save_kable(iiv,'../../output/iiv_supp_means.pdf')


