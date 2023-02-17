# This script was created by Heather in March 2021 and updated in September 2022
# to compile all data analyses for David's Variability project.

# Naming notes:
# HYA - healthy young adults
# HOA - healthy old adults
# MCI - mild cognitive impairment
# SCM - subjective memory complaints
# 'reaction' = Motor Processing
# 'cpt' = Attention
# 'complex_reasoning' = Abstract Matching

# Load the appropriate R packages
library(ggpubr)
library(gt)
library(data.table)
library(gridExtra)
# Need this for Levene's Test:
library(car)
# Need this for cohensD/etaSquared:
library(lsr)
library(corrplot)
# Need this for sidak correction:
library(mutoss)

############## ALL OLD ADULT DATA ##############

# Set your working directory for wherever the old adult data live.
setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/input")

# Set the following sources so you can use the functions below 
# (summarySE, makeDF, makeRawTable, mainANOVA, multipleComparisons).
source("./scripts/summary_se_function.R")
source("./scripts/other_functions.R")

# Load the demographics csv
demos <- read.csv("./data/old_demos.csv")

# Set all the grouping variables as factors for following analyses.
# Note:
# 'Group' = HOA vs affected older adults (AOA; includes MCI & SCM)
# 'DX_graph' = HOA vs MCI vs SCM 
# 'Group_2' = HOA (includes SCM) vs MCI
demos$Group <- as.factor(demos$Group)
demos$DX_graph <- as.factor(demos$DX_graph)
demos$Group_2 <- as.factor(demos$Group_2)

# Set new working directory to use the IIV scores from new calculation (IIV using
# all subjects). Note: IIV scores were calculated separately for 1) older subjects
# and 2) old and young subjects. In order to compare across samples, IIV scores
# were re-calculated to include all subjects in these analyses.
setwd("./iiv_scores")

# Make dataframes for each task, using the variables we just created.
# makeDF(x) where x is a string of the task 
# (string comes from what the csv file is named)
reaction <- makeDF('reaction')
choice_reaction <- makeDF('choice_reaction')
simple_processing <- makeDF('simple_processing_speed')
attention <- makeDF('cpt')
complex_processing <- makeDF('complex_processing_speed')
abstract_matching <- makeDF('complex_reasoning')

# Make raw data tables for each task and for each grouping ('Group', 'Dx_graph',
# and 'Group_2').
# makeRawTable(x,group,name) where x is the df, group is a string of the 
# grouping variable (either 'Group', 'DX_new', or 'Group_2), name is a 
# string of the task name for the title, and sample is old or young.
# The output is a raw data table that is saved as an rtf in corresponding output
# directory (hoa_mci or hoa_hya).

# For final analyses, we used 'Group_2'.
makeRawTable(reaction, 'Group_2', 'Motor Processing', 'old')
makeRawTable(choice_reaction, 'Group_2', 'Choice Reaction', 'old')
makeRawTable(simple_processing, 'Group_2', 'Simple Processing', 'old')
makeRawTable(attention, 'Group_2', 'Attention', 'old')
makeRawTable(complex_processing, 'Group_2', 'Complex Processing', 'old')
makeRawTable(abstract_matching, 'Group_2', 'Abstract Matching', 'old')

# Do ANOVA for each task and for each grouping.
# mainANOVA(x,group,name) - same input as makeRawTable.
# The output is an anova table that is saved as an rtf in your working directory.

# For final analyses, we used 'Group_2'.
mainANOVA(reaction, 'Group_2', 'Motor Processing')
mainANOVA(choice_reaction, 'Group_2', 'Choice Reaction')
mainANOVA(simple_processing, 'Group_2', 'Simple Processing')
mainANOVA(attention, 'Group_2', 'Attention')
mainANOVA(complex_processing, 'Group_2', 'Complex Processing')
mainANOVA(abstract_matching, 'Group_2', 'Abstract Matching')

# Follow up comparisons looking at HOA v MCI v SCM.
# For final analyses, this grouping wasn't used - SCM were included in HOA.
# multipleComparisons(reaction, 'DX_graph')
# multipleComparisons(choice_reaction, 'DX_graph')
# multipleComparisons(simple_processing, 'DX_graph')
# multipleComparisons(attention, 'DX_graph')
# multipleComparisons(complex_processing, 'DX_graph')
# multipleComparisons(abstract_matching, 'DX_graph')

# Effect Sizes: Get cohen's d values for all tasks. 
# call the function calc_effectsize from other_functions.R
reaction_es <- calc_effectsize(reaction, 'old')
attn_es <- calc_effectsize(attention, 'old')
cr_es <- calc_effectsize(choice_reaction, 'old')
sp_es <- calc_effectsize(simple_processing, 'old')
cp_es <- calc_effectsize(complex_processing, 'old')
am_es <- calc_effectsize(abstract_matching, 'old')

# Combine all of the task effect sizes into one df.
cohen_all <- rbind(reaction_es, attn_es, cr_es, sp_es, cp_es, am_es)
cohen_all$Value <- as.numeric(cohen_all$Value)

# Separate out the cohen_all df by variable (percent_correct, reaction time,
# and IIV). Add an average value across tasks. Save to output folder.
hoavmci_d <- subset(cohen_all, Group == 'HOA v MCI')
hoavmci_iiv <- subset(hoavmci_d, Type == 'IIV')
avg_hoavmci_iiv <- mean(hoavmci_iiv$Value)
hoavmci_iiv <- rbind(hoavmci_iiv, avg_hoavmci_iiv)
hoavmci_iiv$Group[7] <- 'HOA v MCI'
hoavmci_iiv$Task[7] <- 'AVERAGE'
hoavmci_iiv$Type[7] <- 'IIV'
write.csv(hoavmci_iiv, file = '../../output/hoa_mci/hoavmci_iiv_effectsize.csv', 
          row.names = FALSE)

hoavmci_rt <- subset(hoavmci_d, Type == 'median_rt' | Type == 'rtcr')
avg_hoavmci_rt <- mean(hoavmci_rt$Value)
hoavmci_rt <- rbind(hoavmci_rt, avg_hoavmci_rt)
hoavmci_rt$Group[7] <- 'HOA v MCI'
hoavmci_rt$Task[7] <- 'AVERAGE'
hoavmci_rt$Type[7] <- 'rtcr'
write.csv(hoavmci_rt, file = '../../output/hoa_mci/hoavmci_rt_effectsize.csv', 
          row.names = FALSE)

hoavmci_pc <- subset(hoavmci_d, Type == 'percent_correct')
avg_hoavmci_pc <- mean(hoavmci_pc$Value)
hoavmci_pc <- rbind(hoavmci_pc, avg_hoavmci_pc)
hoavmci_pc$Group[6] <- 'HOA v MCI'
hoavmci_pc$Task[6] <- 'AVERAGE'
hoavmci_pc$Type[6] <- 'percent_correct'
write.csv(hoavmci_pc, file = '../../output/hoa_mci/hoavmci_pc_effectsize.csv', 
          row.names = FALSE)

# Make bar plot for HOA (incl SCM) v MCI.
# Load in correct csv files.
setwd("../data")
reaction_time <- read.csv("rt_graph_2groups.csv")
iiv <- read.csv("iiv_graph_2groups.csv")
percent_correct <- read.csv("pc_graph_2groups.csv")

# Use FDR and Sidak correction for p-values.
# First, load in the output from the previous ANOVAs and Kruskal-Wallis tests.
reaction_p <- read.csv('../../output/hoa_mci/reaction_Group_2_anova.csv')
choice_reaction_p <- read.csv('../../output/hoa_mci/choice_reaction_Group_2_anova.csv')
choice_reaction_kw_p <- read.csv('../../output/hoa_mci/choice_reaction_Group_2_kwtest.csv')
simple_processing_p <- read.csv('../../output/hoa_mci/simple_processing_speed_Group_2_anova.csv')
simple_processing_kw_p <- read.csv('../../output/hoa_mci/simple_processing_speed_Group_2_kwtest.csv')
attention_p <- read.csv('../../output/hoa_mci/cpt_Group_2_anova.csv')
attention_kw_p <- read.csv('../../output/hoa_mci/cpt_Group_2_kwtest.csv')
complex_processing_p <- read.csv('../../output/hoa_mci/complex_processing_speed_Group_2_anova.csv')
complex_processing_kw_p <- read.csv('../../output/hoa_mci/complex_processing_speed_Group_2_kwtest.csv')
abstract_matching_p <- read.csv('../../output/hoa_mci/complex_reasoning_Group_2_anova.csv')

# Take the p-values from the ANOVAs/KW tests for reaction time.
rt_p_values <- c(reaction_p$p[8], attention_p$p[8],
               choice_reaction_kw_p$p.value[2], simple_processing_kw_p$p.value[1],
               complex_processing_p$p[2], abstract_matching_p$p[8])

# Take the p-values from the ANOVAs/KW tests for percent correct.
pc_p_values <- c(attention_kw_p$p.value[1], choice_reaction_kw_p$p.value[1],
               simple_processing_p$p[2], complex_processing_kw_p$p.value[1],
               abstract_matching_p$p[14])

# Take the p-values from the ANOVAs/KW tests for IIV.
iiv_p_values <- c(reaction_p$p[2], attention_p$p[2], choice_reaction_p$p[2],
                simple_processing_kw_p$p.value[2], complex_processing_kw_p$p.value[2],
                abstract_matching_p$p[2])

# Run FDR corrections for p-values
fdr_rt_p <- p.adjust(rt_p_values, method = "fdr")
fdr_pc_p <- p.adjust(pc_p_values, method = "fdr")
fdr_iiv_p <- p.adjust(iiv_p_values, method = "fdr")

# Run Sidak corrections for p-values
sidak_rt_p <- SidakSD(rt_p_values, .05)
sidak_pc_p <- SidakSD(pc_p_values, .05)
sidak_iiv_p <- SidakSD(iiv_p_values, .05)

# Save these bar plots as PDFs.
pdf("../../output/hoa_mci/rt_old_2_plot.pdf")
reaction_time_graph <- ggplot(reaction_time, aes(x=reorder(Task, rank), 
                                                 y=Value, fill=Group)) + 
  geom_bar(stat = "identity", position = "dodge", width=0.6) + 
  xlab("Cognitive Test") + ylab("Reaction Time") + 
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), position = "dodge", 
                stat = "identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("HOA", "MCI"), 
                    labels = c("Healthy Old Adults", "Mild Cognitive Impairment"), 
                    values = c("skyblue2", "palevioletred4")) + 
  geom_hline(yintercept =0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 3000)) +
  geom_signif(y_position = c(1000), xmin = c(2.8), xmax = c(3.2),
              annotation = c('*'), tip_length = 0, textsize = 6)
reaction_time_graph
dev.off()

pdf("../../output/hoa_mci/iiv_old_2_plot.pdf")
iiv_graph <- ggplot(iiv, aes(x = reorder(Task, rank), y = Value, fill = Group)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Intra-Individual Variability") + 
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), position = "dodge", 
                stat = "identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("HOA", "MCI"), 
                    labels = c("Healthy Old Adults", "Mild Cognitive Impairment"), 
                    values = c("skyblue2", "palevioletred4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 2)) +
  geom_signif(y_position = c(1.6), xmin = c(5.8), xmax = c(6.2), 
              annotation = c('*'), tip_length = 0, textsize = 6)
iiv_graph 
dev.off()

pdf("../../output/hoa_mci/pc_old_2_plot.pdf")
pc_graph <- ggplot(percent_correct, aes(x = reorder(Task, rank), y = Value, fill = Group)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Percent Correct") + 
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), position = "dodge", 
                stat = "identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("HOA", "MCI"), 
                    labels = c("Healthy Old Adults", "Mild Cognitive Impairment"), 
                    values = c("skyblue2", "palevioletred4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 1.5)) +
  geom_signif(y_position = c(1), xmin = c(0.8, 1.8, 3.8), xmax = c(1.2, 2.2, 4.2),
              annotation = c('*'), tip_length = 0, textsize = 6)
pc_graph 
dev.off()

# Correlations between scales for old subjects.
# Make scatter plots of those that are significant.
cor.test(demos$CCI, demos$MoCA.Total) #ns
cor.test(demos$CCI, demos$MMSE.Score)
cor.test(demos$CCI, demos$GDSTotal)
cor.test(demos$CCI, demos$CERADTotal)

pdf("../../output/hoa_mci/scales_cor_plot.pdf")
ggscatter(demos, x = "MoCA.Total", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "MoCA Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 13, label.y = 35)

ggscatter(demos, x = "MMSE.Score", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "MMSE Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 20, label.y = 35)

ggscatter(demos, x = "GDSTotal", y = "CCI", 
          add = "reg.line", conf.int = TRUE,
          xlab = "GDS Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 12, label.y = 35)

ggscatter(demos, x = "CERADTotal", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "CERAD Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 40, label.y = 75)
dev.off()

# Correlation plot of scales - just another way to visualize these analyses.
scales <- data.frame(demos$CCI, demos$MoCA.Total, demos$MMSE.Score, 
                     demos$GDSTotal, demos$CERADTotal)
M <- cor(scales, method = "pearson", use = "complete.obs")
corrplot(M, method = 'number', type = 'lower')

# Group by color plot:
pdf("../../output/hoa_mci/scales_cor_plot_bygroup.pdf")
ggscatter(demos, x = "MoCA.Total", y = "CCI", 
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "MoCA Score", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x = 13, cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 

ggscatter(demos, x = "MMSE.Score", y = "CCI",
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "MMSE Score", ylab = "CCI Score") +  
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x = 19, cor.coef.name =c ('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 

ggscatter(demos, x = "GDSTotal", y = "CCI",
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "GDS Score", ylab = "CCI Score") +  
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x = 9, cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 

ggscatter(demos, x = "CERADTotal", y = "CCI",
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "CERAD Score", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x = 39, cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 
dev.off()


# Then do correlations between CCI and task measures 
# using adjusted p-value of 0.008
cor.test(reaction$median_rt, reaction$CCI) #ns
cor.test(reaction$IIV, reaction$CCI) #ns

cor.test(choice_reaction$percent_correct, choice_reaction$CCI)
cor.test(choice_reaction$rtcr, choice_reaction$CCI) #ns
cor.test(choice_reaction$IIV, choice_reaction$CCI) #ns

cor.test(complex_processing$percent_correct, complex_processing$CCI)
cor.test(complex_processing$rtcr, complex_processing$CCI) #ns
cor.test(complex_processing$IIV, complex_processing$CCI) #ns

cor.test(abstract_matching$percent_correct, abstract_matching$CCI) #ns
cor.test(abstract_matching$rtcr, abstract_matching$CCI) #ns
cor.test(abstract_matching$IIV, abstract_matching$CCI)

cor.test(attention$percent_correct, attention$CCI) #ns
cor.test(attention$rtcr, attention$CCI) #ns
cor.test(attention$IIV, attention$CCI) #ns

cor.test(simple_processing$percent_correct, simple_processing$CCI) #ns
cor.test(simple_processing$rtcr, simple_processing$CCI) #ns
cor.test(simple_processing$IIV, simple_processing$CCI)

# make correlation plots of significant correlations.
pdf("../../output/hoa_mci/task_cor_plot.pdf")
ggscatter(choice_reaction, x = "percent_correct", y = "CCI", 
          add = "reg.line", conf.int = TRUE,
          xlab = "Choice Reaction Percent Correct", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c ('r'), label.x = 0.5, 
           label.y = 30)

ggscatter(complex_processing, x = "percent_correct", y = "CCI", 
          add = "reg.line", conf.int = TRUE,
          xlab = "Complex Processing Percent Correct", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 0.5, 
           label.y = 30)

ggscatter(abstract_matching, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Abstract Matching IIV", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 2, 
           label.y = 30)

ggscatter(simple_processing, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Simple Processing IIV", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 1.5, 
           label.y = 26)
dev.off()

# Do same, but group by color
pdf("../../output/hoa_mci/task_cor_plot_bygroup.pdf")
ggscatter(choice_reaction, x = "percent_correct", y = "CCI", 
          add = "reg.line", conf.int = TRUE, color = "Group_2",
          xlab = "Choice Reaction Percent Correct", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x.npc = 'left', label.y.npc = 'bottom', cor.coef.name = c('r'), 
           show.legend = FALSE)

ggscatter(complex_processing, x = "percent_correct", y = "CCI", 
          add = "reg.line", conf.int = TRUE,color = "Group_2",
          xlab = "Complex Processing Percent Correct", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x.npc = 'left', label.y.npc = 'bottom', cor.coef.name = c('r'), 
           show.legend = FALSE)

ggscatter(abstract_matching, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, color = "Group_2",
          xlab = "Abstract Matching IIV", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x.npc = 0.65, label.y.npc = 'bottom', cor.coef.name = c('r'), 
           show.legend = FALSE)

ggscatter(simple_processing, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, color = "Group_2",
          xlab = "Simple Processing IIV", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x.npc = 0.65, label.y.npc = 'bottom', cor.coef.name = c('r'), 
           show.legend = FALSE)
dev.off()

# Look at t-tests of variability by task
t.test(reaction$IIV, attention$IIV)
t.test(reaction$IIV, choice_reaction$IIV)
t.test(reaction$IIV, simple_processing$IIV)
t.test(reaction$IIV, complex_processing$IIV)
t.test(reaction$IIV, abstract_matching$IIV)

############## ALL HEALTHY DATA ##############

# Do the same as above old data, but with HOA v HYA.
demos <- read.csv("old_demos.csv")

demos_2 <- read.csv("healthy_demos.csv")
CCI <- read.csv("healthy_CCI.csv")
demos_2 <- merge(demos_2, CCI, by = "Subject", all = T)
colnames(demos_2) <- c("Subject", "age", "group", "Race", "Gender", "CCI")

demos <- merge(demos, demos_2, all = TRUE)
demos$Group_2[demos$group == 1] <- 'HYA'
demos <- subset(demos, Group_2 == 'HYA' | Group_2 == 'HOA', 
                select = c(Subject, CCI, age, Race, Gender, Group_2, group))

# Set new working directory to use the IIV scores from new calculation (IIV using
# all subjects). Note: IIV scores were calculated separately for 1) older subjects
# and 2) old and young subjects. In order to compare across samples, IIV scores
# were re-calculated to include all subjects in these analyses.
setwd("../iiv_scores")

# Create strings of the tasks in order to use them for the following function, makeDF.
reaction <- 'reaction'
choice_reaction <- 'choice_reaction'
simple_processing <- 'simple_processing_speed'
attention <- 'cpt'
complex_processing <- 'complex_processing_speed'
abstract_matching <- 'complex_reasoning'

# Make data frames for each task, using the variables we just created.
reaction <- makeDF(reaction)
choice_reaction <- makeDF(choice_reaction)
simple_processing <- makeDF(simple_processing)
attention <- makeDF(attention)
complex_processing <- makeDF(complex_processing)
abstract_matching <- makeDF(abstract_matching)

# Make raw data tables for each task with group being HOA v HYA.
makeRawTable(reaction, 'Group_2', 'Motor Processing', 'young')
makeRawTable(choice_reaction, 'Group_2', 'Choice Reaction', 'young')
makeRawTable(simple_processing, 'Group_2', 'Simple Processing', 'young')
makeRawTable(attention, 'Group_2', 'Attention', 'young')
makeRawTable(complex_processing, 'Group_2', 'Complex Processing', 'young')
makeRawTable(abstract_matching, 'Group_2', 'Abstract Matching', 'young')

# Do ANOVA for each task.
# Made a separate function for healthy sample because Age is not used as 
# a covariate for this group.
healthyANOVA(reaction, 'Group_2', 'Motor Processing')
healthyANOVA(choice_reaction, 'Group_2', 'Choice Reaction')
healthyANOVA(simple_processing, 'Group_2', 'Simple Processing')
healthyANOVA(attention, 'Group_2', 'Attention')
healthyANOVA(complex_processing, 'Group_2', 'Complex Processing')
healthyANOVA(abstract_matching, 'Group_2', 'Abstract Matching')

# Do effect sizes for healthy data
reaction_es <- calc_effectsize(reaction, 'young')
attn_es <- calc_effectsize(attention, 'young')
cr_es <- calc_effectsize(choice_reaction, 'young')
sp_es <- calc_effectsize(simple_processing, 'young')
cp_es <- calc_effectsize(complex_processing, 'young')
am_es <- calc_effectsize(abstract_matching, 'young')

cohen_all <- rbind(reaction_es, attn_es, cr_es, sp_es, cp_es, am_es)
cohen_all$Value <- as.numeric(cohen_all$Value)

hoavhya_rt <- subset(cohen_all, Type == 'median_rt' | Type == 'rtcr')
avg_hoavhya_rt <- mean(hoavhya_rt$Value)
hoavhya_rt <- rbind(hoavhya_rt, avg_hoavhya_rt)
hoavhya_rt$Group[7] <- 'HOA v HYA'
hoavhya_rt$Task[7] <- 'AVERAGE'
hoavhya_rt$Type[7] <- 'rtcr'
write.csv(hoavhya_rt, file = '../../output/hoa_hya/hoavhya_rt_effectsize.csv', 
          row.names = FALSE)

hoavhya_iiv <- subset(cohen_all, Type == 'IIV')
avg_hoavhya_iiv <- mean(hoavhya_iiv$Value)
hoavhya_iiv <- rbind(hoavhya_iiv, avg_hoavhya_iiv)
hoavhya_iiv$Group[7] <- 'HOA v HYA'
hoavhya_iiv$Task[7] <- 'AVERAGE'
hoavhya_iiv$Type[7] <- 'IIV'
write.csv(hoavhya_iiv, file = '../../output/hoa_hya/hoavhya_iiv_effectsize.csv', 
          row.names = FALSE)

hoavhya_pc <- subset(cohen_all, Type == 'percent_correct')
avg_hoavhya_pc <- mean(hoavhya_pc$Value)
hoavhya_pc <- rbind(hoavhya_pc, avg_hoavhya_pc)
hoavhya_pc$Group[6] <- 'HOA v HYA'
hoavhya_pc$Task[6] <- 'AVERAGE'
hoavhya_pc$Type[6] <- 'percent_correct'
write.csv(hoavhya_pc, file = '../../output/hoa_hya/hoavhya_pc_effectsize.csv', 
          row.names = FALSE)

# Make bar plot of healthy data
# Load the csv files.
setwd("../data")
reaction_time <- read.csv("reaction_time_graph_healthy.csv")
iiv <- read.csv("iiv_graph_healthy.csv")
percent_correct <- read.csv("percent_correct_graph_healthy.csv")

# Use FDR and Sidak correction for p-values
reaction_p <- read.csv('../../output/hoa_hya/reaction_Group_2_healthyanova.csv')
choice_reaction_p <- read.csv('../../output/hoa_hya/choice_reaction_Group_2_healthyanova.csv')
simple_processing_p <- read.csv('../../output/hoa_hya/simple_processing_speed_Group_2_healthyanova.csv')
attention_p <- read.csv('../../output/hoa_hya/cpt_Group_2_healthyanova.csv')
complex_processing_p <- read.csv('../../output/hoa_hya/complex_processing_speed_Group_2_healthyanova.csv')
complex_processing_kw_p <- read.csv('../../output/hoa_hya/complex_processing_speed_Group_2_healthykwtest.csv')
abstract_matching_p <- read.csv('../../output/hoa_hya/complex_reasoning_Group_2_healthyanova.csv')

rt_p_values <- c(reaction_p$p[7], attention_p$p[7], choice_reaction_p$p[7],
               simple_processing_p$p[7], complex_processing_p$p[7],
               abstract_matching_p$p[7])

pc_p_values <- c(attention_p$p[12], choice_reaction_p$p[12],
               simple_processing_p$p[12], complex_processing_kw_p$p.value[1],
               abstract_matching_p$p[12])

iiv_p_values <- c(reaction_p$p[2], attention_p$p[2], choice_reaction_p$p[2],
                simple_processing_p$p[2], complex_processing_p$p[2],
                abstract_matching_p$p[2])

fdr_rt_p <- p.adjust(rt_p_values, method = "fdr")
fdr_pc_p <- p.adjust(pc_p_values, method = "fdr")
fdr_iiv_p <- p.adjust(iiv_p_values, method = "fdr")

sidak_pc_p <- SidakSD(pc_p_values, .05)

pdf("../../output/hoa_hya/rt_barplot.pdf")
reaction_time_graph <- ggplot(reaction_time, aes(x = reorder(Name, rank),
                                                 y = Value, fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Reaction Time") + 
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), 
                position = "dodge", stat = "Identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("1HYA", "2HOA"), 
                    labels = c("Healthy Young Adults", "Healthy Old Adults"), 
                    values = c("lightseagreen", "skyblue2")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), 
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 3000)) +
  geom_signif(y_position = c(650, 750, 1250, 800, 2200), 
              xmin = c(1.8, 2.8, 3.8, 4.8, 5.8), 
              xmax = c(2.2, 3.2, 4.2, 5.2, 6.2),
              annotation = c('*'), tip_length = 0, textsize = 6)
reaction_time_graph
dev.off()

pdf("../../output/hoa_hya/iiv_barplot.pdf")
iiv_graph <- ggplot(iiv, aes(x = reorder(Name, rank), y = Value, fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Intra-Individual Variability") + 
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), 
                position = "dodge", stat = "Identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("1HYA", "2HOA"), 
                    labels = c("Healthy Young Adults", "Healthy Old Adults"), 
                    values = c("lightseagreen", "skyblue2")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 1.9)) +
  geom_signif(y_position = c(0.9, 1.25, 1.25, 1.25, 1.25),
              xmin = c(0.8, 2.8, 3.8, 4.8, 5.8),
              xmax = c(1.2, 3.2, 4.2, 5.2, 6.2),
              annotation = c('*'), tip_length = 0, textsize = 6)
iiv_graph
dev.off()

pdf("../../output/hoa_hya/pc_barplot.pdf")
percent_correct_graph <- ggplot(percent_correct, aes(x = reorder(Name, rank), 
                                                     y = Value, fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Percent Correct") + 
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), 
                position = "dodge", stat = "Identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("1HYA", "2HOA"), 
                    labels = c("Healthy Young Adults", "Healthy Old Adults"), 
                    values = c("lightseagreen", "skyblue2")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 1.5)) +
  geom_signif(y_position = c(1), xmin = c(3.8), xmax = c(4.2),
              annotation = c('*'), tip_length = 0, textsize = 6)
percent_correct_graph
dev.off()

# Correlations for healthy subjects
# CCI and task measures using adjusted p-value of 0.008
cor.test(choice_reaction$percent_correct, choice_reaction$CCI) #ns
cor.test(choice_reaction$rtcr, choice_reaction$CCI) #ns
cor.test(choice_reaction$IIV, choice_reaction$CCI) #ns

cor.test(complex_processing$percent_correct, complex_processing$CCI) #ns
cor.test(complex_processing$rtcr, complex_processing$CCI) #ns
cor.test(complex_processing$IIV, complex_processing$CCI) #ns

cor.test(abstract_matching$percent_correct, abstract_matching$CCI) #ns
cor.test(abstract_matching$rtcr, abstract_matching$CCI) #ns
cor.test(abstract_matching$IIV, abstract_matching$CCI) #ns

cor.test(attention$percent_correct, attention$CCI) #ns
cor.test(attention$rtcr, attention$CCI) #ns
cor.test(attention$IIV, attention$CCI) #ns

cor.test(reaction$median_rt, reaction$CCI) #ns
cor.test(reaction$IIV, reaction$CCI) #ns

cor.test(simple_processing$percent_correct, simple_processing$CCI) #ns
cor.test(simple_processing$rtcr, simple_processing$CCI)
cor.test(simple_processing$IIV, simple_processing$CCI)

pdf("../../output/hoa_hya/H_task_cor_plot.pdf")
ggscatter(simple_processing, x = "rtcr", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Simple Processing Reaction Time", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 500, label.y = 55)

plot <- ggscatter(simple_processing, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Simple Processing IIV", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 0.5, label.y = 55)

ggpar(plot,xlim=c(0.5, 1.75))
dev.off()

pdf("../../output/hoa_hya/H_task_cor_plot_bygroup.pdf")
ggscatter(simple_processing, x = "rtcr", y = "CCI", 
          add = "reg.line", conf.int = TRUE, color = "Group_2",
          xlab = "Simple Processing Reaction Time", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x = 500, cor.coef.name = c('r'), show.legend = FALSE)

plot <- ggscatter(simple_processing, x = "IIV", y = "CCI", 
                add = "reg.line", conf.int = TRUE, color = "Group_2",
                xlab = "Simple Processing IIV", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., 
                                              sep = "~`,`~")),
           label.x = 0.5, cor.coef.name = c('r'), show.legend = FALSE)

ggpar(plot,xlim=c(0.5, 1.75))
dev.off()

# Now separate out correlations by young and old
# Make old and young dataframes
old_choice_reaction <- subset(choice_reaction, Group_2 == 'HOA')
young_choice_reaction <- subset(choice_reaction, Group_2 == 'HYA')

old_complex_processing <- subset(complex_processing, Group_2 == 'HOA')
young_complex_processing <- subset(complex_processing, Group_2 == 'HYA')

old_abstract_matching <- subset(abstract_matching, Group_2 == 'HOA')
young_abstract_matching <- subset(abstract_matching, Group_2 == 'HYA')

old_attention <- subset(attention, Group_2 == 'HOA')
young_attention <- subset(attention, Group_2 == 'HYA')

old_reaction <- subset(reaction, Group_2 == 'HOA')
young_reaction <- subset(reaction, Group_2 == 'HYA')

old_simple_processing <- subset(simple_processing, Group_2 == 'HOA')
young_simple_processing <- subset(simple_processing, Group_2 == 'HYA')

# Old correlations - still using 0.008 threshold
cor.test(old_choice_reaction$percent_correct, old_choice_reaction$CCI) #ns
cor.test(old_choice_reaction$rtcr, old_choice_reaction$CCI) #ns
cor.test(old_choice_reaction$IIV, old_choice_reaction$CCI) #ns

cor.test(old_complex_processing$percent_correct, old_complex_processing$CCI) #ns
cor.test(old_complex_processing$rtcr, old_complex_processing$CCI) #ns
cor.test(old_complex_processing$IIV, old_complex_processing$CCI) #ns

cor.test(old_abstract_matching$percent_correct, old_abstract_matching$CCI) #ns
cor.test(old_abstract_matching$rtcr, old_abstract_matching$CCI) #ns
cor.test(old_abstract_matching$IIV, old_abstract_matching$CCI) #ns

cor.test(old_attention$percent_correct, old_attention$CCI) #ns
cor.test(old_attention$rtcr, old_attention$CCI) #ns
cor.test(old_attention$IIV, old_attention$CCI) #ns

cor.test(old_reaction$median_rt, old_reaction$CCI) #ns
cor.test(old_reaction$IIV, old_reaction$CCI) #ns

cor.test(old_simple_processing$percent_correct, old_simple_processing$CCI) #ns
cor.test(old_simple_processing$rtcr, old_simple_processing$CCI) #ns
cor.test(old_simple_processing$IIV, old_simple_processing$CCI)

pdf("../../output/hoa_hya/old_task_cor_plot.pdf")
ggscatter(old_simple_processing, x = "IIV", y = "CCI", 
                add = "reg.line", conf.int = TRUE, 
                xlab = "Simple Processing IIV: HOA", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 0.5, label.y = 65)
dev.off()

# Young correlations
cor.test(young_choice_reaction$percent_correct, young_choice_reaction$CCI) #ns
cor.test(young_choice_reaction$rtcr, young_choice_reaction$CCI) #ns
cor.test(young_choice_reaction$IIV, young_choice_reaction$CCI) 

cor.test(young_complex_processing$percent_correct, young_complex_processing$CCI) #ns
cor.test(young_complex_processing$rtcr, young_complex_processing$CCI) #ns
cor.test(young_complex_processing$IIV, young_complex_processing$CCI) #ns

cor.test(young_abstract_matching$percent_correct, young_abstract_matching$CCI) #ns
cor.test(young_abstract_matching$rtcr, young_abstract_matching$CCI) #ns
cor.test(young_abstract_matching$IIV, young_abstract_matching$CCI) #ns

cor.test(young_attention$percent_correct, young_attention$CCI) #ns
cor.test(young_attention$rtcr, young_attention$CCI) #ns
cor.test(young_attention$IIV, young_attention$CCI) #ns

cor.test(young_reaction$median_rt, young_reaction$CCI) #ns
cor.test(young_reaction$IIV, young_reaction$CCI) #ns

cor.test(young_simple_processing$percent_correct, young_simple_processing$CCI) #ns
cor.test(young_simple_processing$rtcr, young_simple_processing$CCI) #ns
cor.test(young_simple_processing$IIV, young_simple_processing$CCI) #ns

pdf("../../output/hoa_hya/HYA_task_cor_plot.pdf")
ggscatter(young_choice_reaction, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Choice Reaction IIV: HYA", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name = c('r'), label.x = 0.6, label.y = 65)
dev.off()

# Look at variability by task
t.test(reaction$IIV, attention$IIV)
t.test(reaction$IIV, choice_reaction$IIV)
t.test(reaction$IIV, simple_processing$IIV)
t.test(reaction$IIV, complex_processing$IIV)
t.test(reaction$IIV, abstract_matching$IIV)

## Make bar plots of effect sizes
hya_hoa_iiv <- read.csv("../../output/hoa_hya/hoavhya_iiv_effectsize.csv")
hya_hoa_rt <- read.csv("../../output/hoa_hya/hoavhya_rt_effectsize.csv")
hya_hoa_pc <- read.csv("../../output/hoa_hya/hoavhya_pc_effectsize.csv")

hoa_mci_iiv <- read.csv("../../output/hoa_mci/hoavmci_iiv_effectsize.csv")
hoa_mci_rt <- read.csv("../../output/hoa_mci/hoavmci_rt_effectsize.csv")
hoa_mci_pc <- read.csv("../../output/hoa_mci/hoavmci_pc_effectsize.csv")

# make bar plots by percent correct, reaction time, and IIV
hya_hoa_rt$Task <- c('Motor Processing', 'Attention', 'Choice Reaction',
                     'Simple Processing', 'Complex Processing', 'Abstract Matching',
                     'Average')
hya_hoa_rt$Rank <- c(1, 2, 3, 4, 5, 6, 7)

hoa_mci_rt$Task <- c('Motor Processing', 'Attention', 'Choice Reaction',
                     'Simple Processing', 'Complex Processing', 'Abstract Matching',
                     'Average')
hoa_mci_rt$Rank <- c(1, 2, 3, 4, 5, 6, 7)

rt_effectsize <- rbind(hya_hoa_rt, hoa_mci_rt)

pdf("../../output/effectsize_rt_barplot.pdf")
effectsize_rt <- ggplot(rt_effectsize, aes(x = reorder(Task, Rank), y = Value, 
                                           fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Reaction Time Effect Size") + 
  scale_fill_manual(name = "Group", breaks = c("HOA v HYA", "HOA v MCI"), 
                    labels = c("HOA v HYA", "HOA v MCI"), 
                    values = c("cyan3", "cyan4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.18, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 2))
effectsize_rt
dev.off()

hya_hoa_pc$Task <- c('Attention', 'Choice Reaction', 'Simple Processing',
                     'Complex Processing', 'Abstract Matching', 'Average')
hya_hoa_pc$Rank <- c(1, 2, 3, 4, 5, 6)

hoa_mci_pc$Task <- c('Attention', 'Choice Reaction', 'Simple Processing',
                     'Complex Processing', 'Abstract Matching', 'Average')
hoa_mci_pc$Rank <- c(1, 2, 3, 4, 5, 6)
pc_effectsize <- rbind(hya_hoa_pc, hoa_mci_pc)

pdf("../../output/effectsize_pc_barplot.pdf")
effectsize_pc <- ggplot(pc_effectsize, aes(x = reorder(Task, Rank), y = Value, 
                                           fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Percent Correct Effect Size") + 
  scale_fill_manual(name = "Group", breaks = c("HOA v HYA", "HOA v MCI"), 
                    labels = c("HOA v HYA", "HOA v MCI"), 
                    values = c("cyan3", "cyan4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.18, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 2))
effectsize_pc
dev.off()

hya_hoa_iiv$Task <- c('Motor Processing', 'Attention', 'Choice Reaction', 
                      'Simple Processing', 'Complex Processing', 
                      'Abstract Matching', 'Average')
hya_hoa_iiv$Rank <- c(1, 2, 3, 4, 5, 6, 7)

hoa_mci_iiv$Task <- c('Motor Processing', 'Attention', 'Choice Reaction', 
                      'Simple Processing', 'Complex Processing', 
                      'Abstract Matching', 'Average')
hoa_mci_iiv$Rank <- c(1, 2, 3, 4, 5, 6, 7)
iiv_effectsize <- rbind(hya_hoa_iiv, hoa_mci_iiv)

pdf("../../output/effectsize_iiv_barplot.pdf")
effectsize_iiv <- ggplot(iiv_effectsize, aes(x = reorder(Task, Rank), y = Value, 
                                             fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Intra-Individual Variability Effect Size") + 
  scale_fill_manual(name = "Group", breaks = c("HOA v HYA", "HOA v MCI"), 
                    labels = c("HOA v HYA", "HOA v MCI"), 
                    values = c("cyan3", "cyan4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), 
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.18, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 2))
effectsize_iiv
dev.off()

## add change in dx to demos ##
demos <- read.csv("old_demos.csv")
demos$group_change <- NA
for (i in 1:nrow(demos)) {
  iiv_group <- demos$Group_2[i]
  current_group <- print(demos[i, 20])
  if (iiv_group == 'HOA' && current_group == 'Normal') {
    demos[i, 23] <- 'HOA->HOA'
  }
  if (iiv_group == 'HOA' && grepl('MCI', current_group) == 'TRUE') {
    demos[i, 23] <- 'HOA->MCI'
  }
  if (iiv_group == 'HOA' && current_group == 'Subjective Cognitive Complaints') {
    demos[i, 23] <- 'HOA->HOA(SMC)'
  }
  if (iiv_group == 'MCI' && grepl('MCI', current_group) == 'TRUE') {
    demos[i, 23] <- 'MCI->MCI'
  }
  if (iiv_group == 'MCI' && current_group == 'Normal') {
    demos[i, 23] <- 'MCI->HOA'
  }
  if (iiv_group == 'MCI' && grepl('Dementia', current_group) == 'TRUE') {
    demos[i, 23] <- 'MCI->AD'
  }
  if (iiv_group == 'MCI' && grepl('AD', current_group) == 'TRUE') {
    demos[i, 23] <- 'MCI->AD'
  }
  if (iiv_group == 'MCI' && grepl('Other', current_group) == 'TRUE') {
    demos[i, 23] <- 'MCI->Other'
  }
}

demos[45,23]<-'MCI->Other'

tbl<- with(demos, table(group_change))

pdf('../../output/hoa_mci/current_dx.pdf')
ggplot(demos, aes(x=group_change)) +
  geom_bar()
dev.off()

## exploratory APOE analyses ##
demos$apoe_graph <- demos$APOE
demos$apoe_graph[demos$APOE == 'E2/E3'] <- 'Non-carrier'
demos$apoe_graph[demos$APOE == 'E3/E3'] <- 'Non-carrier'
demos$apoe_graph[demos$APOE == 'E3/E4'] <- 'Carrier'
demos$apoe_graph[demos$APOE == 'E4/E4'] <- 'Carrier'

demos$apoe_hoa <- NA
demos$apoe_mci <- NA
for (i in 1:nrow(demos)) {
  group <- demos$Group_2[i]
  apoe4 <- demos$apoe_graph[i]
  if (is.na(apoe4) == TRUE) {
    demos$apoe_hoa[i] <- NA
    demos$apoe_mci[i] <- NA
  }
  else if (group == 'HOA' && apoe4 == 'Non-carrier') {
    demos$apoe_hoa[i] <- 'Non-carrier'
  }
  else if (group == 'HOA' && apoe4 == 'Carrier') {
    demos$apoe_hoa[i] <- 'Carrier'
  }
  else if (group == 'MCI' && apoe4 == 'Carrier') {
    demos$apoe_mci[i] <- 'Carrier'
  }
  else if (group == 'MCI' && apoe4 == 'Non-carrier') {
    demos$apoe_mci[i] <- 'Non-carrier'
  }
}

## remove rows that don't have apoe status ##
demos <- demos[!is.na(demos$apoe_graph), ]

pdf('../../output/hoa_mci/scales_cor_apoe.pdf')
ggscatter(demos, x = "MoCA.Total", y = "CCI", 
          add = "reg.line", conf.int = FALSE, color = "apoe_graph",
          xlab = "MoCA Score", ylab = "CCI Score") +
  stat_cor(aes(color = apoe_graph, label = paste(..r.label.., ..p.label.., 
                                                 sep = "~`,`~")),
           label.x = 11, cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(name = 'apoe_graph')

ggscatter(demos, x = "MMSE.Score", y = "CCI", 
          add = "reg.line", conf.int = FALSE, color = "apoe_graph",
          xlab = "MMSE Score", ylab = "CCI Score") +
  stat_cor(aes(color = apoe_graph, label = paste(..r.label.., ..p.label.., 
                                                 sep = "~`,`~")),
           label.x = 17,cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(name = 'apoe_graph') 

ggscatter(demos, x = "GDSTotal", y = "CCI", 
          add = "reg.line", conf.int = FALSE, color = "apoe_graph",
          xlab = "GDS Score", ylab = "CCI Score") +
  stat_cor(aes(color = apoe_graph, label = paste(..r.label.., ..p.label.., 
                                                 sep = "~`,`~")),
           label.x = 11,cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(name = 'apoe_graph') 

ggscatter(demos, x = "CERADTotal", y = "CCI", 
          add = "reg.line", conf.int = FALSE, color = "apoe_graph",
          xlab = "CERAD Score", ylab = "CCI Score") +
  stat_cor(aes(color = apoe_graph, label = paste(..r.label.., ..p.label.., 
                                                 sep = "~`,`~")),
           label.x = 25,cor.coef.name = c('r'), show.legend = FALSE) +
  scale_color_discrete(name = 'apoe_graph')
dev.off()

apoe4 <- subset(demos, demos$apoe_graph == 'Carrier')
noapoe4 <- subset(demos, demos$apoe_graph == 'Non-carrier')

t.test(apoe4$CCI, noapoe4$CCI) #ns
t.test(apoe4$MoCA.Total, noapoe4$MoCA.Total) #ns
t.test(apoe4$MMSE.Score, noapoe4$MMSE.Score) #ns
t.test(apoe4$GDSTotal, noapoe4$GDSTotal) #ns

## do analyses from above w/ APOE status ##
setwd('../iiv_scores')
reaction <- 'reaction'
choice_reaction <- 'choice_reaction'
simple_processing <- 'simple_processing_speed'
attention <- 'cpt'
complex_processing <- 'complex_processing_speed'
abstract_matching <- 'complex_reasoning'

reaction <- makeDF(reaction)
choice_reaction <- makeDF(choice_reaction)
simple_processing <- makeDF(simple_processing)
attention <- makeDF(attention)
complex_processing <- makeDF(complex_processing)
abstract_matching <- makeDF(abstract_matching)

makeRawTable(reaction, 'apoe_graph', 'Motor Processing', 'old')
makeRawTable(choice_reaction, 'apoe_graph', 'Choice Reaction', 'old')
makeRawTable(simple_processing, 'apoe_graph', 'Simple Processing', 'old')
makeRawTable(attention, 'apoe_graph', 'Attention', 'old')
makeRawTable(complex_processing, 'apoe_graph', 'Complex Processing', 'old')
makeRawTable(abstract_matching, 'apoe_graph', 'Abstract Matching', 'old')

mainANOVA(reaction, 'apoe_graph', 'Motor Processing')
mainANOVA(choice_reaction, 'apoe_graph', 'Choice Reaction')
mainANOVA(simple_processing, 'apoe_graph', 'Simple Processing')
mainANOVA(attention, 'apoe_graph', 'Attention')
mainANOVA(complex_processing, 'apoe_graph', 'Complex Processing')
mainANOVA(abstract_matching, 'apoe_graph', 'Abstract Matching')

## make barplots for APOE status ##
reaction_time <- read.csv("../data/rt_graph_apoe_graph.csv")
iiv <- read.csv("../data/iiv_graph_apoe_graph.csv")
percent_correct <- read.csv("../data/pc_graph_apoe_graph.csv")

pdf("../../output/hoa_mci/rt_barplot_apoeall.pdf")
reaction_time_graph <- ggplot(reaction_time, aes(x = reorder(Task, rank), 
                                                 y = Mean, fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Reaction Time") + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), position = "dodge",
                stat = "Identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("Carrier", "Non-carrier"), 
                    labels = c("APOE4 Carrier", "APOE4 Non-carrier"), 
                    values = c("darkslategray2", "darkslategray4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 3000))
reaction_time_graph
dev.off()

pdf("../../output/hoa_mci/iiv_barplot_apoeall.pdf")
iiv_graph <- ggplot(iiv, aes(x = reorder(Task, rank), y = Mean, 
                             fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Intra-Individual Variability") + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), position = "dodge", 
                stat = "Identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("Carrier", "Non-carrier"), 
                    labels = c("APOE4 Carrier", "APOE4 Non-carrier"), 
                    values = c("darkslategray2", "darkslategray4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 1.9))
iiv_graph
dev.off()

pdf("../../output/hoa_mci/pc_barplot_apoeall.pdf")
percent_correct_graph <- ggplot(percent_correct, aes(x = reorder(Task, rank), 
                                                     y = Mean, fill = Group)) + 
  geom_bar(stat = "Identity", position = "dodge", width = 0.6) + 
  xlab("Cognitive Test") + ylab("Percent Correct") + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = "dodge", stat = "Identity", width = 0.6) + 
  scale_fill_manual(name = "Group", breaks = c("Carrier", "Non-carrier"), 
                    labels = c("APOE4 Carrier", "APOE4 Non-carrier"), 
                    values = c("darkslategray2", "darkslategray4")) + 
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),
                                    size = 15), 
        legend.position = c(0.25, 0.875), legend.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(NA, 1.5)) +
  geom_signif(y_position = c(1.2), xmin = c(4.8), xmax = c(5.2), 
              annotation = c('*'), tip_length = 0, textsize = 6)
percent_correct_graph
dev.off()

## do the same but with just HOA ##
makeRawTable(reaction, 'apoe_hoa', 'Motor Processing', 'old')
makeRawTable(choice_reaction, 'apoe_hoa', 'Choice Reaction', 'old')
makeRawTable(simple_processing, 'apoe_hoa', 'Simple Processing', 'old')
makeRawTable(attention, 'apoe_hoa', 'Attention', 'old')
makeRawTable(complex_processing, 'apoe_hoa', 'Complex Processing', 'old')
makeRawTable(abstract_matching, 'apoe_hoa', 'Abstract Matching', 'old')

mainANOVA(reaction, 'apoe_hoa', 'Motor Processing')
mainANOVA(choice_reaction, 'apoe_hoa', 'Choice Reaction')
mainANOVA(simple_processing, 'apoe_hoa', 'Simple Processing')
mainANOVA(attention, 'apoe_hoa', 'Attention')
mainANOVA(complex_processing, 'apoe_hoa', 'Complex Processing')
mainANOVA(abstract_matching, 'apoe_hoa', 'Abstract Matching')

reaction_time<-read.csv("../data/rt_graph_apoe_hoa.csv")
iiv<-read.csv("../data/iiv_graph_apoe_hoa.csv")
percent_correct<-read.csv("../data/pc_graph_apoe_hoa.csv")

pdf("../../output/hoa_mci/rt_barplot_apoe_hoa.pdf")
reaction_time_graph<-ggplot(reaction_time, aes(x= reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Reaction Time") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("Carrier", "Non-carrier"), labels=c("APOE4 Carrier (HOA)", "APOE4 Non-carrier (HOA)"), 
                    values=c("darkslategray2","darkslategray4")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 3000))
reaction_time_graph
dev.off()

pdf("../../output/hoa_mci/iiv_barplot_apoe_hoa.pdf")
iiv_graph<-ggplot(iiv, aes(x= reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Intra-Individual Variability") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("Carrier", "Non-carrier"), labels=c("APOE4 Carrier (HOA)", "APOE4 Non-carrier (HOA)"), 
                    values=c("darkslategray2","darkslategray4")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1,size=10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.9))
iiv_graph
dev.off()

pdf("../../output/hoa_mci/pc_barplot_apoe_hoa.pdf")
percent_correct_graph<-ggplot(percent_correct, aes(x= reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Percent Correct") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("Carrier", "Non-carrier"), labels=c("APOE4 Carrier (HOA)", "APOE4 Non-carrier (HOA)"), 
                    values=c("darkslategray2","darkslategray4")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=45, vjust=1,hjust=1,size = 10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.5)) +
  geom_signif(y_position=c(1.2),xmin=c(2.8),xmax=c(3.2),annotation=c('*'),tip_length=0,textsize=6)
percent_correct_graph
dev.off()

## do the same but with just MCI ##
makeRawTable(reaction, 'apoe_mci', 'Motor Processing', 'old')
makeRawTable(choice_reaction, 'apoe_mci', 'Choice Reaction', 'old')
makeRawTable(simple_processing, 'apoe_mci', 'Simple Processing', 'old')
makeRawTable(attention, 'apoe_mci', 'Attention', 'old')
makeRawTable(complex_processing, 'apoe_mci', 'Complex Processing', 'old')
makeRawTable(abstract_matching, 'apoe_mci', 'Abstract Matching', 'old')

mainANOVA(reaction, 'apoe_mci', 'Motor Processing')
mainANOVA(choice_reaction, 'apoe_mci', 'Choice Reaction')
mainANOVA(simple_processing, 'apoe_mci', 'Simple Processing')
mainANOVA(attention, 'apoe_mci', 'Attention')
mainANOVA(complex_processing, 'apoe_mci', 'Complex Processing')
mainANOVA(abstract_matching, 'apoe_mci', 'Abstract Matching')

reaction_time<-read.csv("../data/rt_graph_apoe_mci.csv")
iiv<-read.csv("../data/iiv_graph_apoe_mci.csv")
percent_correct<-read.csv("../data/pc_graph_apoe_mci.csv")

pdf("../../output/hoa_mci/rt_barplot_apoe_mci.pdf")
reaction_time_graph<-ggplot(reaction_time, aes(x= reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Reaction Time") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("Carrier", "Non-carrier"), labels=c("APOE4 Carrier (MCI)", "APOE4 Non-carrier (MCI)"), 
                    values=c("darkslategray2","darkslategray4")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 3000))
reaction_time_graph
dev.off()

pdf("../../output/hoa_mci/iiv_barplot_apoe_mci.pdf")
iiv_graph<-ggplot(iiv, aes(x= reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Intra-Individual Variability") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("Carrier", "Non-carrier"), labels=c("APOE4 Carrier (MCI)", "APOE4 Non-carrier (MCI)"), 
                    values=c("darkslategray2","darkslategray4")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1,size=10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.9))
iiv_graph
dev.off()

pdf("../../output/hoa_mci/pc_barplot_apoe_mci.pdf")
percent_correct_graph<-ggplot(percent_correct, aes(x= reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Percent Correct") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("Carrier", "Non-carrier"), labels=c("APOE4 Carrier (MCI)", "APOE4 Non-carrier (MCI)"), 
                    values=c("darkslategray2","darkslategray4")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=45, vjust=1,hjust=1,size = 10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.5)) +
  geom_signif(y_position=c(1.2),xmin=c(4.8),xmax=c(5.2),annotation=c('*'),tip_length=0,textsize=6)
percent_correct_graph
dev.off()
