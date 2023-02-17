# This function creates a new data frame from csv files in your working directory.
# x = a string of the task name
# All tasks need their percent correct calculated EXCEPT reaction (aka motor processing)
makeDF <- function(x) {
  x_IIV <- read.csv(paste(x, '_IIV.csv', sep = ""))
  x_IIV$Subject <- sub('(_).*$', '', x_IIV$Subject, perl = TRUE)
  x_scored <- read.csv(paste(x, '_scored_data.csv', sep = ""))
  x_scored$Subject <- sub('(_).*$','', x_scored$Subject, perl = TRUE)
  x <- merge(x_scored, x_IIV, by = "Subject", all = T)
  x <- merge(x, demos, by = "Subject", all.y = T)
  if (x[3, 2] != 'reaction') {
    x$percent_correct<-((x$total_correct) / (x$total_correct + x$total_incorrect))
  }
  print(x)
}

# This function creates tables of the summarySE function output (Group, N, Mean, SD, SE, and CI)
# and saves it to an output folder.
# x = dataframe
# group = 'grouping variable'
# name = 'name of task'
# sample = 'old' or 'young'
makeRawTable <- function(x, group, name, sample) {
  # the df reaction does not include percent_correct, so if looking at reaction, do this instead:
  if (x[3, 2] == 'reaction') {
    rt_sum <- summarySE(data = x, measurevar = "median_rt", groupvars = group, 
                        na.rm = T)
    iiv_sum <- summarySE(data = x, measurevar = "IIV", groupvars = group, 
                        na.rm = T)
    colnames(rt_sum) <- c("Group", "N", "Mean", "SD", "SE", "CI")
    colnames(iiv_sum) <- c("Group", "N", "Mean", "SD", "SE", "CI")
    sum_stats <- rbind(rt_sum, iiv_sum)
    sum_stats[, -1:-2] <- round(sum_stats[, -1:-2], 3)
    gt_table_sum <- gt(data = sum_stats)
    
    if (group != 'DX_graph') {
      gt_table_sum <-
        gt_table_sum %>%
        tab_row_group(
          label = "Reaction Time",
          rows = 1:2
        ) %>%
        tab_row_group(
          label = "IIV",
          rows = 3:4
        ) %>%
        tab_header(
          title = paste(name, ' Raw Data', sep = "")
        )
    } else {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          label = "Reaction Time",
          rows = 1:3
        ) %>%
        tab_row_group(
          label = "IIV",
          rows = 4:6
        ) %>%
        tab_header(
          title = paste(name, ' Raw Data', sep = "")
        )
    }
    
  } else {
    pc_sum <- summarySE(data = x, measurevar = "percent_correct", 
                        groupvars = group, na.rm = T)
    rt_sum <- summarySE(data = x, measurevar = "rtcr", 
                        groupvars = group, na.rm = T)
    iiv_sum <- summarySE(data = x, measurevar = "IIV", 
                        groupvars = group, na.rm = T)
    colnames(pc_sum) <- c("Group", "N", "Mean", "SD", "SE", "CI")
    colnames(rt_sum) <- c("Group", "N", "Mean", "SD", "SE", "CI")
    colnames(iiv_sum) <- c("Group", "N", "Mean", "SD", "SE", "CI")
    sum_stats <- rbind(pc_sum, rt_sum, iiv_sum)
    sum_stats[, -1:-2] <- round(sum_stats[, -1:-2], 3)
    gt_table_sum <- gt(data = sum_stats)
    if (group != 'DX_graph') {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          label = "Percent Correct",
          rows = 1:2
        ) %>%
        tab_row_group(
          label = "Reaction Time",
          rows = 3:4
        ) %>%
        tab_row_group(
          label = "IIV",
          rows = 5:6
        ) %>%
        tab_header(
          title = paste(name, ' Raw Data', sep = "")
        )
    } else {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          label = "Percent Correct",
          rows = 1:3
        ) %>%
        tab_row_group(
          label = "Reaction Time",
          rows = 4:6
        ) %>%
        tab_row_group(
          label = "IIV",
          rows = 7:9
        ) %>%
        tab_header(
          title = paste(name, ' Raw Data', sep = "")
        )
    }
    
  }
  print(gt_table_sum)
  if (sample == 'old') {
    gtsave(gt_table_sum, paste('../../output/hoa_mci/', x[3, 2],'_', group, 
                               '_rawdata.rtf', sep = ''))
  } else if (sample == 'young') {
    gtsave(gt_table_sum, paste('../../output/hoa_hya/', x[3, 2], '_', group, 
                               '_rawdata.rtf', sep = ''))
  }
  
}

# This function first runs Levene's Test. If it passes, it continues with
# an ANOVA and if it fails (i.e. p-value is significant), it uses the non-parametric
# Kruskal-Wallis Test instead. It saves the output to an output folder.
# x = dataframe
# group = 'grouping variable'
# name = 'name of task'
mainANOVA <- function(x, group, name) {
  # again the reaction df is slightly different than the others
  a <- data.frame()
  k <- data.frame()
  if (x[3, 2] == 'reaction') {
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1, 3] >= 0.05) {
        #do anova - 
        print(i)
        i_aov <- aov(x[[i]] ~ x[[group]] + x$age + x$Sex + x$Race)
        i_anova <- round(Anova(i_aov, type = "III"), 3)
        rownames(i_anova) <- c("Intercept", "Group", "Age", "Gender", 
                               "Race", "Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
        i_anova <- cbind.data.frame(i_anova, i)
        a <- rbind.data.frame(a, i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest <- kruskal.test(x[[i]] ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_mci/', x[3, 2], '_', group, 
                           '_kwtest.csv',sep = ''))
      }
    }
    
  } else {
    dv_type <- c('percent_correct', 'rtcr', 'IIV')
    for (i in dv_type) {
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1, 3] >= 0.05) {
        #do anova
        print(i)
        i_aov <- aov(x[[i]] ~ x[[group]] + x$age + x$Sex + x$Race)
        i_anova <- round(Anova(i_aov, type = "III"), 3)
        rownames(i_anova) <- c("Intercept", "Group", "Age", "Gender", 
                               "Race", "Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
        i_anova <- cbind.data.frame(i_anova, i)
        a <- rbind.data.frame(a, i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest <- kruskal.test(x[[i]] ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_mci/', x[3, 2], '_', group, 
                           '_kwtest.csv', sep = ''))
      }
    }
  }
  table <- a[, 1:5]
  gt_table_anova <- gt(data = table)
  for (j in seq(6, nrow(table), 6)) {
    gt_table_anova <-
      gt_table_anova %>%
      tab_row_group(
        label = a[j, 6],
        rows = (j - 5):j
      )
  }
  
  gt_table_anova <-
    gt_table_anova %>%
    tab_header(
      title = paste(name, ' ANOVA', sep = "")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova, paste('../../output/hoa_mci/', x[3,2], '_', group, 
                               '_anova.rtf', sep = ''))
  write.csv(gt_table_anova, paste('../../output/hoa_mci/', x[3,2], '_', group,
                                  '_anova.csv', sep = ''))
}

# Same as the mainANOVA function, but with the HYA and HOA samples.
# This will save the output to a separate folder (hoa_hya).
healthyANOVA <- function(x, group, name) {
  # again the reaction df is slightly different than the others
  a <- data.frame()
  k <- data.frame()
  if (x[3, 2] == 'reaction') {
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] >= 0.05) {
        #do anova - 
        print(i)
        i_aov <- aov(x[[i]] ~ x[[group]] + x$Gender + x$Race)
        i_anova <- round(Anova(i_aov, type = "III"), 3)
        rownames(i_anova) <- c("Intercept", "Group", "Gender",
                               "Race", "Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
        i_anova <- cbind.data.frame(i_anova, i)
        a <- rbind.data.frame(a, i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest <- kruskal.test(x[[i]] ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_hya/', x[3,2], '_', group,
                           '_healthykwtest.csv', sep = ''))
      }
    }
  } else {
    dv_type <- c('percent_correct', 'rtcr', 'IIV')
    for (i in dv_type) {
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] >= 0.05) {
        #do anova
        print(i)
        i_aov <- aov(x[[i]] ~ x[[group]] + x$Gender + x$Race)
        i_anova <- round(Anova(i_aov, type = "III"), 3)
        rownames(i_anova) <- c("Intercept", "Group", "Gender", 
                               "Race", "Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
        i_anova <- cbind.data.frame(i_anova, i)
        a <- rbind.data.frame(a, i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest <- kruskal.test(x[[i]] ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_hya/', x[3,2], '_', group, 
                           '_healthykwtest.csv', sep = ''))
      }
    }
  }
  table <- a[, 1:5]
  gt_table_anova <- gt(data = table)
  for (j in seq(5, nrow(table), 5)) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        label = a[j, 6],
        rows = (j - 4):j
      )
  }
  
  gt_table_anova <-
    gt_table_anova %>%
    tab_header(
      title = paste(name, ' ANOVA', sep = "")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova, paste('../../output/hoa_hya/', x[3, 2], '_', group,
                               '_healthyanova.rtf', sep = ''))
  write.csv(gt_table_anova, paste('../../output/hoa_hya/', x[3, 2], '_', group,
                                  '_healthyanova.csv', sep = ''))
  
}

# This functions runs multiple comparisons for HOA, MCI, and SCM.
# x = data
# a = group 1
# b = group 2
# c = optional group 3
multipleComparisons <- function(x, group) {
  hoa <- subset(x, DX_graph == 'HOA')
  mci <- subset(x, DX_graph == 'MCI')
  scm <- subset(x, DX_graph == 'SCM')
  if (x[3, 2] == 'reaction'){
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      print(i)
      compare <- pairwise.t.test(x[[i]], x[[group]], p.adj = "none")
      print(compare)
      cohens_d1 <- cohensD(x = hoa[[i]], y = mci[[i]])
      cohens_d2 <- cohensD(x = scm[[i]], y = mci[[i]])
      cohens_d3 <- cohensD(x = hoa[[i]], y = scm[[i]])
      cohen <- rbind(cohens_d1, cohens_d2, cohens_d3)
      colnames(cohen) <- c("Cohen's D")
      rownames(cohen) <- c("HOA v MCI:", "SCM v MCI:", "HOA v SCM:")
      print(cohen)
    }
  } else {
    dv_type<-c('percent_correct', 'rtcr', 'IIV')
    for (i in dv_type) {
      print(i)
      compare <- pairwise.t.test(x[[i]], x[[group]], p.adj = "none")
      print(compare)
      cohens_d1 <- cohensD(x = hoa[[i]], y = mci[[i]])
      cohens_d2 <- cohensD(x = scm[[i]], y = mci[[i]])
      cohens_d3 <- cohensD(x = hoa[[i]], y = scm[[i]])
      cohen <- rbind(cohens_d1, cohens_d2, cohens_d3)
      colnames(cohen) <- c("Cohen's D")
      rownames(cohen) <- c("HOA v MCI:", "SCM v MCI:", "HOA v SCM:")
      print(cohen)
    }
  }
}

# This function subsets either HOA v MCI or HOA v HYA and calculates
# Cohen's D for each variable (accuracy, reaction time, and IIV)
# in the task.
calc_effectsize <- function(x, sample) {
  cohens_d <- data.frame(Value = integer(), Group = character(),
                         Task = character(), Type = character())
  if (sample == 'old') {
    hoa_x <- subset(x, Group_2 == 'HOA')
    mci_x <- subset(x, Group_2 == 'MCI')
    
    if (x[3, 2] == 'reaction') {
      dv_type <- c('median_rt', 'IIV')
      
      for (i in dv_type) {
        cohens_d1 <- cohensD(x = hoa_x[[i]], y = mci_x[[i]])
        d1 <- c(cohens_d1, 'HOA v MCI', 'reaction', paste(i, '', sep = ''))
        cohens_d <- rbind(cohens_d, d1)
        colnames(cohens_d) <- c('Value', 'Group', 'Task', 'Type')
      }
    } else {
      dv_type <- c('percent_correct', 'rtcr', 'IIV')
      
      for (i in dv_type) {
        cohens_d1 <- cohensD(x = hoa_x[[i]], y = mci_x[[i]])
        d1 <- c(cohens_d1, 'HOA v MCI', paste(x[3, 2], '', sep = ''), paste(i, '', sep = ''))
        cohens_d <- rbind(cohens_d, d1)
        colnames(cohens_d) <- c('Value', 'Group', 'Task', 'Type')
      }
      
    }
    
  } else {
    hoa_x <- subset(x, Group_2 == 'HOA')
    hya_x <- subset(x, Group_2 == 'HYA')
    
    if (x[3, 2] == 'reaction') {
      dv_type <- c('median_rt', 'IIV')
      
      for (i in dv_type) {
        cohens_d1 <- cohensD(x = hoa_x[[i]], y = hya_x[[i]])
        d1 <- c(cohens_d1, 'HOA v HYA', 'reaction', paste(i, '', sep = ''))
        cohens_d <- rbind(cohens_d, d1)
        colnames(cohens_d) <- c('Value', 'Group', 'Task', 'Type')
      }
      
    } else {
      dv_type <- c('percent_correct', 'rtcr', 'IIV')
      
      for (i in dv_type) {
        cohens_d1 <- cohensD(x = hoa_x[[i]], y = hya_x[[i]])
        d1 <- c(cohens_d1, 'HOA v HYA', paste(x[3, 2], '', sep = ''), paste(i, '', sep = ''))
        cohens_d <- rbind(cohens_d, d1)
        colnames(cohens_d) <- c('Value', 'Group', 'Task', 'Type')
      }
      
    }
    
  }
  
  print(cohens_d)
}



