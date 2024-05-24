# This function creates a new data frame from csv files in your working directory.
# x = a string of the task name
# All tasks need their percent correct calculated EXCEPT reaction (aka motor processing)
makeDF <- function(x) {
  x_IIV<-read.csv( paste(x,'_IIV.csv',sep="") )
  x_IIV$Subject<-sub('(_).*$', '', x_IIV$Subject, perl=TRUE)
  x_scored<-read.csv(paste(x,'_scored_data.csv',sep=""))
  x_scored$Subject<-sub('(_).*$', '', x_scored$Subject, perl=TRUE)
  x<-merge(x_scored,x_IIV, by="Subject", all=T)
  x<-merge(x, demos, by="Subject", all.y=T)
  if (x[3,2] != 'reaction') {
    x$percent_correct<-((x$total_correct)/(x$total_correct+x$total_incorrect))
  }
  print(x)
}

# This function creates tables of the summarySE function output (Group, N, Mean, SD, SE, and CI)
# x = dataframe
# group = 'grouping variable'
# name = 'name of task'
makeRawTable <- function(x,group,name,sample) {
  # the df reaction does not include percent_correct, so if looking at reaction, do this instead:
  if (x[3,2] == 'reaction') {
    rt_sum<-summarySE(data=x, measurevar="median_rt", groupvars=group, na.rm=T)
    iiv_sum<-summarySE(data=x, measurevar="IIV", groupvars=group, na.rm=T)
    colnames(rt_sum)<-c("Group","N","Mean","SD","SE","CI")
    colnames(iiv_sum)<-c("Group","N","Mean","SD","SE","CI")
    sum_stats<-rbind(rt_sum,iiv_sum)
    sum_stats[,-1:-2]<-round(sum_stats[,-1:-2],3)
    gt_table_sum<-gt(data = sum_stats)
    if (group != 'DX_graph') {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 1:2
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 3:4
        )

      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )
    } else {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 1:3
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 4:6
        )

      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )

    }
    
  } else {
    pc_sum<-summarySE(data=x, measurevar="percent_correct", groupvars=group, na.rm=T)
    rt_sum<-summarySE(data=x, measurevar="rtcr", groupvars=group, na.rm=T)
    iiv_sum<-summarySE(data=x, measurevar="IIV", groupvars=group, na.rm=T)
    colnames(pc_sum)<-c("Group","N","Mean","SD","SE","CI")
    colnames(rt_sum)<-c("Group","N","Mean","SD","SE","CI")
    colnames(iiv_sum)<-c("Group","N","Mean","SD","SE","CI")
    sum_stats<-rbind(pc_sum,rt_sum,iiv_sum)
    sum_stats[,-1:-2]<-round(sum_stats[,-1:-2],3)
    gt_table_sum<-gt(data = sum_stats)
    if (group != 'DX_graph') {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Percent Correct",
          rows = 1:2
        ) %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 3:4
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 5:6
        )
      
      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )
      
    } else {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Percent Correct",
          rows = 1:3
        ) %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 4:6
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 7:9
        )
      
      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )
    }
    
  }
  print(gt_table_sum)
  if (sample == 'old') {
    gtsave(gt_table_sum,
           paste('/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/output/hoa_oam/',
                 x[3,2],'_',group,'_rawdata.rtf',sep=''))
  } else if (sample == 'young') {
    gtsave(gt_table_sum,
           paste('/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/output/hoa_hya/',
                 x[3,2],'_',group,'_rawdata.rtf',sep=''))
  }
  
}

# Use Levene's Test - if passes, continue with ANOVA; if fails (is significant), use
# Kruskal-Wallis Test
mainANOVA_accuracy <- function(x, group, name) {
  # again the reaction df is slightly different than the others
  a <- data.frame()
  b <- data.frame()
  k <- data.frame()
  if (x[3, 2] == 'reaction') {
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      #do anova
      print(i)
      i_aov <- aov(x[[i]] ~ x[[group]] + x$age + x$Sex + x$Race + x$Education)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Age", "Gender", 
                             "Race","Education","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('age','Sex','Race','Education')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        # write.csv(k, paste('../../output/hoa_oam/extra/w_accuracy/', x[3, 2], '_', group, 
        #                    '_kwtest.csv',sep = ''))
      }
      
    } 
  } else {
    dv_type <- c('rtcr', 'IIV')
    for (i in dv_type) {
      print(i)
      i_aov <- aov(x[[i]] ~ x[[group]] + x$age + x$Sex + x$Race + x$Education + x$percent_correct)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Age", "Gender", 
                             "Race", "Education","Accuracy","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('age','Sex','Race','Education','percent_correct')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_oam/extra/w_accuracy/', x[3, 2], '_', group, 
                           '_kwtest.csv',sep = ''))
        
        table_regress <- b[, 1:5]
        gt_table_reg <- gt(data = table_regress)
        for (j in seq(nrow(resids_anova), nrow(table_regress), nrow(resids_anova))) {
          gt_table_reg<-
            gt_table_reg %>%
            tab_row_group(
              label = b[j, 6],
              rows = (j - (nrow(resids_anova) - 1)):j
            )
          
          gt_table_reg <-
            gt_table_reg %>%
            tab_header(
              title = paste(name, ' ANOVA', sep = "")
            )
          
          print(gt_table_reg)
          gtsave(gt_table_reg, paste('../../output/hoa_oam/extra/w_accuracy/', x[3,2], '_', group, 
                                     '_anova.rtf', sep = ''))
          write.csv(gt_table_reg, paste('../../output/hoa_oam/extra/w_accuracy/', x[3,2], '_', group,
                                        '_anova.csv', sep = ''))
        }
      }
    }
  }
  
  table <- a[, 1:5]
  gt_table_anova <- gt(data = table)
  for (j in seq(nrow(i_anova), nrow(table), nrow(i_anova))) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        label = a[j, 6],
        rows = (j - (nrow(i_anova) - 1)):j
      )
  }
  
  gt_table_anova <-
    gt_table_anova %>%
    tab_header(
      title = paste(name, ' ANCOVA', sep = "")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova, paste('../../output/hoa_oam/extra/w_accuracy/', x[3,2], '_', group, 
                               '_ancova.rtf', sep = ''))
  write.csv(gt_table_anova, paste('../../output/hoa_oam/extra/w_accuracy/', x[3,2], '_', group,
                                  '_ancova.csv', sep = ''))
  
}

mainANOVA <- function(x, group, name) {
  # again the reaction df is slightly different than the others
  a <- data.frame()
  b <- data.frame()
  k <- data.frame()
  if (x[3, 2] == 'reaction') {
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      #do anova
      print(i)
      i_aov <- aov(x[[i]] ~ x[[group]] + x$age + x$Sex + x$Race + x$Education)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Age", "Gender", 
                             "Race","Education","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('age','Sex','Race','Education')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        # write.csv(k, paste('../../output/hoa_oam/extra/w_accuracy/', x[3, 2], '_', group, 
        #                    '_kwtest.csv',sep = ''))
      }
      
    } 
  } else {
    dv_type <- c('percent_correct','rtcr', 'IIV')
    for (i in dv_type) {
      print(i)
      i_aov <- aov(x[[i]] ~ x[[group]] + x$age + x$Sex + x$Race + x$Education)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Age", "Gender", 
                             "Race", "Education","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('age','Sex','Race','Education')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_oam/', x[3, 2], '_', group, 
                           '_kwtest.csv', sep = ''), row.names = FALSE)
        
        table_regress <- b[, 1:5]
        gt_table_reg <- gt(data = table_regress)
        for (j in seq(nrow(resids_anova), nrow(table_regress), nrow(resids_anova))) {
          gt_table_reg<-
            gt_table_reg %>%
            tab_row_group(
              label = b[j, 6],
              rows = (j - (nrow(resids_anova) - 1)):j
            )
          
          gt_table_reg <-
            gt_table_reg %>%
            tab_header(
              title = paste(name, ' ANOVA', sep = "")
            )
          
          print(gt_table_reg)
          gtsave(gt_table_reg, paste('../../output/hoa_oam/', x[3,2], '_', group, 
                                     '_anova.rtf', sep = ''), row.names = FALSE)
          write.csv(gt_table_reg, paste('../../output/hoa_oam/', x[3,2], '_', group,
                                        '_anova.csv', sep = ''), row.names = FALSE)
        }
      }
    }
  }
  
  table <- a[, 1:5]
  gt_table_anova <- gt(data = table)
  for (j in seq(nrow(i_anova), nrow(table), nrow(i_anova))) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        label = a[j, 6],
        rows = (j - (nrow(i_anova) - 1)):j
      )
  }
  
  gt_table_anova <-
    gt_table_anova %>%
    tab_header(
      title = paste(name, ' ANCOVA', sep = "")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova, paste('../../output/hoa_oam/', x[3,2], '_', group, 
                               '_ancova.rtf', sep = ''), row.names = FALSE)
  write.csv(gt_table_anova, paste('../../output/hoa_oam/', x[3,2], '_', group,
                                  '_ancova.csv', sep = ''), row.names = FALSE)
  
}

healthyANOVA_accuracy <- function(x, group, name) {
  # again the reaction df is slightly different than the others
  a <- data.frame()
  b <- data.frame()
  k <- data.frame()
  x[[group]] <- as.factor(x[[group]])
  if (x[3, 2] == 'reaction') {
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      #do anova - 
      print(i)
      
      i_aov <- aov(x[[i]] ~ x[[group]] + x$Gender + x$Race + x$Education)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Gender",
                             "Race", "Education","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('Gender','Race','Education')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        # write.csv(k, paste('../../output/hoa_hya/extra/', x[3, 2], '_', group, 
        #                    '_kwtest.csv',sep = ''))
      }
      
    }
  } else {
    dv_type <- c('rtcr', 'IIV')
    for (i in dv_type) {
      #do anova
      print(i)
      i_aov <- aov(x[[i]] ~ x[[group]] + x$Gender + x$Race + x$Education + x$percent_correct)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Gender", 
                             "Race", "Education","Accuracy","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('Gender','Race','Education','percent_correct')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_hya/extra/w_accuracy/', x[3, 2], '_', group, 
                           '_kwtest.csv',sep = ''))
        
        table_regressed <- b[, 1:5]
        gt_table_reg <- gt(data = table_regressed)
        for (j in seq(nrow(resids_anova), nrow(table_regressed), nrow(resids_anova))) {
          gt_table_reg<-
            gt_table_reg %>%
            tab_row_group(
              label = b[j, 6],
              rows = (j - (nrow(resids_anova) - 1)):j
            )
          
          gt_table_reg <-
            gt_table_reg %>%
            tab_header(
              title = paste(name, ' ANOVA', sep = "")
            )
          
          print(gt_table_reg)
          gtsave(gt_table_reg, paste('../../output/hoa_hya/extra/w_accuracy/', x[3,2], '_', group, 
                                     '_healthyanova.rtf', sep = ''))
          write.csv(gt_table_reg, paste('../../output/hoa_hya/extra/w_accuracy/', x[3,2], '_', group,
                                        '_healthyanova.csv', sep = ''))
        }
      }
      
    }
  }
  
  table <- a[, 1:5]
  gt_table_anova <- gt(data = table)
  for (j in seq(nrow(i_anova), nrow(table), nrow(i_anova))) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        label = a[j, 6],
        rows = (j - (nrow(i_anova) - 1)):j
      )
  }
  
  gt_table_anova <-
    gt_table_anova %>%
    tab_header(
      title = paste(name, ' ANOVA', sep = "")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova, paste('../../output/hoa_hya/extra/w_accuracy/', x[3, 2], '_', group,
                               '_healthyancova.rtf', sep = ''))
  write.csv(gt_table_anova, paste('../../output/hoa_hya/extra/w_accuracy/', x[3, 2], '_', group,
                                  '_healthyancova.csv', sep = ''))
  
}

healthyANOVA <- function(x, group, name) {
  # again the reaction df is slightly different than the others
  a <- data.frame()
  b <- data.frame()
  k <- data.frame()
  x[[group]] <- as.factor(x[[group]])
  if (x[3, 2] == 'reaction') {
    dv_type <- c('median_rt', 'IIV')
    for (i in dv_type) {
      #do anova - 
      print(i)
      
      i_aov <- aov(x[[i]] ~ x[[group]] + x$Gender + x$Race + x$Education)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Gender",
                             "Race", "Education","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('Gender','Race','Education')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        # write.csv(k, paste('../../output/hoa_hya/extra/', x[3, 2], '_', group, 
        #                    '_kwtest.csv',sep = ''))
      }
      
    }
  } else {
    dv_type <- c('percent_correct','rtcr', 'IIV')
    for (i in dv_type) {
      #do anova
      print(i)
      i_aov <- aov(x[[i]] ~ x[[group]] + x$Gender + x$Race + x$Education)
      i_anova <- round(Anova(i_aov, type = "III"), 3)
      rownames(i_anova) <- c("Intercept", "Group", "Gender", 
                             "Race", "Education","Residuals")
      setDT(i_anova, keep.rownames = TRUE)[]
      colnames(i_anova) <- c("Factor", "Sum Sq", "df", "F", "p")
      i_anova <- cbind.data.frame(i_anova, i)
      a <- rbind.data.frame(a, i_anova)
      print(a)
      
      levene_test <- leveneTest(x[[i]], x[[group]])
      print(levene_test)
      if (levene_test[1,3] < 0.05) {
        # idvar<-'Subject'
        covars<-c('Gender','Race','Education')
        
        form<-paste(paste(i),' ~ (', paste(covars, collapse=' + '), ')' )
        mm<-lm(as.formula(form), data=x, na.action=na.omit)
        resids<-resid(mm)
        
        # Create or update the 'resids' data frame
        # Check if 'resids' is missing any columns
        missing_columns <- setdiff(rownames(x), names(resids))
        
        # If 'resids' is missing columns, add columns with NA values
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            resids[[col]] <- NA
          }
        }
        
        # Now create the 'resids' column in the data frame
        # resids<-data.frame('res'=resids[match(rownames(reaction), names(resids))], 'ids'=reaction[,idvar])
        x$resids<-resids[match(rownames(x), names(resids))]
        
        resids_aov <- aov(x$resids ~ x$Group_2)
        resids_anova <- round(Anova(resids_aov, type = "III"), 3)
        rownames(resids_anova) <- c("Intercept", "Group","Residuals")
        setDT(resids_anova, keep.rownames = TRUE)[]
        resids_anova <- cbind.data.frame(resids_anova, i)
        colnames(resids_anova) <- c("Factor", "Sum Sq", "df", "F", "p","Variable")
        b <- rbind.data.frame(b, resids_anova)
        print(b)
        
        i_kwtest <- kruskal.test(x$resids ~ x[[group]])
        k <- rbind.data.frame(k, i_kwtest)
        print(k)
        write.csv(k, paste('../../output/hoa_hya/', x[3, 2], '_', group, 
                           '_kwtest.csv',sep = ''))
        
        table_regressed <- b[, 1:5]
        gt_table_reg <- gt(data = table_regressed)
        for (j in seq(nrow(resids_anova), nrow(table_regressed), nrow(resids_anova))) {
          gt_table_reg<-
            gt_table_reg %>%
            tab_row_group(
              label = b[j, 6],
              rows = (j - (nrow(resids_anova) - 1)):j
            )
          
          gt_table_reg <-
            gt_table_reg %>%
            tab_header(
              title = paste(name, ' ANOVA', sep = "")
            )
          
          print(gt_table_reg)
          gtsave(gt_table_reg, paste('../../output/hoa_hya/', x[3,2], '_', group, 
                                     '_healthyanova.rtf', sep = ''), row.names = FALSE)
          write.csv(gt_table_reg, paste('../../output/hoa_hya/', x[3,2], '_', group,
                                        '_healthyanova.csv', sep = ''), row.names = FALSE)
        }
      }
      
    }
  }
  
  table <- a[, 1:5]
  gt_table_anova <- gt(data = table)
  for (j in seq(nrow(i_anova), nrow(table), nrow(i_anova))) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        label = a[j, 6],
        rows = (j - (nrow(i_anova) - 1)):j
      )
  }
  
  gt_table_anova <-
    gt_table_anova %>%
    tab_header(
      title = paste(name, ' ANOVA', sep = "")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova, paste('../../output/hoa_hya/', x[3, 2], '_', group,
                               '_healthyancova.rtf', sep = ''), row.names = FALSE)
  write.csv(gt_table_anova, paste('../../output/hoa_hya/', x[3, 2], '_', group,
                                  '_healthyancova.csv', sep = ''), row.names = FALSE)
  
}

# x = data
# a = group 1
# b = group 2
# c = optional group 3
multipleComparisons<- function(x,group) {
  hoa<-subset(x, DX_graph == 'HOA')
  mci<-subset(x, DX_graph == 'MCI')
  scm<-subset(x, DX_graph == 'SCM')
  if (x[3,2] == 'reaction'){
    dv_type<-c('median_rt','IIV')
    for (i in dv_type) {
      print(i)
      compare<-pairwise.t.test(x[[i]], x[[group]], p.adj = "none")
      print(compare)
      cohens_d1<-cohensD(x=hoa[[i]],y=mci[[i]])
      cohens_d2<-cohensD(x=scm[[i]],y=mci[[i]])
      cohens_d3<-cohensD(x=hoa[[i]],y=scm[[i]])
      cohen<-rbind(cohens_d1,cohens_d2,cohens_d3)
      colnames(cohen)<-c("Cohen's D")
      rownames(cohen)<-c("HOA v MCI:","SCM v MCI:","HOA v SCM:")
      print(cohen)
    }
  } else {
    dv_type<-c('percent_correct','rtcr','IIV')
    for (i in dv_type) {
      print(i)
      compare<-pairwise.t.test(x[[i]], x[[group]], p.adj = "none")
      print(compare)
      cohens_d1<-cohensD(x=hoa[[i]],y=mci[[i]])
      cohens_d2<-cohensD(x=scm[[i]],y=mci[[i]])
      cohens_d3<-cohensD(x=hoa[[i]],y=scm[[i]])
      cohen<-rbind(cohens_d1,cohens_d2,cohens_d3)
      colnames(cohen)<-c("Cohen's D")
      rownames(cohen)<-c("HOA v MCI:","SCM v MCI:","HOA v SCM:")
      print(cohen)
    }
  }
}

# This function subsets either HOA v OAM or HOA v HYA and calculates
# Cohen's D for each variable (accuracy, reaction time, and IIV)
# in the task.
calc_effectsize <- function(x, sample) {
  cohens_d <- data.frame(Value = integer(), Group = character(),
                         Task = character(), Type = character())
  if (sample == 'old') {
    hoa_x <- subset(x, Group_2 == 'HOA')
    oam_x <- subset(x, Group_2 == 'OAM')
    
    if (x[3, 2] == 'reaction') {
      dv_type <- c('median_rt', 'IIV')
      
      for (i in dv_type) {
        cohens_d1 <- cohensD(x = hoa_x[[i]], y = oam_x[[i]])
        d1 <- c(cohens_d1, 'HOA v OAM', 'reaction', paste(i, '', sep = ''))
        cohens_d <- rbind(cohens_d, d1)
        colnames(cohens_d) <- c('Value', 'Group', 'Task', 'Type')
      }
    } else {
      dv_type <- c('percent_correct', 'rtcr', 'IIV')
      
      for (i in dv_type) {
        cohens_d1 <- cohensD(x = hoa_x[[i]], y = oam_x[[i]])
        d1 <- c(cohens_d1, 'HOA v OAM', paste(x[3, 2], '', sep = ''), paste(i, '', sep = ''))
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











