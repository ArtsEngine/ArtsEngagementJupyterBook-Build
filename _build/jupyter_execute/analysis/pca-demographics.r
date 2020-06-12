# Analysis of Principal Components x Demographic Factors

In this notebook, principal components are generated from question topic prevalences and disctionary-based estimates of linguistic patterns — and their relationships to various demographic data are explored through statistical tests.

We used a Principle Components Analysis (PCA) in R ('prcomp' function) to integrate multiple variables and conduct an analysis for each question and set of responses. PCA is a multivariate statistical technique that extracts important information about the inter-correlations of multiple variables and re-represents that information as new variables called principle components. These new principle components can then be used to show the similarity and graphic relationships between variables as a way of exploring the data. Because PCA condenses multiple variables into fewer dimensions, the new principle components may also be used to integrate new descriptions of the combined variables. These reified principle components may then be used as covariates to estimate relationships with other variables such as demographic factors.

Twenty-seven dictionary-based variables (LIWC, AIC, and Docuscope) were not relevant and were excluded from the analysis, leaving a total of 221 dictionary-based variables in the PCA. This was in addition to the measures of topic prevalence, which each had different numbers of topic variables depending on the question/response type. In order to remove variables with high numbers of 0s (no observations), we set a 0.3 maximum threshold, leaving variable columns with 70% or more observations in the analysis. The number of responses and variables included for each question in the analysis are provided in Table 1. Because of the different scales used by the different dictionaries and topic measures (for example, topic prevalence is 0-1 while AIC is measured on a scale from 1-7), the PCA was based on a correlation matrix. The data were also centered around zero and rotated.

# Load libraries and data

library(tidyverse)
library(broom)
library(FactoMineR)
library(magrittr)
library(emmeans)

load('../data/merged_artsengagement.rda')
load('../data/tidy_questions_best.Rda')
source('../scripts/select_helpers.R')
load('../data/pca_labels.rda')

# Subset demographic variables

motiv_demos <- df %>% select(contains('sr_motivation')) %>% names
identity_demos <- df %>% select(contains('sr_identity')) %>% names
participation_demos <- df %>% select(all_arts('participation')) %>% select(starts_with('sr')) %>% names
real_demos <- c('ethnic_group', 'sex', 'school', 'parented', 'income', 'artsincollege', 'hstype', 'hssize',
           'hslocation', 'hs_arts_freq', 'hs_encouragement', 'hs_required', 'hs_fees',
           'so_childhood1', 'so_childhood3', 'so_childhood5', 'sr_participated',
               'sr_highestdegreeplanned')
demo_groups <- c('real_demos','motiv_demos','identity_demos','participation_demos')

# Define helper functions

# Used within the mergeQuestion function to merge variable names
stripnames <- function(data, question) {
    data <- data %>% select('rownums', starts_with(question))
    varnames <- names(data)
    newnames <- varnames
    for (i in seq(3, length(varnames))) {
        newnames[i] <- substr(varnames[i], gregexpr("\\.\\.", varnames[i])[[1]][1]+2, nchar(varnames[i]))
    }
    names(data) <- newnames
    return(data)
}

# Remove irrelavant variables
stripVars <- function(data) {
    removenames <- names(data %>% select(matches("AIC..Words|AIC..IC_Differentiation|AIC..IC_Integration|AIC..DIAL_Differentiation|AIC..DIAL_Integration|AIC..ELAB_Differentiation|AIC..ELAB_Integration|docuscope..OralCues|docuscope..DialogCues|docuscope..XWordTokens|docuscope..XPunctuationTokens|docuscope..XTokens|LIWC..WC|LIWC..Sixltr|LIWC..Dic|LIWC..WPS|LIWC..swear|LIWC..netspeak|LIWC..assent|LIWC..nonflu|LIWC..filler|LIWC..AllPunc|LIWC..Period|LIWC..Comma|LIWC..Colon|LIWC..SemiC|LIWC..QMark|LIWC..Exclam|LIWC..Dash|LIWC..Quote|LIWC..Apostro|LIWC..Parenth|LIWC..OtherP")))
    data <- data[ , !names(data) %in% removenames]
    return(data)
}

# Merges responses between years
mergeQuestion <- function(data, question) {
    if (substr(question, 3,3) == '_') {
        question <- substr(question, 4, nchar(question))
    }
    # if you're aware of other prefixes, add them here
    yrs <- c(NA,'fl','f1','f2','so','jr','sr','sl')
    data$rownums <- seq(nrow(data)) # add rownums so resulting data can be re-integrated
    mergeddf <- data.frame()
    for (yr in yrs) {
        if (question == 'definition' && is.na(yr))
            qyr <- question
        else if (is.na(yr))
            next
        else {
            qyr <- paste0(yr, '_', question)
        }
        if (data %>% select(starts_with(qyr)) %>% ncol == 0) { next }
        tempdf <- stripnames(data, qyr)
        names(tempdf)[2] <- question
        mergeddf <- bind_rows(mergeddf, tempdf)        
    }
    mergeddf <- mergeddf[complete.cases(mergeddf),]
    mergeddf <- mergeddf %>% stripVars
    rownames(mergeddf) <- c()
    return(mergeddf)
}

# ANOVA, EMMeans Loop

sig_contrasts.demos <- c()
sig_aov_posthoc <- c()
dir.create('./results', showWarnings = F)
for(question in question_names[-c(4:6,9:14)]) {
    # directories
    dir.create(paste('./results', question, sep = '/'), showWarnings = F)
    
    # data subsetting
    data_subset <- mergeQuestion(df, question)
    data_subset_rows <- data_subset[[1]]
    data_subset <- data_subset[-1]
    data_subset <- data_subset[-1]
    percent <- 0.4
    data_subset <- data_subset[ lapply( data_subset, function(x) sum(x==0) / length(x) ) < percent ]
                                       
    # generate principal components
    pd <- prcomp(data_subset, retx= TRUE, center=TRUE, scale=TRUE)

    # merge with demographic cols
    pcs <- data.frame(rownum = data_subset_rows, pd$x[,seq(5)] %>% as.data.frame)
    rownums <- pcs$rownum %>% sort
    pcs <- pcs %>% arrange(rownum)
    pcs <- pcs[-1]
    demodfpcs <- bind_cols(df[rownums,c('key',real_demos)], pcs)

    # weighting by # of responses per respondent
    # ( each respondent has up to 4 responses per question, and each response is given a weight between 1/4 and 1 )
    resps_by_key <- demodfpcs$key %>% table %>% as.data.frame
    names(resps_by_key) <- c('key','n')
    resps_by_key %<>% filter(n!=0)
    for(r in seq(nrow(demodfpcs))) {
        nresps <- resps_by_key %>% filter(key == demodfpcs[r,]$key) %>% .$n
        demodfpcs[r, names(select(demodfpcs,matches('PC[12345]')))] <- (1/nresps) *
                                                            (demodfpcs[r, names(select(demodfpcs,matches('PC[12345]')))])
    }
    demodfpcs <- demodfpcs %>% select(-'key')
    demodf <- demodfpcs %>% select(-matches('PC[12345]'))
    pcs <- demodfpcs %>% select(matches('PC[12345]'))

    # analysis
    for(p in seq(pcs)) {
        # creating directories
        dir.create(paste('./results', question, names(pcs)[p], sep = '/'), showWarnings = F)
        dir.create(paste('./results', question, names(pcs)[p], 'emmeans', sep = '/'), showWarnings = F)
        dir.create(paste('./results', question, names(pcs)[p], 'anova_tukey', sep = '/'), showWarnings = F)
        for(d in seq(demodf)) {
            temp <- bind_cols(pcs[p], demodf[d])
            names(temp) <- c('pc','demo')
            if((temp$demo %>% table %>% as.data.frame %>% filter(Freq != 0) %>% nrow) == 1) next

            # emmeans contrasts
            lm.out <- lm(pc ~ demo, data=temp, na.action = na.exclude)
            em.out <- emmeans(lm.out, ~ demo, data=temp, weights = 'proportional')
            contrast.out <- contrast(em.out, method="del.eff") %>% tidy

            contrast.out$p.value %<>% p.adjust(method='holm')
            contrast.out %<>% mutate(question_name=question,
                                        PC_num = paste0('PC',p), 
                                        PC_pos = pca_labels %>% 
                                                  filter(question_name==question) %>% 
                                                  select(matches(paste0(p,'_pos'))) %>%
                                                  pull %>% as.character,
                                        PC_neg = pca_labels %>% 
                                                  filter(question_name==question) %>% 
                                                  select(matches(paste0(p,'_neg'))) %>%
                                                  pull %>% as.character,
                                        demographic = names(demodf)[d]
                                     )            

            # anova tukey
            aov.out <- aov(pc ~ demo, data=temp) %>% TukeyHSD %>% tidy
            aov.out %<>% select(-term) %>% mutate(question_name=question,
                                        PC_num = paste0('PC',p), 
                                        PC_pos = pca_labels %>% 
                                                  filter(question_name==question) %>% 
                                                  select(matches(paste0(p,'_pos'))) %>%
                                                  pull %>% as.character,
                                        PC_neg = pca_labels %>% 
                                                  filter(question_name==question) %>% 
                                                  select(matches(paste0(p,'_neg'))) %>%
                                                  pull %>% as.character,
                                        demographic = names(demodf)[d]
                                     )
            
            # bind rows
            options(warn=-1)
            sig_contrasts.demos %<>% bind_rows(contrast.out)
            sig_aov_posthoc %<>% bind_rows(aov.out)
            options(warn=0)
            
            # write csvs
            write.csv(contrast.out, paste('./results',question,names(pcs)[p],'emmeans',paste0(names(demodf)[d],'.csv'),sep='/'))
            write.csv(aov.out, paste('./results',question,names(pcs)[p],'anova_tukey',paste0(names(demodf)[d],'.csv'),sep='/'))
        }
    }
}
# # write rdas
save(sig_contrasts.demos, file='./results/emmeans_contrasts.rda')
save(sig_aov_posthoc, file='./results/anova_tukey.rda')

# MANOVA Loop

# skip aftercollege bc it causes errors (index 7)
for(question in (question_names[-c(4:6,9:14)])[-7]) {
    # directories
    dir.create(paste('./results', question, sep = '/'), showWarnings = F)
    
    # data subsetting, PC
    data_subset <- mergeQuestion(df, question)
    data_subset_rows <- data_subset[[1]]
    data_subset <- data_subset[-1]
    data_subset <- data_subset[-1]
    percent <- 0.4
    data_subset <- data_subset[ lapply( data_subset, function(x) sum(x==0) / length(x) ) < percent ]
    pd <- prcomp(data_subset, retx= TRUE, center=TRUE, scale=TRUE)

    # merge with demographic cols
    pcs <- data.frame(rownum = data_subset_rows, pd$x[,seq(5)] %>% as.data.frame)
    rownums <- pcs$rownum %>% sort
    pcs <- pcs %>% arrange(rownum)
    pcs <- pcs[-1]
    demodfpcs <- bind_cols(df[rownums,c('key',real_demos)], pcs)

    # weighting by # of responses per respondent
    resps_by_key <- demodfpcs$key %>% table %>% as.data.frame
    names(resps_by_key) <- c('key','n')
    resps_by_key %<>% filter(n!=0)
    for(r in seq(nrow(demodfpcs))) {
        nresps <- resps_by_key %>% filter(key == demodfpcs[r,]$key) %>% .$n
        demodfpcs[r, names(select(demodfpcs,matches('PC[12345]')))] <- (1/nresps) *
                                                            (demodfpcs[r, names(select(demodfpcs,matches('PC[12345]')))])
    }
    demodfpcs <- demodfpcs %>% select(-'key')
    demodf <- demodfpcs %>% select(-matches('PC[12345]'))
    pcs <- demodfpcs %>% select(matches('PC[12345]'))

    # manova
    demo_level_nums <- demodf %>% na.omit %>% sapply(function(x)(x %>% table %>% as.data.frame %>% filter(Freq != 0) %>% nrow)) %>% as.data.frame
    demo_level_nums$demo <- rownames(demo_level_nums)
    names(demo_level_nums) <- c('n','demo')
    filter_list <- demo_level_nums %>% filter(n < 2) %>% pull(demo)
    man_demodf <- demodf %>% select(-filter_list)
    print(nrow(man_demodf))
    man.out <- manova(pcs %>% as.matrix ~ .,
                    data=man_demodf)
                                                     
    # write csv
    write.csv(tidy(man.out), paste('./results',question,'manova_table.csv',sep='/'))
    sink(paste('./results',question,'manova_output.txt',sep='/'))
    print(man.out)
    sink()
}