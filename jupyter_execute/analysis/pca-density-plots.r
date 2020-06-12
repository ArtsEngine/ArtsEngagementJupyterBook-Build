# Plots for Principal Components x Demographic Factors

This notebook generates density plots from question topic principal components, by demographic.

## Load libraries, scripts, and data

library(tidyverse)
library(magrittr)
library(broom)
library(FactoMineR)
library(emmeans)

load('../data/merged_artsengagement.rda')
load('../data/tidy_questions_best.Rda')
source('../scripts/select_helpers.R')
load('../data/pca_labels.rda')

## Subset demographic variables

real_demos <- c('ethnic_group', 'sex', 'school', 'parented', 'income', 'artsincollege', 'hstype', 'hssize',
           'hslocation', 'hs_arts_freq', 'hs_encouragement', 'hs_required', 'hs_fees',
           'so_childhood1', 'so_childhood3', 'so_childhood5', 'sr_participated',
               'sr_highestdegreeplanned')

## Define helper functions

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

stripVars <- function(data) {
    #remove irrelavant variables
    removenames <- names(data %>% select(matches("AIC..Words|AIC..IC_Differentiation|AIC..IC_Integration|AIC..DIAL_Differentiation|AIC..DIAL_Integration|AIC..ELAB_Differentiation|AIC..ELAB_Integration|docuscope..OralCues|docuscope..DialogCues|docuscope..XWordTokens|docuscope..XPunctuationTokens|docuscope..XTokens|LIWC..WC|LIWC..Sixltr|LIWC..Dic|LIWC..WPS|LIWC..swear|LIWC..netspeak|LIWC..assent|LIWC..nonflu|LIWC..filler|LIWC..AllPunc|LIWC..Period|LIWC..Comma|LIWC..Colon|LIWC..SemiC|LIWC..QMark|LIWC..Exclam|LIWC..Dash|LIWC..Quote|LIWC..Apostro|LIWC..Parenth|LIWC..OtherP")))
    data <- data[ , !names(data) %in% removenames]
    return(data)
}

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

### Filter demographics to those within min & max levels (inclusive) into small_demos variable

min = 2
max = 5
temp <- demodfpcs %>% sapply(function(x)(x %>% levels %>% length)) %>% as.data.frame
temp$demo <- rownames(temp)
temp <- temp %>% filter(. >= min & max >= .)
small_demos <- temp %>% pull(demo)
small_demos

## Loop over all questions, generating PCs, munging their data, and finally generating graphs for them all
I recommend letting the code run for say 10 seconds then interrupt code execution to then print out / explore the variables being used within the ggplot code. Alternatively, break the code into chunks to see what each chunk does.

dir_to_save_results = './results'
plot_type = 'density_plots' # plural
output_format = '.pdf' # with dot

dir.create(dir_to_save_results, showWarnings = F)
# for all stm'd questions
for(question in question_names[-c(4:6,9:14)]) {
    dir.create(paste(dir_to_save_results, question, sep = '/'), showWarnings = F)
    
    # data subsetting, PC creation
    data_subset <- mergeQuestion(df, question)
    data_subset_rows <- data_subset[[1]]
    data_subset <- data_subset[-1]
    data_subset <- data_subset[-1]
    percent <- 0.4
    data_subset <- data_subset[ lapply( data_subset, function(x) sum(x==0) / length(x) ) < percent ]
                                       
    # principal component generation
    pd <- prcomp(data_subset, retx= TRUE, center=TRUE, scale=TRUE)

    # merge PCs with demographic cols
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
                                       
    # separate PCs and demographics
    demodfpcs <- demodfpcs %>% select(-'key')
    demodf <- demodfpcs %>% select(-matches('PC[12345]'))
    pcs <- demodfpcs %>% select(matches('PC[12345]'))
                                       
    # plotting (by PC)
    for(p in seq(pcs)) {
        # combine individual PC with demographics
        temp <- bind_cols(pcs[p], demodf)
        names(temp) <- c("pc", names(temp)[-1])
        
        # for each demographic...
        for(d in seq(small_demos)) {
            mu <- temp %>% na.omit %>% group_by_at(vars(small_demos[d])) %>%
                    summarise(grp.median=median(pc))

            ggplot(na.omit(demodfpcs), aes_string(x=names(pcs)[p], fill=small_demos[d])) +
                geom_density(alpha=0.4) +
                xlim(round(na.omit(pcs[[p]]) %>% summary %>% tidy %>% .$minimum)-0.5, 
                     round(na.omit(pcs[[p]]) %>% summary %>% tidy %>% .$maximum)+.5) +
                geom_vline(data=mu, aes_string(xintercept='grp.median', color=names(mu)[1]),
                         linetype="dashed") +
                labs(subtitle=paste0('pos-x: ', pca_labels %>% 
                      filter(question_name==question) %>% 
                      select(matches(paste0(p,'_pos'))) %>%
                      pull %>% as.character,
                     '\nneg-x: ', pca_labels %>% 
                      filter(question_name==question) %>% 
                      select(matches(paste0(p,'_neg'))) %>%
                      pull %>% as.character)) +
                theme(plot.subtitle = element_text(size=10))

            # saving
            dir.create(paste(dir_to_save_results, question, names(pcs)[p], sep='/'), showWarnings = F)
            dir.create(paste(dir_to_save_results, question, names(pcs)[p], plot_type,sep='/'), showWarnings = F)
            ggsave(paste(dir_to_save_results, question, names(pcs)[p], plot_type, 
                         paste0(small_demos[d], output_format),sep='/'), 
                   width = 5, height=4)
        }
    }
}