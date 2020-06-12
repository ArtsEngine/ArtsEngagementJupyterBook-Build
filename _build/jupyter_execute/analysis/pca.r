# Principal Components Analysis

We used a Principle Components Analysis (PCA) in R ('prcomp' function) to integrate multiple variables and conduct an analysis for each question and set of responses. PCA is a multivariate statistical technique that extracts important information about the inter-correlations of multiple variables and re-represents that information as new variables called principle components. These new principle components can then be used to show the similarity and graphic relationships between variables as a way of exploring the data. Because PCA condenses multiple variables into fewer dimensions, the new principle components may also be used to integrate new descriptions of the combined variables. These reified principle components may then be used as covariates to estimate relationships with other variables such as demographic factors. 

Twenty-seven dictionary-based variables (LIWC, AIC, and Docuscope) were not relevant and were excluded from the analysis, leaving a total of 221 dictionary-based variables in the PCA. This was in addition to the measures of topic prevalence, which each had different numbers of topic variables depending on the question/response type. In order to remove variables with high numbers of 0s (no observations), we set a 0.3 maximum threshold, leaving variable columns with 70% or more observations in the analysis. The number of responses and variables included for each question in the analysis are provided in Table 1. Because of the different scales used by the different dictionaries and topic measures (for example, topic prevalence is 0-1 while AIC is measured on a scale from 1-7), the PCA was based on a correlation matrix. The data were also centered around zero and rotated.  

#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("plotly")
install.packages("factoextra")
install.packages("FactoMineR")

library(ggplot2)   # plotting;
library(plotly)
library(haven)     # SPSS/.sav file import
library(dplyr)     # data manipulation
library("corrplot")#
library(stringr)   # string manipulation
library(labelled)
library("FactoMineR")
library("factoextra")

#data <- read.csv('pcadata.csv')
load(file = 'merged_artsengagement.rda')
ls()

data <- df
#print the dimension of the dataframe
dim(data)

stripnames <- function(data, question) {
    data <- data %>% select(starts_with(question))
    varnames <- names(data)
    newnames <- varnames
    for (i in seq(2, length(varnames))) {
        newnames[i] <- substr(varnames[i], gregexpr("\\.\\.", varnames[i])[[1]][1]+2, nchar(varnames[i]))
    }
    names(data) <- newnames
    return(data)
}

mergeQuestion <- function(data, question) {
    if(substr(question, 3,3) == '_') {
        question <- substr(question, 4, nchar(question))
    }
    # if you're aware of other prefixes, add them here
    yrs <- c('','fl_','f1_','f2_','so_','jr_','sr_','sl_')
    mergeddf <- data.frame()
    for(yr in yrs) {
        qyr <- paste0(yr, question)
        if(data %>% select(starts_with(qyr)) %>% ncol == 0) { next }
        tempdf <- stripnames(df, qyr)
        names(tempdf)[1] <- question
        mergeddf <- bind_rows(mergeddf, tempdf)        
    }
    return(mergeddf)
}

stripVars <- function(data) {
    #remove irrelavant variables
    removenames <- names(data %>% select(matches("AIC..Words|AIC..IC_Differentiation|AIC..IC_Integration|AIC..DIAL_Differentiation|AIC..DIAL_Integration|AIC..ELAB_Differentiation|AIC..ELAB_Integration|docuscope..OralCues|docuscope..DialogCues|docuscope..XWordTokens|docuscope..XPunctuationTokens|docuscope..XTokens|LIWC..WC|LIWC..Sixltr|LIWC..Dic|LIWC..WPS|LIWC..swear|LIWC..netspeak|LIWC..assent|LIWC..nonflu|LIWC..filler|LIWC..AllPunc|LIWC..Period|LIWC..Comma|LIWC..Colon|LIWC..SemiC|LIWC..QMark|LIWC..Exclam|LIWC..Dash|LIWC..Quote|LIWC..Apostro|LIWC..Parenth|LIWC..OtherP")))
    data <- data[ , !names(data) %in% removenames]
    return(data)
}

#example input for parameter question: "sr_career"
#threshold: set the threshold for cos2. only variables with cos2> threshold will be kept in biplots
#percent: set the percentage of 0 in a column; columns with percentage of 0 values > percent will be removed
pca_biplot <- function(question,threshold,percent) {
    
    
    if(substr(question, 3,3) == '_') {
        question <- substr(question, 4, nchar(question))
    }
    
    # select columns relevant to the question
    data_subset <- mergeQuestion(data, question) %>% stripVars
    print(dim(data_subset))
    #create file for eigenvales and output
    eigfile <- paste(question,"_eigenvalues.txt", sep="")
    sink(eigfile, append=FALSE, split=TRUE)       
    
    #remove rows with no response
    data_subset <- data_subset[complete.cases(data_subset),]
    print(dim(data_subset))
    # replace NULL with 0
    # this shouldnt be necessary -- investigate if so
    # data_subset[is.na(data_subset)] <- 0

    #remove columns with percentage of 0 values > percent 
    data_subset <- data_subset[ lapply( data_subset, function(x) sum(x==0) / length(x) ) < percent ]

    print(dim(data_subset))

    print(names(data_subset))

    #scale all the variables
    data_subset[, -c(1)] <- scale(data_subset[, -c(1)])

    #create new dataframe pcadata to store scaled variables
    pcadata<-data_subset[,-c(1)]



    #remove columns that only have NULL; columns that only have 0 after scaling will become NULL
    pcadata2=Filter(function(x)!all(is.na(x)), pcadata)
    print(dim(pcadata2))


    #fit PCA uses correlatio matrix, rotation, centered
    pca.fit <- prcomp(pcadata2, retx= TRUE, center=TRUE, scale=TRUE)
    eig.val <- get_eigenvalue(pca.fit)


    #get variable contributions
    var <- get_pca_var(pca.fit)
    #print(var$contrib)
    #print(var$coord)
    #print(pca.fit)

 
    # Print Eigenvalues and variable contributions and close sink
    print(eig.val)

    sink(append=TRUE, type="message")
    sink()

    # Export into a TXT file
    write.infile(pca.fit, file=paste(question,"_pca_results.txt", sep=""), sep = "\t")


    #print screeplot              
    pdf(file=paste(question,"_screeplot.pdf", sep=""))    
    screeplot=fviz_eig(pca.fit, addlabels = TRUE, ylim = c(0, 50))
    print(screeplot)
    dev.off()

    #make biplot for PC1&2               
    p12=fviz_pca_var(pca.fit, axes = c(1, 2), col.var = "cos2", alpha.var="cos2", select.var = list(cos2 = threshold),
                 gradient.cols = c("goldenrod1", "firebrick2", "navy"), 
                 repel = TRUE # Avoid text overlapping
                 )+ xlim(-1, 1) + ylim (-1, 1)
                    
                    
    #make biplot for PC3&4              
    p34=fviz_pca_var(pca.fit, axes = c(3, 4), col.var = "cos2", alpha.var="cos2", select.var = list(cos2 = threshold),
                 gradient.cols = c("goldenrod1", "firebrick2", "navy"), 
                 repel = TRUE # Avoid text overlapping
                 )+ xlim(-1, 1) + ylim (-1, 1)

    #print biplot                
    pdf(file=paste(question,"_biplot12.pdf", sep=""))
    print(p12)
    dev.off()
                    
    #print biplot                
    pdf(file=paste(question,"_biplot34.pdf", sep=""))
    print(p34)
    dev.off()

    #print charts of variable contrib and cos2

    pdf(file=paste(question,"_corr_cos2_viz.pdf", sep=""))
    corrcos2viz <- corrplot(var$cos2, is.corr=FALSE, tl.cex = .5)   
    print(corrcos2viz)
    dev.off()

    pdf(file=paste(question,"_corr_contrib_viz.pdf", sep=""))
    corrcontribviz <- corrplot(var$contrib, is.corr=FALSE, tl.cex = .5)    
    print(corrcontribviz)
    dev.off()


    # Contributions of variables to PC1
    pdf(file=paste(question,"_contributionPC1.pdf", sep=""))
    c1=fviz_contrib(pca.fit, choice = "var", axes = 1, top = 10)
    print(c1)              
    dev.off()
    # Contributions of variables to PC2
    pdf(file=paste(question,"_contributionPC2.pdf", sep=""))
    c2=fviz_contrib(pca.fit, choice = "var", axes = 2, top = 10)
    print(c2)
    dev.off()
    # Contributions of variables to PC3
    pdf(file=paste(question,"_contributionPC3.pdf", sep=""))
    c3=fviz_contrib(pca.fit, choice = "var", axes = 3, top = 10)
    print(c3)
    dev.off()
    # Contributions of variables to PC4
    pdf(file=paste(question,"_contributionPC4.pdf", sep=""))
    c4=fviz_contrib(pca.fit, choice = "var", axes = 4, top = 10)
    print(c4)
    dev.off()
    # Contributions of variables to PC5
    pdf(file=paste(question,"_contributionPC5.pdf", sep=""))
    c5=fviz_contrib(pca.fit, choice = "var", axes = 5, top = 10)
    print(c5)
    dev.off()



    ###### interactive pca plot
    #add text response back to pcadata2
    pcadata2$text<-data_subset[[question]]
    res.pca <- PCA(pcadata2[,!(colnames(pcadata2) %in% c("text"))], graph = FALSE)

    #make biplot with individual dots
    test2<-fviz_pca_biplot(res.pca, axes = c(1, 2), geom=c('point','text'), col.ind = "light blue",
                    pointshape = 19, pointsize = 2,alpha.ind = 0.3,
                    label = "var", 
                    alpha.var ="cos2", select.var = list(cos2 = threshold),
                     col.var = "cos2",gradient.cols = c("blue", "red")

                    )
                    
                    
      #labs(fill = "identity")# Change legend title

    #add hover text
    mytext=paste(pcadata2$text) 
    pp=ggplotly(p=test2,width=800,height=800)
    ppp=style(pp,text=mytext,hoverinfo="text", traces = c(1,2,3))
    htmlwidgets::saveWidget(as_widget(ppp), paste(question,"_plotly12.html", sep=""))
                    
    test3<-fviz_pca_biplot(res.pca, axes = c(3, 4), geom=c('point','text'), col.ind = "light blue",
                    pointshape = 19, pointsize = 2,alpha.ind = 0.3,
                    label = "var", 
                    alpha.var ="cos2", select.var = list(cos2 = threshold),
                     col.var = "cos2",gradient.cols = c("blue", "red")

                    )
                    
    #add hover text
    mytext=paste(pcadata2$text) 
    pp=ggplotly(p=test3,width=800,height=800)
    ppp=style(pp,text=mytext,hoverinfo="text", traces = c(1,2,3))
    htmlwidgets::saveWidget(as_widget(ppp), paste(question,"_plotly34.html", sep=""))
}
                
            

#example input for parameter question: "career"
#1st is threshold: set the threshold for cos2. only variables with cos2> threshold will be kept in biplots
#2nd is percent: set the percentage of 0 in a column; columns with percentage of 0 values > percent will be removedpca_biplot('barriers',0.3,0.4)
pca_biplot('career',0.2,0.3)
pca_biplot('development',0.2,0.3)
pca_biplot('othergrowth',0.2,0.3)
pca_biplot('society',0.2,0.3)
pca_biplot('role',0.2,0.3)
pca_biplot('barriers',0.2,0.3)
pca_biplot('feel',0.2,0.3)
pca_biplot('definition',0.2,0.3)
pca_biplot('aftercollege4',0.2,0.3)
pca_biplot('behavior2',0.2,0.3)
pca_biplot('change',0.2,0.3)
pca_biplot('childhood2',0.2,0.3)
