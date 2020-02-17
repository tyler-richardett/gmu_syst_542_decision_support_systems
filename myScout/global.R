library(dplyr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyjqui)
library(scales)
library(topsis)
library(bsplus)
library(tools)


target_players_model_positions <- read.csv("target_players_model_positions.csv", colClasses = "character")
target_players_model_values <- read.csv("target_players_model_values.csv", 
                                        colClasses = c(rep("character", 2), "numeric", "Date",
                                                       rep("numeric", 2), rep("character", 3),
                                                       "logical", rep("character", 2),
                                                       "numeric", "Date", "character",
                                                       rep("numeric", 4), rep("character", 3), "numeric"),
                                        fileEncoding = "UTF-8")
target_players_season_stats <- read.csv("target_players_season_stats.csv",
                                        colClasses = c(rep("character", 2), rep("numeric", 10)))

position_clusters <- c("Central Defender", "Left-Sided Midfielder/Fullback", 
                       "Central Midfielder", "Right-Sided Midfielder/Fullback", 
                       "Left-Sided Attacker", "Central Forward", "Right-Sided Attacker")

height_values <- data.frame(height_preference = c(rep("Short", 3), rep("Median", 3), rep("Tall", 3), rep("No Preference", 3)),
                            binned_height = rep(c("Short", "Median", "Tall"), 4),
                            height_score = c(1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 1, 1, 1),
                            stringsAsFactors = FALSE)

weight_values <- data.frame(weight_preference = c(rep("Light", 3), rep("Median", 3), rep("Heavy", 3), rep("No Preference", 3)),
                            binned_weight = rep(c("Light", "Median", "Heavy"), 4),
                            weight_score = c(1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 1, 1, 1),
                            stringsAsFactors = FALSE)

age_values <- data.frame(age_preference = c(rep("Long-Term", 3), rep("Medium-Term", 3), rep("Short-Term", 3)),
                         binned_age = rep(c("22 or younger", "23-28", "29 or older"), 3),
                         binned_age_score = c(1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1),
                         stringsAsFactors = FALSE)





# Source: https://taragonmd.github.io/2018/08/11/criteria-weights-for-decision-making-the-easy-way/

rank.ordinal.weights <- function(x, criteria, labels, evaluators,
                                 method=c("sr","roc"), weights){
        ## x = matrix: row is criteria ranking for each evaluator
        ## criteria = character vector of criteria factor levels (values)
        ## labels = character vector of criteria labels (long names)
        ## evaluators = evaluator names
        ## weights = full vector of customized weights (optional)
        ## NOTE: unique values in x must be subset of criteria levels
        ##       To override default factors, provide both criteria
        ##       levels and labels
        ## methods = see https://link.springer.com/chapter/10.1007/978-3-319-52624-9_2
        method <- match.arg(method)
        if(method=="sr") {
                calc.rank.wts = function(k){
                        if(missing(k)){
                                stop("Must provide integer value for k (number of criteria)")
                        }
                        wi = dd = rep(NA, k)
                        for(i in 1:k){
                                dd[i] = (1/i)+((k+1-i)/(k))
                        }
                        denom = sum(dd)
                        for(i in 1:k){
                                wi[i] = ((1/i)+((k+1-i)/(k)))/(denom)
                        }
                        return(wi)
                }
        }
        if(method=="roc"){
                calc.rank.wts = function(k){
                        if(missing(k)){
                                stop("Must provide integer value for k (number of criteria)")
                        }
                        wi = rep(NA, k)
                        for(i in 1:k){
                                wi[i] = (1/k)*sum(1/(i:k))
                        }
                        return(wi)
                }
        }
        
        nr = nrow(x); nc = ncol(x)
        if(missing(criteria)) criteria = levels(factor(as.vector(t(x))))
        if(missing(labels)) labels = criteria
        if(missing(evaluators)){
                if(!is.null(rownames(x))) evaluators = rownames(x)
                if(is.null(rownames(x))) evaluators = paste("Eval_", 1:nr, sep = "")
        }
        eval.vec = rep(evaluators, rep(nc, nr))
        crit.vec = as.vector(t(x))
        roc.wts = calc.rank.wts(k = nc)
        if(missing(weights)) wts.vec = rep(roc.wts, nr)
        if(missing(weights)) roc.vec = rep(roc.wts, nr)
        df = data.frame(Evaluator = eval.vec,
                        Criterion = factor(crit.vec, levels = criteria),
                        Weight = roc.vec,
                        Label = factor(crit.vec, levels = criteria, labels = labels)
        )
        criteria.wts = rev(sort(tapply(df$Weight, df$Criterion, sum)/nr))
        criteria.wts2 = as.matrix(rev(sort(tapply(df$Weight,
                                                  df$Label, sum)/nr)))
        ranktab = data.frame(Criterion = names(criteria.wts),
                             Label = rownames(criteria.wts2),
                             Weight = criteria.wts)
        list(x= x,
             criteria = criteria,
             labels = labels,
             results = df,
             ranking = ranktab
        )
}
