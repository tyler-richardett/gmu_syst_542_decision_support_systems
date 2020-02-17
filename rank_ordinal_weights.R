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