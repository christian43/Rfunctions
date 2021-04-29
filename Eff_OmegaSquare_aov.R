# Fuction for Effectsize omega^2 using with anova
omega_sq <- function(aovm){
    sum_stats <- summary(aovm)[[1]]
    SSm <- sum_stats[["Sum Sq"]][1]
    SSr <- sum_stats[["Sum Sq"]][2]
    DFm <- sum_stats[["Df"]][1]
    MSr <- sum_stats[["Mean Sq"]][2]
    W2 <- (SSm-DFm*MSr)/(SSm+SSr+MSr)
    return(W2)
}
#for n-way aov models:

omega_sq_nway <- function(aov_in, neg2zero=T){
    aovtab <- summary(aov_in)[[1]]
    n_terms <- length(aovtab[["Sum Sq"]]) - 1
    output <- rep(-1, n_terms)
    SSr <- aovtab[["Sum Sq"]][n_terms + 1]
    MSr <- aovtab[["Mean Sq"]][n_terms + 1]
    SSt <- sum(aovtab[["Sum Sq"]])
	for(i in 1:n_terms){
        SSm <- aovtab[["Sum Sq"]][i]
        DFm <- aovtab[["Df"]][i]
        output[i] <- (SSm-DFm*MSr)/(SSt+MSr)
        if(neg2zero & output[i] < 0){output[i] <- 0}
    
	}
    names(output) <- rownames(aovtab)[1:n_terms]

    return(output)

}