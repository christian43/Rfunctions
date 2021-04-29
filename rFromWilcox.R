## Effectsize for Wilcoxon rank-sum test and Wilcoxon signed rank test
# Field S.665


rFromWilcox <-function(wilcoxModel, N){
  z <- qnorm(wilcoxModel$p.value/2)
  r <- round(z/sqrt(N),2)
  cat(wilcoxModel$data.name, "Effects Size, r = ", r)
}