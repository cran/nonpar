## Cochran's Q Test


## Q is the Test Stat
## k is the number of treatments (columns)
## b is the number of blocks (rows)
## x.j = column total for the jth treatment
## xi. = row total for ith block
## Pvalue obtained through chi square test


cochrans.q = function(x, alpha=NULL){

  if(is.null(alpha)){
    alpha<-0.05
  }

  if(!is.numeric(alpha)||alpha > 1||alpha<0){
    stop("'alpha' must be numeric and between 0 and 1.")
  }

  k <- ncol(x)
  b <- nrow(x)
  N <- sum(x)

  x.j <- 0
  for(j in c(1:k)){
    x.j <- x.j + sum(x[,j])
  }
  x.j2 <- 0
  for(j in c(1:k)){
    x.j2 <- x.j2 + sum(x[,j])^2
  }

  xi. <- 0
  for(i in c(1: b)){
    xi. <- xi. + sum(x[i,])
  }

  xi.2 <- 0
  for(i in c(1:b)){
    xi.2 <- xi.2 + sum(x[i,])^2
  }

  numerator <- (k*x.j2 - x.j^2)
  denominator <- k*xi. - xi.2

  Q <- (k-1) * (numerator / denominator)

  df = k-1

  p = 1 - pchisq(Q, df)

  interpretation = character()
  if(p < alpha){
    interpretation = "There is enough evidence to conclude that the effectiveness of at least two treatments differ."
  }
  else{
    interpreation = "There is not enough evidence to conclude that there is a difference in the effectiveness of treatments."
  }

  setClass("output",
           representation(
             Title="character",
             NHypothesis="character",
             AHypothesis="character",
             TestStat="character",
             DF = "character",
             alphalevel = "character",
             PVal="character",
             pinter="character"), where=topenv(parent.frame())
  )

  display<-new("output", Title="Cochran's Q Test", NHypothesis=paste("H0: There is no difference in the effectiveness of treatments."),
               AHypothesis=paste("HA: There is a difference in the effectiveness of treatments."),
               TestStat=paste("Q =",toString(Q)), DF = paste("Degrees of Freedom =", toString(df)),
               alphalevel=paste("Significance Level =", toString(alpha)),
               PVal=paste("The p-value is ", toString(p)), pinter=interpretation)

  setMethod("show", "output",
            function(object){
              cat("\n", object@Title, "\n", "\n",
                  object@NHypothesis, "\n",
                  object@AHypothesis, "\n", "\n",
                  object@TestStat, "\n", "\n",
                  object@DF, "\n", "\n",
                  object@alphalevel, "\n",
                  object@PVal, "\n",
                  object@pinter, "\n", "\n")
            }, where=topenv(parent.frame())
  )
  display
}


