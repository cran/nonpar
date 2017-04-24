## Stuart-Maxwell Test (n-Sample McNemar's)
## Use Fridline's Data to Test


stuart.maxwell <- function(X, alpha = NULL){

  if(nrow(X)!=3||ncol(X)!=3){
    stop("Data must be a 3x3 matrix")
  }

  if(is.null(alpha)){
    alpha<-0.05
  }

  if(!is.numeric(alpha)||alpha > 1||alpha<0){
    stop("'alpha' must be numeric and between 0 and 1.")
  }

  Xdiff1 = sum(X[1,]) - sum(X[,1])
  Xdiff2 = sum(X[2,]) - sum(X[,2])

  d = matrix(c(Xdiff1, Xdiff2),2,1)

  s11 = sum(X[1,]) + sum(X[,1]) - 2*X[1,1]
  s22 = sum(X[2,]) + sum(X[,2]) - 2*X[2,2]
  s12 = -1 * (X[1,2] + X[2,1])
  s21 = s12

  S = matrix(c(s11, s12, s21, s22), 2,2)


  ts = t(d) %*% solve(S) %*% d

  df = nrow(X) - 1

  p = 1 - pchisq(ts, df)

  interpretation = character()
  if(p < alpha){
    interpretation = "There is enough evidence to conclude that there is a difference in the distribution between the paired groups."
  }
  else{
    interpreation = "There is not enough evidence to conclude that there is a difference in the distribution between the paired groups."
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

  display<-new("output", Title="Stuart-Maxwell Test", NHypothesis=paste("H0: There is no difference in the distribution between the paired groups."),
               AHypothesis=paste("HA: There is a difference in the distribution between the paired groups."),
               TestStat=paste("Test Statistic =",toString(ts)), DF = paste("Degrees of Freedom =", toString(df)),
               alphalevel = paste("Significance Level =", toString(alpha)),
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
