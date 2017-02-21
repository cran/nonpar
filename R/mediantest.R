#Median Two-Sample Test

mediantest <- function(x, y, alpha = NULL, exact=FALSE) {

  if(!is.numeric(x)||length(x)<2){
    stop("'x' must be numeric and a vector")
  }

  if(!is.numeric(y)||length(y)<2){
    stop("'y' must be numeric and a vector")
  }

  if(is.null(alpha)){
    alpha<-0.05
  }

  if(!is.numeric(alpha)||alpha > 1||alpha<0){
    stop("'alpha' must be numeric and between 0 and 1.")
  }

  n1 = length(x)
  n2 = length(y)
  n = min(n1, n2)
  N = n1 + n2

  belowx = 0
  belowy = 0
  abovex = 0
  abovey = 0

  X = append(x, y)
  mhat <- median(X)
  for(i in 1:length(x)){
    if(x[i]<mhat){
      belowx = belowx + 1
    }
    else{abovex = abovex + 1}
  }
  for(i in 1:length(y)){
    if(y[i]<mhat){
      belowy = belowy + 1
    }
    else{abovey = abovey + 1}
  }

  countx = append(belowx, abovex)
  county = append(belowy, abovey)
  count = rbind(countx, county)
  totalbelow = belowx+belowy
  totalabove = abovex+abovey


  if(n<20||isTRUE(exact)) {
    num = (choose(totalbelow, belowx))*(choose(totalabove, abovex))
    den = choose(N, n1)
    p = 2 * (num / den)
  }
  else {
    Z = ((count[2,1]*count[1,2])-(count[1,1]*count[2,2])) / sqrt((totalbelow*totalabove*n1*n2)/N)
    p = 2*pnorm(-abs(Z))
  }

  interpretation = character()
  if(p < alpha){
    interpretation = paste("There is enough evidence to conclude that the population medians are different at a significance level of ", toString(alpha), ".")
  }
  else {
    interpretation = paste("There is not enough evidence to conclude that the population medians are different at a significance level of ", toString(alpha), ".")
  }

  setClass("output",
           representation(
             Title="character",
             NHypothesis="character",
             AHypothesis="character",
             TestStat="character",
             siglevel="character",
             PVal="character",
             pinter="character"), where=topenv(parent.frame())
  )
  if(n<20 || isTRUE(exact)){
    display<-new("output", Title="Exact Median Test", NHypothesis=paste("H0: The 2 population medians are equal. "),
                 AHypothesis=paste("HA: The 2 population medians are not equal."),
                 TestStat=paste(""), siglevel = paste("Significance Level =", toString(alpha)),
                 PVal=paste("The p-value is ", toString(p)), pinter=interpretation)
  }
  else{
    display<-new("output", Title="Large Sample Approximation for the Median Test", NHypothesis=paste("H0: The 2 population medians are equal."),
                 AHypothesis=paste("HA: The 2 population medians are not equal."),
                 TestStat=paste("Z =", toString(Z)), siglevel = paste("Significance Level =", toString(alpha)),
                 PVal=paste("The p-value is ", toString(p)), pinter=interpretation)
  }
  setMethod("show", "output",
            function(object){
              cat("\n", object@Title, "\n", "\n",
                  object@NHypothesis, "\n",
                  object@AHypothesis, "\n", "\n",
                  object@TestStat, "\n", "\n",
                  object@siglevel, "\n",
                  object@PVal, "\n",
                  object@pinter, "\n", "\n")
            }, where=topenv(parent.frame())
  )
  display
}



