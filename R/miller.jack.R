## Miller's Jackknife Procedure

miller.jack <- function(x, y, alpha = NULL, alternative =c("two.sided", "greater", "less"), exact = FALSE) {

  if(!is.numeric(x)||length(x)<2)
    stop("'x' must be numeric and a vector")
  if(!is.numeric(y)||length(y)<2)
    stop("'y' must be numeric and a vector")

  if(is.null(alpha)){
    alpha<-0.05
  }

  if(!is.numeric(alpha)||alpha > 1||alpha<0){
    stop("'alpha' must be numeric and between 0 and 1.")
  }

    n1 = length(x)
    n2 = length(y)
    df = n1 + n2 - 2
    n = min(n1, n2)

    s1 = vector(length=n1)
    s2 = vector(length=n2)

    for(i in c(1:n1)){
      s1[i] = log(var(x[-i]))
    }
    for(i in c(1:n2)){
      s2[i] = log(var(y[-i]))
    }

    s01 = log(var(x))
    s02 = log(var(y))

    a1 = n1*s01 - ((n1-1) * s1)
    a2 = n2*s02 - ((n2-1) * s2)

    A1 = sum(a1) / n1
    A2 = sum(a2) / n2

    V1 = sum((a1-A1)^2) / (n1*(n1-1))
    V2 = sum((a2-A2)^2) / (n2*(n2-1))

    J <- (A1 - A2) / sqrt(V1+V2)

    alternative <- match.arg(alternative)
    if(xor(n<20, isTRUE(exact))){
      p <- pt(-abs(J), df=df)
      if(alternative=="two.sided"){
        if(2*p>1){p<-1}
        else{p<-2*p}
      }
      else if(alternative=="greater"){
        p <- 1 - p
      }
    }
    else {
      p <- pnorm(-abs(J))
      if(alternative=="two.sided"){
        if(2*p>1){p<-1}
        else{p<-2*p}
      }
      else if(alternative=="greater"){
        p <- 1 - p
      }
    }

    interpretation = character()
    if(p < alpha){
      interpretation = switch(alternative, two.sided=paste("There is enough evidence to conclude that the population varicances are different at a significance level of ", toString(alpha), "."),
                              less=paste("There is enough evidence to conclude that the population variance of population 1 is less than population 2 at a significance level of ", toString(alpha), "."),
                              greater=paste("There is enough evidence to conclude that the population variance of population 1 is greater than population 2 at a significance level of ", toString(alpha), "."))
    }
    else {
      interpretation= switch(alternative, two.sided=paste("There is not enough evidence to conclude that the population variances are different at a significance level of ", toString(alpha), "."),
                             less=paste("There is not enough evidence to conclude that the population variance of population 1 is less than population 2 at a significance level of ", toString(alpha), "."),
                             greater=paste("There is not enough evidence to conclude that the population variance of population 1 is greater than population 2 at a significance level of ", toString(alpha), "."))
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
      display<-new("output", Title="Miller's Jackknife Procedure", NHypothesis=paste("H0: The population variances are equal."),
                   AHypothesis=switch(alternative, two.sided=paste("HA: The population variances are not equal."),
                                      less = paste("HA: The variance of population 1 is less than population 2."),
                                      greater = paste("HA: The variance population 1 is greater than population 2.")),
                                      TestStat=paste("J =",toString(J)), siglevel = paste("Significance Level =", toString(alpha)),
                                      PVal=paste("The p-value is ", toString(p)), pinter=interpretation)


    }
    else{
      display<-new("output", Title="Large Sample Approximation for Miller's Jackknife Procedure", NHypothesis=paste("H0: The population variances are equal."),
                   AHypothesis=switch(alternative, two.sided=paste("HA: The variance population 1 is not equal to population 2."),
                                      less = paste("HA: The variances of population 1 is less than population 2."),
                                      greater = paste("HA: The variance population 1 is greater than population 2.")),
                                      TestStat=paste("J =",toString(J)), siglevel = paste("Significance Level =", toString(alpha)),
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
