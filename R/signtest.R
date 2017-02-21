#One-Sample Sign Test

signtest <- function(x, m = NULL, alpha = NULL, alternative =c("two.sided", "greater", "less"), conf.level=NULL, exact = FALSE) {

  if(!is.numeric(x)||length(x)<2){
    stop("'x' must be numeric and a vector")
  }

  if(is.null(m)){
    m <- 0
  }

  if(!is.numeric(m)||length(m)>1){
    stop("'m' must be numeric and a single number")
  }

  if(is.null(alpha)){
    alpha<-0.05
  }

  if(!is.numeric(alpha)||alpha > 1||alpha<0){
    stop("'alpha' must be numeric and between 0 and 1.")
  }

  x <- sort(x)
  d <- x - m
  pos = 0
  neg = 0
  eq = 0

  for(i in 1:length(d)){
    if(d[i]>0){
      pos <- pos + 1
    }
    else if(d[i]<0){
      neg <- neg + 1
    }
    else{
      eq = eq + 1
    }
  }
  n <- length(d) - eq
  alternative <- match.arg(alternative)
  if(alternative=="two.sided"){
  B <- max(neg, pos)
  }
  else if(alternative=="less"){
    B <- neg
  }
  else{B <- pos}
  if(xor(n<20, isTRUE(exact))){
    p <- round(binom.test(B, n, 0.5, alternative = "greater")$p.value, 5)
    if(alternative=="two.sided"){
      if(2*p>1){p<-1}
      else{p<-2*p}
    }
  }
  else{
    Z <- ((B - 0.5) - 0.5*n) / (.5 * sqrt(n))
    if(alternative=="two.sided"){
      p<-2 * pnorm(-abs(Z))
    }
    else{
      p<-pnorm(-Z)
    }
  }
  if(!is.null(conf.level)){
    if(xor(n<20, isTRUE(exact))){
      alpha = 1 - conf.level
      alpha2 = alpha / 2
      level = 1 - alpha2
      b = qbinom(level, length(x), 0.5)
      a = n + 1 - b
      LB = x[a]
      UB = x[b]
      conf.inter = c(LB, UB)
    }
    else{
      alpha = 1 - conf.level
      alpha2 = alpha / 2
      level = 1 - alpha2
      Z = qnorm(level)
      a = 0.5*n - Z*0.5*sqrt(n)
      b = 0.5*n + Z*0.5*sqrt(n)
      LB = x[a]
      UB = x[b]
      conf.inter = c(LB, UB)
    }
    Confidence = "True"
  }
  else{Confidence = "False"}

  interpretation = character()
  if(p < alpha){
    interpretation = switch(alternative, two.sided=paste("There is enough evidence to conclude that the population median is different than", toString(m), "at a significance level of ", toString(alpha)),
                            less=paste("There is enough evidence to conclude that the population median is less than", toString(m), "at a significance level of ", toString(alpha)),
                            greater=paste("There is enough evidence to conclude that the population median is greater than", toString(m), "at a significance level of ", toString(alpha)))
  }
  else {
    interpretation= switch(alternative, two.sided=paste("There is not enough evidence to conclude that the population median is different than", toString(m), "at a significance level of ", toString(alpha)),
                          less=paste("There is not enough evidence to conclude that the population median is less than" ,toString(m), "at a significance level of ", toString(alpha)),
                          greater=paste("There is not enough evidence to conclude that the population median is greater than", toString(m), "at a significance level of ", toString(alpha)))
  }

  setClass("output",
           representation(
             Title="character",
             NHypothesis="character",
             AHypothesis="character",
             TestStat="character",
             siglevel="character",
             PVal="character",
             pinter="character",
             conf.int="vector",
             confiterpretation="character"), where=topenv(parent.frame())
           )
  if(xor(n<20,isTRUE(exact))){
    display<-new("output", Title="Exact Sign Test", NHypothesis=paste("H0: The population median is = ", toString(m)),
                 AHypothesis=switch(alternative, two.sided=paste("HA: The population median is not equal to ", toString(m)),
                                    less = paste("HA: The population median is less than", toString(m)),
                                    greater = paste("HA: The population median is greater than",toString(m))),
                 TestStat=paste("B =",toString(B)), siglevel = paste("Significance Level =", toString(alpha)),
                 PVal=paste("The p-value is ", toString(p)), pinter=interpretation, conf.int=switch(Confidence, True=conf.inter, False=" "),
                 confiterpretation=switch(Confidence, True=paste("The ", toString(conf.level*100), "% confidence interval is [", toString(LB),  ", ", toString(UB),  "]."), False=" ")
    )
  }
  else{
    display<-new("output", Title="Large Sample Approximation for the Sign Test", NHypothesis=paste("H0: The population median is = ", toString(m)),
                 AHypothesis=switch(alternative, two.sided=paste("HA: The population median is not equal to ", toString(m)),
                                    less = paste("HA: The population median is less than", toString(m)),
                                    greater = paste("HA: The population median is greater than", toString(m))),
                 TestStat=paste("B =",toString(B)), siglevel = paste("Significance Level =", toString(alpha)),
                 PVal=paste("The p-value is ", toString(p)), pinter=interpretation, conf.int=switch(Confidence, True=conf.inter, False=" "),
                 confiterpretation=switch(Confidence, True=paste("The ", toString(conf.level*100), "% confidence interval is [", toString(LB),  ", ", toString(UB),  "]."), False=" ")
    )
  }
  setMethod("show", "output",
            function(object){
              cat("\n", object@Title, "\n", "\n",
                  object@NHypothesis, "\n",
                  object@AHypothesis, "\n", "\n",
                  object@TestStat, "\n", "\n",
                  object@siglevel, "\n",
                  object@PVal, "\n",
                  object@pinter, "\n", "\n",
                  object@confiterpretation, "\n", "\n")
            }, where=topenv(parent.frame())
            )
  display
}


