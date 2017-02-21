\name{miller.jack}
\alias{miller.jack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  The Miller Jackknife Procedure
}
\description{
  This function will perform Miller's Jackknife Procedure to test differences in scale between 2 samples.  It is best for large samples.
}
\usage{
miller.jack(x, y, alpha = NULL,
alternative =c("two.sided", "greater", "less"), exact = FALSE)
}

\arguments{
  \item{x}{
    A vector containing data from the first sample.
  }
  \item{y}{
    A vector containing data from the second sample.
  }
  \item{alpha}{
    The Significance level, defaults to 0.05.
  }
  \item{alternative}{
    Defaults to two.sided. Used to determine what type of test to run.
  }
  \item{exact}{
    Defaults to FALSE. Used to determine whether to run the exact procedure or a large sample approximation.
  }
}

\value{
  \item{J}{
    The test statistic.
  }
  \item{Significance Level}{
    Returns the alpha value.
  }
  \item{P-value}{
    Returns the p-value from Miller's Jackknife Procedure.
  }
}

\references{
 Wiley Series in Probability and Statistics: Nonparametric Statistical Methods (3rd Edition). (2013). John Wiley & Sons.
}

\author{
  D. Lukke Sweet
}

\examples{
  ## Run Miller's Jackknife Procedure on the 2 vectors.
  miller.jack(x= c(6.2, 5.9, 8.9, 6.5, 8.6),
              y = c(9.5, 9.8, 9.5, 9.6, 10.3), alpha=0.05, alternative="less")

}
