\name{mediantest}
\alias{mediantest}

\title{
  2 Sample Median Test
}
\description{
  The 2 sample median test is for testing the medians of 2 samples to see if they are equal.
}
\usage{
  mediantest(x, y, alpha=NULL, exact=FALSE)
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
  \item{exact}{
    Defaults to FALSE. Runs the exact test or a large sample approximation.
  }
}

\value{
  \item{Z}{
    The test statistic for the large sample approximation.
  }
  \item{P-value}{
    Returns the p-value from the Median Test.
  }
}

\references{
 Higgins, J. J. (2005). An Introduction to modern nonparametric statistics. Belmont: Thomson Brooks/Cole.

 Wiley Series in Probability and Statistics: Nonparametric Statistical Methods (3rd Edition). (2013). John Wiley & Sons.
}

\author{
  D. Lukke Sweet
}

\examples{
  ## Run the Median Test on the 2 vectors.
  mediantest(x = c(5.5, 5.8, 6.8, 6.9, 7.2, 7.3, 7.5, 7.6, 8.0),
             y = c(5.3, 5.4, 5.6, 5.7, 6.2, 6.4, 6.6, 6.7, 8.2), exact=TRUE)

}
