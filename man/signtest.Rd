\name{signtest}
\alias{signtest}

\title{
  The Sign Test
}
\description{
  A nonpametric test for center. The sign test compares the median to a value.
}
\usage{
signtest(x, m = NULL, alpha = NULL,
alternative =c("two.sided", "greater", "less"), conf.level=NULL, exact = FALSE)
}

\arguments{
  \item{x}{
    A vector of sample data.
  }
  \item{m}{
    The median to test. Defaults to 0.
  }
  \item{alpha}{
    The Significance level, defaults to 0.05.
  }
  \item{alternative}{
    Defaults to two.sided. Used to determine what type of test to run.
  }
  \item{conf.level}{
    Defaults to NULL.  Used to construct a confidence interval. Input as a decimal.
  }
  \item{exact}{
    Defaults to FALSE. Used to determine whether to run the exact procedure or a large sample approximation.
  }
}

\value{
  \item{B}{
    The Test Statistic
  }
  \item{Significance Level}{
    Returns the alpha value.
  }
  \item{P-value}{
    Returns the p-value from the Sign Test.
  }
  \item{Confidence Interval}{
    The confidence interval requested.
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
  ## Run the Sign Test on the vector.
  signtest(c(1.8, 3.3, 5.65, 2.25, 2.5, 3.5, 2.75, 3.25, 3.10, 2.70, 3, 4.75, 3.4), m=3.5)
}

