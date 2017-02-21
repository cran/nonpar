\name{cochrans.q}
\alias{cochrans.q}

\title{
  Cochran's Q Test
}
\description{
  This function will perform the Cochran's Q Test to test for identical treatment effects in a two-way randomized block design with k treatments.
}
\usage{
  cochrans.q(x, alpha=NULL)
}

\arguments{
  \item{x}{
    A b x k matrix, where b is the number of blocking factors and k is the number of treatment factors.
  }

  \item{alpha}{
    The Significance level, defaults to 0.05.
  }
}

\value{
  \item{Q}{
    This is the Cochran's Q Test Statistic.
  }
  \item{Degrees of Freedom}{
    The number of degrees of freedom used in calculating the p-value.
  }
  \item{Significance Level}{
    Returns the alpha value.
  }
  \item{P-value}{
    Returns the p-value from the Cochran's Q Test.
  }
}
\references{
  \url{https://www.r-bloggers.com/cochran-q-test-for-k-related-samples-in-r/}

  \url{http://rcompanion.org/handbook/H_07.html}
}

\author{
  D. Lukke Sweet
}

\examples{
  ## Run Cochran's Q Test on a matrix.
  cochrans.q(matrix(c(1,1,1,1,1,1,
              1,1,0,1,1,1,
              0,0,0,1,0,0,
              0,1,0,0,1,1), 6, 4))

  ## Cochran's Q Test works for any size matrix.
  cochrans.q(matrix(c(0,1,0,0,1,0,0,0,1,0,0,0,0,0,
                0,1,1,1,1,1,1,1,0,1,1,1,1,1,
                0,1,0,0,0,0,0,0,1,0,0,0,0,0,
                0,1,1,0,0,1,1,0,0,0,0,1,0,1), 14, 4), alpha=0.01)

}
