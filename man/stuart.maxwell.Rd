\name{stuart.maxwell}
\alias{stuart.maxwell}

\title{
  The Stuart-Maxwell Test
}
\description{
  This function runs the Stuart-Maxwell Test, an extension of McNemar's for a 3x3 matrix.
}
\usage{
stuart.maxwell(X, alpha = NULL)
}

\arguments{
  \item{X}{
    A 3x3 matrix of frequencies.
  }
  \item{alpha}{
    The Significance level, defaults to 0.05.
  }
}

\value{
  \item{Test Statistic}{
    The Test Statistic for the Stuart-Maxwell Test.
  }
  \item{Significance Level}{
    Returns the alpha value.
  }
  \item{P-value}{
    Returns the p-value from the Stuart-Maxwell Test.
  }
}

\author{
  D. Lukke Sweet
}

\examples{
  ## Run the Stuart-Maxwell Test on the 3x3 Matrix.
  stuart.maxwell(matrix(c(12, 30, 13, 7, 70, 34, 3, 20, 32), 3,3))
}

