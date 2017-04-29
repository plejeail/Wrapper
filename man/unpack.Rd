\name{unpack}
\alias{unpack}

\title{
Unpack Vector(s)
}
\description{
 Concatenate vectors after converting to character. Unpack a vector \code{v} as a string of concatenated values, separated by sep "+". Userful for as.formula.
}
\usage{
unpack(v, separator = " + ")
}

\arguments{
  \item{v}{
one or more \code{R} objects, to be converted to character vector(s)
}
  \item{separator}{
a character string to separate the terms. Default: the character is "+"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
The implementation of fonction formula by Pierre Lejeail, Jean-Luc Yonwe & Maite Garcia (2016).
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# define vector v
v <- c("Hello", "world!", "Vector", "unpacked!")
unpack(v, separator=" ")

# Example with Iris
data(iris)
v <- colnames(iris)[-5]
# return in shape for as.formula without interactions
f1 <- paste("Species ~", unpack(v, "+"))
# return in shape for as.formula with interactions
f2 <- paste("Species ~", unpack(v, "*"))

library(MASS)
# lda without interactions
lda(as.formula(f1), data=iris, family="binomial")
# lda with interactions
lda(as.formula(f2), data=iris, family="binomial")
}

\keyword{ unpack }
\keyword{ formula }
\keyword{ paste }
