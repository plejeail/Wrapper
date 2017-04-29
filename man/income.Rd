\docType{data}
\name{income}
\alias{income}
\title{Adult Census Income}
\format{\describe{
\item{age}{age of the individual}
\item{workclass}{activity area}
\item{fnlwgt}{The weights on the Current Population Survey}
\item{education}{Highest diploma obtained}
\item{education-num}{}
\item{marital-status}{}
\item{occupation}{}
\item{relationship}{Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried}
\item{race}{White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black}
\item{sex}{}
\item{capital-gain}{}
\item{capital-loss}{}
\item{hours-per-week}{time of work per week}
\item{native-country}{}
\item{income}{wether income is superior or inferior to 50k}
}}
\usage{
income
}
\description{
This data was extracted from the 1994 Census bureau database by Ronny Kohavi and Barry Becker (Data Mining and Visualization, Silicon Graphics). A set of reasonably clean records was extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1) && (HRSWK>0)). The prediction task is to determine whether a person makes over $50K a year.
}
\author{
Ronny Kohavi and Barry Becker
}
\keyword{datasets}
