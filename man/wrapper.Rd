
\name{wrapper}
\alias{wrapper}
\title{Method selection wrapper
}
\description{
The wrapper method is a strategy of selection of features for data mining models.
It can be used for a serie of statistical methods as the Linear discriminant analysis for example
}

\usage{
wrapper(dataset, learner="svm", train=0.7, algorithm="forward", k.lim=2,
        target=NULL, features=NULL, plot.err=TRUE, ...)
}


\arguments{
  \item{dataset}{
numeric dataset of data to learn
}
  \item{learner}{
classifier used to learn the model.
Default: \code{svm}
}
  \item{train}{
split \code{dataset} train/test.
Default: value of the parameter is \code{0.7}
}
\item{algorithm}{
wrapper algorithm used to select variables.
Default:  \code{forward}
}
  \item{k.lim}{
number of iteration to continue without optimizing the error rate.
Default: there are two iteration
}
  \item{target}{
index/name of the target column of the dataset that we want to learn.
Default: last column if the parameter is \code{NULL}
}
  \item{features}{
index/name of the features column of the dataset that we want to learn from.
Default: all but target if the parameter is \code{NULL}
}
  \item{plot.err}{
boolean indicating whether to plot or no the error rate for all the tested models. Default: plot is display
}
  \item{...}{
supplementary parameters for the classifier
}
}
\details{
The algorithms backward, forward and genetics are implemented. It is possible to choose one of these approaches.
Forward is an additive selection of features from nothing to all the features.
Backward start with all features and eliminate the features which do not allow to decrease the rate of error of the model.
Genetic algorithm is stochastic, less sensitive to local optima and stronger with big datasets.

It is possible to use the following methods of learning: the logistic regression (logit), the linear discriminating analysis (\code{\link[MASS]{lda}}) and quadratique ( \code{\link[MASS]{qda}}), the linear regression (\link[stats]{lm}), Support Vector Machine (\code{\link[e1071]{svm}}), the bayesian naive classification (\code{\link[e1071]{naiveBayes}}) and random forest (\code{\link[randomForest]{randomForest}}).
The logistic regression is done with \code{\link[stats]{glm}} and the family="binomial" option.
Furthermore, you can use your own functions (see examples)

the train parameter can be the proportion of the dataset to use for training (train < 1), the size of the training set (train > 1) or a vector
containing the indices of the rows used for training (length(train) > 1).

As forward and backward are sensitive to local minimums, they can continue to look for k.lim iterations when they reach a local minimum without finding a best model.
Thereby, the higher the value of k.lim, more the methods will be flexibles. On the other hand, they test more models and the execution time can increase greatly.
This parameter is unused by the genetic algorithm.

\code{...} add named parameters to the learning function. This is useful for naive bayes, random forest and svm. Unforunately, this parameter cannot be used with
the genetic algorithm.

The criterion of selection of the best model is based on the minimisation of the error rate (except lm which use the BIC).
}
\value{
\code{modele} return an \code{object} of the called learner (logit, \code{\link[MASS]{lda}}, \code{\link[MASS]{qda}}, \code{\link[stats]{lm}}, \code{\link[e1071]{svm}},  \code{\link[e1071]{naiveBayes}} and \code{\link[randomForest]{randomForest}}).
\code{S3} object (~= list) containing the historic of models, the historic of error rate, the best formula and some parameters as which learner and which algorithm was used.

  \item{algorithm}{
name of the algorithm wrapper used
}
  \item{classifier}{name of the method of learning
}
  \item{formula}{
formula allowing to find the model (object of type "language", obtained with as.formula)
}
  \item{best.feats}{
list of the best features
}
  \item{hist.mods}{
history of the subsets of features tested (in sub the shape of list)
}
\item{hist.errors}{
  vector of the rates of errors of the tested models
}
\item{error.rate}{
rate of error of the best model
}
  \item{train.sample}{
vector of the numbers of lines used for the learning
}
  \item{para.sup}{
list of additional parameters
}
}
\author{
Implementation of model formula by Pierre Lejeail, Maite Garcia & Jean-Luc Yongwe (2016)
based from Wrappers for performance enhancement and oblivious decision graphs by Ron Kohavi (1995).
}
\examples{
data("diabete")
head(diabete)
# We don't want the 6th and 7th variable as it is a factor
diabete <- diabete[, -c(6,7)]

# default the method svm and approch forward are applied
wrapper(diabete)

# We can select manually features to test abd target
# For example, with lda and backward
wrapper(diabete, learner="lda", target=7, features=1:6, algorithm="backward")


# Using own function with cross-validation
evalFUN <- function(features, ...) {  # The parameters must be the same
  # evry other variable you'll need to acces must be defined outside
  k_folds <- 8
  folds_i <- sample(rep(1:k_folds, length.out = nrow(diabete)))
  # Une validation
  error_rate <- function(k) {
    test_i <- which(folds_i == k)
    model <- e1071::svm(as.formula(paste("Outcome~", unpack(features))),
                               data=diabete[-test_i, ])
    prediction <- predict(model, diabete[test_i, ])
    tt <- table(diabete[test_i, "Outcome"], prediction)
    return(sum(tt[row(tt) != col(tt)]) / sum(tt))  # return the error rate
    # it can return another evaluation measure
    # if you want to return a measure which need to be maximized then multiply it with -1
  }
  # mean of all train/test sets generated by cross-validation
  return(mean(sapply(1:k_folds, error_rate)))
}

wrapped <- wrapper(dataset = diabete,
                   learner = evalFUN,
                   algorithm = "forward",
                   features = 1:6,
                   target = "Outcome"  # we can also select variables with names
)
}

\keyword{ wrapper }
\keyword{ wrap }
\keyword{ feature }
\keyword{ selection }
