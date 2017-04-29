########===============================================================########
####                                                                       ####
##        wrapper function                                                   ##
#             this is the main function code                                  #
###                                                                         ###
########===============================================================########
#
# Package shortcuts infos :
# Build and Reload Package:  'Ctrl + Shift + B'
# Check Package:             'Ctrl + Shift + E'
# Test Package:              'Ctrl + Shift + T'
#
#
# Main function wrapper
# INPUT
  # dataset   :  the dataset we want to learn from
  # learner   :  the choosen method used to learn the model
                 # (logit, lm, lda, qda, svm, naiveBayes, randomForest)
                 # or a custom function (must return a measure to minimize)
  # algorithm :  the approach used by the wrapper
                 # (forward, backward, genetic)
  # train     :  the size (or proportion) of the training set
  # target    :  the variable we want to learn (default: 1st column)
  # features  :  the variables we want to learn from (default: all but target)
  # plot.err: :  boolean indicating whether to plot or no the error rate for all
                 # the tested models
  # ...       :  supplementary parameters for the classifier
# OUTPUT
  # wrapped   :  S3 object (~= list) containing the historic of models, the
                 # historic of error rate, the best formula and some parameters
                 # as which learner and which algorithm was used
#
#####======================================================================####
wrapper <- function(
              dataset,
              learner="svm",
              train=0.7,
              algorithm="forward",
              k.lim=2,
              target=NULL,
              features=NULL,
              plot.err=TRUE,
              ...
              )
  {
  # Check integrity of the values --------------------------------------------#
  ## Raise an error if dataset is not data.frame or matrix.
  if (is.matrix(dataset)) {
    # Conversion as matrix for formulas construction as a dataframe for features
    # doesn't work
    dataset = as.data.frame(dataset)
  } else if (!is.data.frame(dataset)) {
    stop("dataset must be a matrix or a data.frame")
  }
  ## Raise an error if dataset has only one column
  if (!ncol(dataset) >= 2)  {
    stop("dataset must have at least 2 columns")
  }
  ## Raise an error if learner unknown
  if (!is.function(learner)) {
    if (!learner %in% c("logit", "lm", "lda", "qda", "svm", "naiveBayes",
                        "randomForest")) {
      stop("learner unknown, see the doc for the list of admitted values")
    }
  }
  ## Set default target (if target == NULL)
  if (is.null(target)) {
    target <- ncol(dataset)
  } else {
    ## Raise an error if target is out of bonds
    tryCatch(dataset[,target],
             error = function(e) {
               stop("dataset[, target] : undefined columns selected")
             })
    ## Raise an error if target is a multidimensional vector
    if (!is.null(ncol(dataset[,target]))) {
      stop("target must be a 1 dimension vector")
    }
  }
  if (is.null(features)) {
    features <- 1:(ncol(dataset) - 1)
  } else {
    ## Raise an error if features is out of bonds
    tryCatch(dataset[, features],
             error = function(e) {
               stop("dataset[, features] : undefined columns selected")
             })
  }
  ## if target/features are numeric, get correspondings colnames
  if (is.numeric(target)) target <- colnames(dataset)[target]
  if (is.numeric(features)) features <- colnames(dataset)[features]

  ## Raise an error if algorithm unknown
  if (!algorithm %in% c("forward", "backward", "genetic")) {
    stop("algorithm unknown, see the doc for the list of admitted values")
  }
  ## Raise an error if train is not a number beetween 0 and n - 1 or a vector
  if (length(train) == 1 && (train < 0 || train > (nrow(dataset) - 1))) {
    stop("train.size must be either a probability (beetween 0 and 1)
          or the number of rows used for training")
  }
  # as in R everything but a function can act as a boolean, we assume that the
  # user will not put a function body and don't check that parameter

  # Settup -------------------------------------------------------------------#
  ## Creation of training and test samples
  if (length(train) == 1) {
    if (train < 1) {
      train = round(nrow(dataset) * train, 0)
    }
    train = sample(nrow(dataset), size=train)
  }


  ## Construct the learner function .methods.R
  wrap.learner <- .choose.classifier(learner, target, dataset, train)
  ## Get the right wrapper
  wrap.algorithm <- .choose.wrapper(algorithm)

  # Wrapper execution and display --------------------------------------------#
  wrapped <- wrap.algorithm(features, wrap.learner, k.lim, ...)

  ## Compute wrapper and save results in a formatted list
  wrapped <- .obj.builder(algorithm=algorithm, target=target, learner=learner,
                          train=train, wrapped[[1]], wrapped[[2]],
                          wrapped[[3]], ...)

  ## plot the results
  if (plot.err) {
    plot(1:length(wrapped$hist.errors),
         wrapped$hist.errors,
         col="slateblue", pch=19,
         main="Error rate curve", xlab="tested model",
         ylab="error rate", type="l")
  }

  ## return the wrapper results
  return(wrapped)
}
