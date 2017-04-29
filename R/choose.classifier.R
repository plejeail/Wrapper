########================================================================########
####                                                                        ####
##        choose.classifier                                                   ##
#             wrap learning methods so they have the same interface            #
###                                                                          ###
########================================================================########




# unpack a vector v as a string of concatenated values, separated by sep
# (useful for as.formula)
unpack <- function(v, separator=' + ') paste(v, collapse=separator)

# The interface of the learner function is
.choose.classifier <- function(learner, target, X, train) {
  # to be saved in the environment of the returned function and used everytime
  # without the need of beeing sent as parameters

  # Function are written with the following template:
  # function(target, features, x) { # the argument must always be the same and
                                    # with the same order
    # comppute model
      # (for a formula, use: as.formula(paste(target, "~", unpack(features)))
    # calculate error rate  # an other measure can be used, as long it need to
                            # be minimized
      # for the error rate, you can use the same code as us
        # prediction <- predict(model, X[-train, ], type = 'response') > 0.5
        # tt <- table(X[-train, target], prediction)
        # error <- sum(tt[row(tt) != col(tt)]) / sum(tt)
      # this work for any method that can use predict()
    # return the mesure
  if (is.function(learner)) {
    return(learner)
  }
  if (learner == "logit") {
    return(function(features, ...) {
        model <- glm(as.formula(paste(target, "~", unpack(features))),
                     data=X, subset=train, family="binomial", ...)
        prediction <- predict(model, X[-train, ], type = 'response') > 0.5
        tt <- table(X[-train, target], prediction)
        return(sum(tt[row(tt) != col(tt)]) / sum(tt))
      }
    )
  }
  if (learner == "lm") {
    return(function(features, ...) {
      return(BIC(lm(as.formula(paste(target, "~", unpack(features))),
                    data=X, ...)))
    }
    )
  }
  if (learner == "lda") {
    return(function(features, ...) {
        model <- MASS::lda(as.formula(paste(target, "~", unpack(features))),
                     data=X, subset=train, ...)
        prediction <- predict(model, X[-train, ])$class
        tt <- table(X[-train, target], prediction)
        return(sum(tt[row(tt) != col(tt)]) / sum(tt))
      }
    )
  }

  if (learner == "qda") {
    return(function(features, ...) {
        model <- MASS::qda(as.formula(paste(target, "~", unpack(features))),
                     data=X, subset=train, ...)
        prediction <- predict(model, X[-train, ])$class
        tt <- table(X[-train, target], prediction)
        return(sum(tt[row(tt) != col(tt)]) / sum(tt))
      }
    )
  }
  if (learner == "svm") {
    return(function(features, ...) {
        model <- e1071::svm(as.formula(paste(target, "~", unpack(features))),
                     data=X, subset=train, ...)
        prediction <- predict(model, X[-train, ])
        tt <- table(X[-train, target], prediction)
        return(sum(tt[row(tt) != col(tt)]) / sum(tt))
      }
    )
  }
  if (learner == "naiveBayes") {
    return(function(features, ...) {
        model <- e1071::naiveBayes(as.formula(paste(target, "~", unpack(features))),
                            data=X, subset=train, ...)
        prediction <- predict(model, X[-train, ])
        tt <- table(X[-train, target], prediction)
        return(sum(tt[row(tt) != col(tt)]) / sum(tt))
      }
    )
  }
  if (learner == "randomForest") {
    return(function(features, ...) {
        model <- randomForest::randomForest(as.formula(paste(target, "~", unpack(features))),
                              data=X, subset=train, ...)
        prediction <- predict(model, X[-train, ])
        tt <- table(X[-train, target], prediction)
        return(sum(tt[row(tt) != col(tt)]) / sum(tt))
      }
    )
  }
}
