####                                                                           #
##        .wrap.obj creation of a list with a special show method              #
#                                                                            ###
########================================================================########



# create the object the wrapper will return
.obj.builder <- function(algorithm,
                        learner,
                        target,
                        train,
                        features,
                        hist.mods,
                        hist.errors,
                        ...) {
  # Get statistics for the current model
  wrap.list <- list(algorithm = algorithm,
                    classifier = learner,
                    target = target,
                    formula = as.formula(paste(target, "~", unpack(features))),
                    best.feats = features,
                    hist.mods = hist.mods,
                    hist.errors = hist.errors,
                    error.rate = min(hist.errors),
                    train.sample = train,
                    para.sup = list(...)
    )
  wrap.list <- structure(wrap.list, class="wrapper")
  return(wrap.list)
}


# print() and show() methods for a wrapper object
print.wrapper <- function(x, ...){
  cat("##  Wrapper: ")
  cat(x$algorithm)
  cat("  ##\n\n")
  cat("Call:\n")
  if (length(x$para.sup) > 0) {
    par <- paste(", ", paste(paste(names(x$para.sup), "=", x$para.sup,sep="")
                             , sep=","), sep="")
  } else par <- ""
  cat(paste(x$classifier, "(", paste(x$target, "~",
                                          unpack(x$best.feats)),
            ", data = dataset, subset = train", par, ")", sep=""))
  cat("\n\nerror rate: ")
  cat(x$error.rate)
  cat("\nprecision: ")
  cat(1 - x$error.rate)
  cat("\ntested models: ")
  cat(length(x$hist.mods))
  cat("\n\nnames : \n")
  cat("algorithm, classifier, formula, best.feats, hist.feats, hist.errors, train",
      "error.rate, train.sample", sep="")
}

