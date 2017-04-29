########===============================================================########
####                                                                       ####
##          Backward                                                         ##
#                                                                             #
########===============================================================########




.wrap.back <- function(features, wrap.learner, k.lim, ...) {
  ## Avoid recursivity limits:
  # we store the option about limit of recursive calls :
  old.exp <- options()["expressions"]
  if (length(features) > old.exp) {
    # we set recursion limits to the number of features :
    options(expressions = length(features))
  }
  back.search <- function(f, X, ...) {
    # see .wrap.learner.R for more informations
    return(wrap.learner(X[X!=f], ...))
  }

  backward <- function(feats.use, err.list, mod.list, err.curr, k=1) {
    err.min <- which.min(err.curr)
    if (min(err.list) > err.curr[err.min] &&
        length(feats.use) > 2) {
      ## if no minimum is reach and we have not an empty set of features
      ## we continue recursion
      feats.use <- feats.use[-err.min]
      backward(# update set used features
              feats.use= feats.use,
              # updated error list
              err.list = c(err.list, err.curr[err.min]),
              # historic of models tested
              mod.list = append(mod.list, list(feats.use)),
              # error rates for the candidates of next step
              err.curr = sapply(feats.use, back.search, feats.use, ...)
      )
    } else {
      ## While there is less than k.lim steps done whithout a better features
      ## subset
      if (k < k.lim && length(feats.use) > 2) {
        feats.use <- feats.use[-err.min]
        backward(feats.use= feats.use,
                err.list = c(err.list, err.curr[err.min]),
                mod.list = append(mod.list, list(feats.use)),
                err.curr = sapply(feats.use, back.search, feats.use, ...),
                k=k+1
        )
      } else {
        # All possibilities have been tested or a minimum has been find and
        # unbeaten for k.lim iterations
        hist.feat <- append(mod.list, list(feats.use[-err.min]))
        hist.err  <- c(err.list, err.curr[err.min])
        return(
          list(best.feats= hist.feat[[which.min(hist.err)]],
               hist.feat = hist.feat,
               hist.err  = hist.err
          )
        )
      }
    }
  }

  # Launch the recursivity with no used features
  result <- backward(features, wrap.learner(features),
                     list(features), sapply(features, back.search, features, ...))

  # we go back to the previous settings for recursivity
  if (length(features) > old.exp) options(expressions=old.exp)
  return(result)
}
