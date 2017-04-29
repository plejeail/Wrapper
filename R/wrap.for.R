########===============================================================########
####                                                                       ####
##          Forward                                                          ##
#                                                                             #
########===============================================================########




.wrap.for <- function(features, wrap.learner, k.lim, ...) {
  ## Avoid recursivity limits:
  # we store the option about limit of recursive calls :
  old.exp <- options()["expressions"]
  if (length(features) > old.exp) {
    # we set recursion limits to the number of features :
    options(expressions = length(features))
  }
  forward.search <- function(f, X, ...) {
    # see .wrap.learner.R for more informations
    return(wrap.learner(c(X, f), ...))
  }

  forward <- function(feats.use, feats.set, err.list, mod.list, err.curr, k=1) {
    err.min <- which.min(err.curr)
    if (min(err.list) > err.curr[err.min] && length(feats.set) > 0) {
      ## if no minimum is reach and we have not an empty set of features
      ## we continue recursion
      forward(# update set used features
              feats.use= c(feats.use, feats.set[err.min]),
              # updated set of unused features
              feats.set= feats.set[-err.min],
              # updated error list
              err.list = c(err.list, err.curr[err.min]), #
              # historic of models tested
              mod.list = append(mod.list, list(c(feats.use, feats.set[err.min]))),
              # error rates for the candidates of next step
              err.curr = sapply(feats.set[-err.min], forward.search,
                                c(feats.use, feats.set[err.min]), ...)
      )
    } else {
      ## While there is less than k.lim steps done without a better features
      ## subset
      if (k < k.lim && length(feats.set) > 0) {
        forward(feats.use= c(feats.use, feats.set[err.min]),
                feats.set= feats.set[-err.min],
                err.list = c(err.list, err.curr[err.min]),
                mod.list = append(mod.list, list(c(feats.use, feats.set[err.min]))),
                err.curr = sapply(feats.set[-err.min], forward.search,
                                  c(feats.use, feats.set[err.min]), ...),
                k=k+1
        )
      } else {
        # All possibilities have been tested or a minimum has been find and
        # unbeaten for k.lim iterations
        hist.feat <- append(mod.list, list(c(feats.use, feats.set[err.min])))
        hist.err  <- c(err.list, err.curr[err.min])[-1]
        return(
          list(best.feats= hist.feat[[which.min(hist.err)]],
               hist.feat = hist.feat,
               hist.err  = unlist(hist.err)
          )
        )
      }
    }
  }

  # Launch the recursivity with no used features
  result <- forward(c(), features, Inf, c(),
                    sapply(features, forward.search, c(), ...))

  # we go back to the previous settings for recursivity
  if (length(features) > old.exp) options(expressions=old.exp)
  return(result)
}
