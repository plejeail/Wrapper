########===============================================================########
####                                                                       ####
##          Genetic Algorithm                                                ##
#                                                                             #
########===============================================================########



# NB: k.lim is unused but still here because all the wrappers must have the
# same interface
.wrap.gen <- function(features = features , wrap.learner = wrap.learner, k.lim, ...)
 {
  plot.err <<- FALSE
  evalFunc <- function(x)
  {
    err.curr=wrap.learner(features[x==1])
    if(err.curr > 0.3) {
      return(1)
    } else return(err.curr)
  }
  GAmodel <- genalg::rbga.bin(size = length(features), iters = 20,
                              mutationChance = 0.01, elitism = T,
                              evalFunc = evalFunc)
  res.curr <- summary(GAmodel)

  #retrieve of the chosen features
  res.curr <- strsplit(res.curr , '\n')[[1]][13]
  res.curr <- substr(res.curr , 19 , nchar(res.curr) - 1)
  res.curr <- unlist(strsplit(res.curr , ' '))
  res.curr <- as.numeric(res.curr)

  #retrieval function
  return(list(feats.use=features[res.curr == 1],
         hist.feats=NULL,
         hist.err=NULL
         )
  )
}
