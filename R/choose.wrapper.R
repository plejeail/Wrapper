########===============================================================########
####                                                                       ####
##          choose.wrapper return the chosen approach                        ##
#                                                                             #
########===============================================================########




.choose.wrapper <- function(approach) {
  if (approach == "forward") {
    return(.wrap.for)
  }
  if (approach == "backward") {
    return(.wrap.back)
  }
  if (approach == "genetic") {
    return(.wrap.gen)
  }
}
