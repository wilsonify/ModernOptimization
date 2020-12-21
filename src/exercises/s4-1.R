# steepest ascent hill climbing method:
hclimbing <- function(par, fn, change, lower, upper, control,
                      type = "min", ...)
{ fpar <- fn(par, ...)
  for (i in 1:control$maxit)
  {
    par1 <- change(par, lower, upper)
    fpar1 <- fn(par1, ...)
    if (control$N > 0) # steepest ascent code
    { for (j in 1:control$N - 1)
    { cand <- change(par, lower, upper)
      fcand <- fn(cand, ...)
      if ((type == "min" && fcand < fpar1)
        || (type == "max" && fcand > fpar1))
      { par1 <- cand; fpar1 <- fcand }
    }
    }
    if (control$REPORT > 0 && (i == 1 || i %% control$REPORT == 0))
      cat("i:", i, "s:", par, "f:", fpar, "s'", par1, "f:", fpar1, "\n")
    if ((type == "min" && fpar1 < fpar)
      || (type == "max" && fpar1 > fpar)) { par <- par1; fpar <- fpar1 }
  }
  if (control$REPORT >= 1) cat("best:", par, "f:", fpar, "\n")
  return(list(sol = par, eval = fpar))
}
