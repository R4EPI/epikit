#nocov start
.onLoad <- function() {
  if (as.integer(R.version$major) < 4 && as.integer(R.version$minor) < 4.0) {
    withAutoprint <- function (exprs, evaluated = FALSE, local = parent.frame(), print. = TRUE, 
      echo = TRUE, max.deparse.length = Inf, width.cutoff = max(20, 
        getOption("width")), deparseCtrl = c("keepInteger", "showAttributes", 
        "keepNA"), ...) 
    {
      if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
          if (exprs[[1]] == quote(`{`)) 
            exprs <- as.list(exprs[-1])
        }
      }
      source(exprs = exprs, local = local, print.eval = print., 
        echo = echo, max.deparse.length = max.deparse.length, 
        width.cutoff = width.cutoff, deparseCtrl = deparseCtrl, 
        ...)
    }
    delayedAssign("withAutoprint", withAutoprint)
  }
}
#nocov end
