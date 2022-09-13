#' Regression tables for lm models
#'
#' regtab method for models of class \code{lm}.
#'
#' @param mod A model of class \code{lm}.
#' @template format
#' @template style_options
#' @template ci_linear
#' @template ci_level
#' @template se_linear
#' @template vcov
#' @template teststatistic
#' @template pval
#' @template intercept
#' @template caption
#' @template n_caption
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @export
#'
#' @examples
#' iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Species, data = iris)
#' regtab(iris.lm)
regtab.lm <- function(mod, format = "latex", style_options = list(),
                      ci = TRUE, ci_level = 0.95, se = FALSE, vcov = NULL,
                      teststatistic = FALSE, pval = TRUE,
                      intercept = FALSE, caption = NULL, n_caption = TRUE,
                      rowlabs = NULL, addref = TRUE, digits = 3, ...) {
  if (is.null(vcov)) {
    coefsm <- stats::coef(summary(mod))
    colnames(coefsm) <- c("Estimate", "Std.Error", "t-Value", "p-Value")
    inc_col <- c(1, which(c(se, teststatistic, pval) != 0) + 1)
    coefsm <- coefsm[, inc_col, drop = FALSE]

    if (ci) {
      estci <- stats::confint(mod, level = ci_level, ...)
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
        "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
    }
  } else {
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      stop("package lmtest must be installed to use this option")
    }
    coefsm <- lmtest::coeftest(mod, vcov. = vcov, ...)
    colnames(coefsm) <- c("Estimate", "Std.Error", "t-Value", "p-Value")
    inc_col <- c(1, which(c(se, teststatistic, pval) != 0) + 1)
    coefsm <- coefsm[, inc_col, drop = FALSE]

    if (ci) {
      estci <- lmtest::coefci(mod, level = ci_level, vcov. = vcov, ...)
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
        "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
    }
  }

  if (!intercept) coefsm <- coefsm[-1, , drop = FALSE]
  if (is.null(caption)) caption <- "Linear Model"
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"

  if (addref) {
    facrows <- sapply(stats::model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    faclevs <- sapply(stats::model.frame(mod)[, facrows],
      function(x) levels(x)[1])
    facrlabs <- paste0(names(faclevs), faclevs)

    facvec <- numeric()
    for(i in 1:length(facrows)) {
      if (facrows[i] == FALSE) {
        facvec <- c(facvec, FALSE)
      } else {
        facvec <- c(facvec, TRUE, rep(FALSE,
          length(levels(stats::model.frame(mod)[, i])) - 2))
      }
    }

    if (!intercept) facvec <- facvec[-1]

    emptyrow <- c(0, rep(".", (ncol(coefsm) - 1)))
    newrowpos <- grep(1, facvec)
    j <- 0
    for(i in 1:sum(facrows)) {
      if (newrowpos[i] == 1) {
        coefsm <- rbind(emptyrow, coefsm)
        rownames(coefsm)[1] <- facrlabs[i]
      } else {
        coefsm <- rbind(coefsm[1:(newrowpos[i] + j - 1), , drop = FALSE],
          emptyrow,
          coefsm[(newrowpos[i] + j):nrow(coefsm), , drop = FALSE])
        rownames(coefsm)[newrowpos[i] + j] <- facrlabs[i]
      }
      j <- j + 1
    }
  }

  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  if (n_caption) {
    caption <- paste0(caption, " (n = ", nrow(stats::model.frame(mod)), ")")
  }

  out <- kableExtra::kbl(coefsm, format = format, booktabs = TRUE,
    caption = caption, ...)

  if (length(style_options) > 0) {
    do.call(kableExtra::kable_styling, c(list(out), style_options))
  } else {
    out
  }
}
