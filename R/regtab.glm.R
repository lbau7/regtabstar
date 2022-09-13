#' Regression tables for glm models
#'
#' regtab method for models of class \code{glm}.
#'
#' @param mod A model of class \code{glm}.
#' @template or
#' @template logor
#' @template ci_or
#' @template ci_level
#' @template se_logor
#' @template vcov
#' @template teststatistic
#' @template pval
#' @template intercept
#' @template caption_glm
#' @template n_caption
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @details Models of class \code{glm} are currently only supported for
#'   logistic regression model.
#' @export
#'
#' @examples
#' iris$Sepal.Length <- ifelse(iris$Sepal.Length < median(iris$Sepal.Length), 0, 1)
#' iris.glm <- glm(Sepal.Length ~ Sepal.Width + Petal.Width + Species,
#'   data = iris,
#'   family = binomial
#'   )
#' regtab(iris.glm)
texmod.glm <- function(mod, or = TRUE, logor = FALSE, ci = TRUE, ci_level = 0.95,
                       se_logor = FALSE, vcov = NULL, teststatistic = FALSE,
                       pval = TRUE, intercept = FALSE, caption = NULL,
                       n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                      digits = 3, ...) {
  if (mod$family$family != "binomial") {
    stop("Only GLMs of family binomial are currently supported")
  }

  if (mod$family[2] != "logit") {
    stop("Only binomial regressions with logit-link are currently supported")
  }

  if (is.null(vcov)) {
    coefsm <- stats::coef(summary(mod))
    coefsm <- cbind(exp(coefsm[, 1]), coefsm)
    colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "z-Value",
      "p-Value")
    inc_col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc_col, drop = FALSE]

    if (ci & or) {
      estci <- exp(stats::confint(mod, level = ci_level, ...))
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
        "Upper CL" = estci[,2 ], coefsm[, -1, drop = FALSE])
    }
  } else {
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      stop("package lmtest must be installed to use this option")
    }
    coefsm <- lmtest::coeftest(mod, vcov. = vcov, ...)
    coefsm <- cbind(exp(coefsm[, 1]), coefsm)
    colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "z-Value",
      "p-Value")
    inc_col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc_col, drop = FALSE]

      if (ci & or) {
        estci <- exp(lmtest::coefci(mod, level = ci_level, vcov = vcov))
        coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
          "Upper CL" = estci[,2 ], coefsm[, -1, drop = FALSE])
      }
    }

  if (!intercept) coefsm <- coefsm[-1, , drop = FALSE]
  if (is.null(caption)) caption <- "Logistic Regression"

  if (pval) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval) coefsm[highsig, ncol(coefsm)] <- "<0.001"

  if (addref) {
    facrows <- sapply(stats::model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    facrows[1] <- FALSE
    if (sum(facrows) > 0) {
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

      if (intercept == FALSE) facvec <- facvec[-1]

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
