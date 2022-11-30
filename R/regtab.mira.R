#' Regression tables for mira model matrices
#'
#' regtab method for a matrix of models of class \code{mira}.
#'
#' @param mod A matrix of models of class \code{mira}.
#' @template format
#' @template style_options
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
#' @details Models of class \code{mira} are currently only supported for
#'   logistic regression model.
#' @export
#'
#' @examples
#' imp <- mice(nhanes2, m = 2, print = FALSE, seed = 14221)
#' fit1 <- with(imp, lm(bmi ~ age + hyp + chl))
#' regtab(fit1)
regtab.mira <- function(mod, format = "latex", style_options = list(),
                       or = TRUE, logor = FALSE, ci = TRUE, ci_level = 0.95,
                       se_logor = FALSE, vcov = NULL, teststatistic = FALSE,
                       pval = TRUE, intercept = FALSE, caption = NULL,
                       n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                      digits = 3, ...) {
  if (mod$analyses[[1]]$family$family != "binomial") {
    stop("Only GLMs of family binomial are currently supported")
  }

  if (mod$analyses[[1]]$family$link != "logit") {
    stop("Only binomial regressions with logit-link are currently supported")
  }

  mod1 <- mod$analyses[[1]]

  if (is.null(vcov)) {
    pooled <- mice::pool(fit)
    coefsm <- summary(pooled)[, c("estimate", "std.error", "statistic", "p.value")]
    coefsm <- cbind(exp(coefsm[, 1]), coefsm)
    colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "statistic",
      "p-Value")
    inc_col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc_col, drop = FALSE]

    if (ci & or) {
      estci <- exp(summary(pooled, conf.int = TRUE, conf.level = ci_level)[7, 8])
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
        "Upper CL" = estci[,2 ], coefsm[, -1, drop = FALSE])
    }
  } else {
      stop("currently only vcov = NULL is supported")
  }

  if (!intercept) coefsm <- coefsm[-1, , drop = FALSE]
  if (is.null(caption)) caption <- "Pooled estimates of logistic regression estimates on multiply imputed data sets"

  if (pval) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval) coefsm[highsig, ncol(coefsm)] <- "<0.001"

  if (addref) {
    facrows <- sapply(stats::model.frame(mod1), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    facrows[1] <- FALSE
    if (sum(facrows) > 0) {
      faclevs <- sapply(stats::model.frame(mod1)[, facrows],
        function(x) levels(x)[1])
      facrlabs <- paste0(names(faclevs), faclevs)

      facvec <- numeric()
      for(i in 1:length(facrows)) {
        if (facrows[i] == FALSE) {
          facvec <- c(facvec, FALSE)
        } else {
          facvec <- c(facvec, TRUE, rep(FALSE,
            length(levels(stats::model.frame(mod1)[, i])) - 2))
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
    caption <- paste0(caption, " (n = ", nrow(stats::model.frame(mod1)), ")")
  }

  out <- kableExtra::kbl(coefsm, format = format, booktabs = TRUE,
    caption = caption, ...)

  if (length(style_options) > 0) {
    do.call(kableExtra::kable_styling, c(list(out), style_options))
  } else {
    out
  }
}
