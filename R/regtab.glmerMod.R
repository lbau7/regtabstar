#' Regression tables for glmerMod models
#'
#' regtab method for models of class \code{glmerMod}.
#'
#' @param mod A model of class \code{glmerMod}.
#' @template format
#' @template style_options
#' @template or
#' @template logor
#' @template ci_or
#' @template ci_level
#' @template se_logor
#' @template teststatistic
#' @template pval
#' @template intercept
#' @template caption_glmmixed
#' @template n_caption
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @details Models of class \code{glmerMod} are currently only supported for
#'   logistic mixed model regressions.
#' @export
#'
#' @examples
#' bdf <- nlme::bdf
#' bdf$IQ.verb <- ifelse(bdf$IQ.verb < median(bdf$IQ.verb), 0, 1)
#' bdf.glmerMod <- lme4::glmer(IQ.verb ~ sex + aritPOST + Minority +
#'    (1|schoolNR), data = bdf, family = "binomial")
#' regtab(bdf.glmerMod)
regtab.glmerMod <- function(mod,  format = "latex", style_options = list(),
                            or = TRUE, logor = FALSE, ci = TRUE,
                            ci_level = 0.95, se_logor = FALSE,
                            teststatistic = FALSE, pval = TRUE,
                            intercept = FALSE, caption = NULL,
                            n_caption = TRUE, rowlabs = NULL,
                            addref = TRUE, digits = 3, ...) {
  if (mod@resp$family$family != "binomial") {
    stop("Only GLMs of family binomial are currently supported")
  }

  if (mod@resp$family$link != "logit") {
    stop("Only binomial regressions with logit-link are currently supported")
  }

  coefsm <- stats::coef(summary(mod))
  coefsm <- cbind(exp(coefsm[, 1]), coefsm)
  colnames(coefsm) <- c("Odds Ratios", "log OR", "SE (log OR)", "z-Value", "p-Value")
  inc.col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
  coefsm <- coefsm[, inc.col, drop = FALSE]

  if (ci & or) {
    estci <- stats::confint(mod, level = ci_level, method = "Wald")
    estci <- exp(estci)
    estci <- estci[(nrow(estci) - nrow(coefsm) + 1):nrow(estci), , drop = FALSE]
    coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
      "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
  }

  if (!intercept) coefsm <- coefsm[-1, , drop = FALSE]
  if (is.null(title)) title <- "Logistic Mixed Model Regression"

  if (pval) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval) coefsm[highsig, ncol(coefsm)] <- "<0.001"

  if (addref) {
    modframe <- mod@frame[, -c(1, ncol(mod@frame)), drop = FALSE]
    facrows <- sapply(modframe, class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    faclevs <- sapply(modframe[,facrows], function(x) levels(x)[1])
    facrlabs <- paste0(names(faclevs), faclevs)

    facvec <- numeric()
    for(i in 1:length(facrows)) {
      if (facrows[i] == FALSE) {
        facvec <- c(facvec, FALSE)
      } else {
        facvec <- c(facvec, TRUE, rep(FALSE, length(levels(modframe[, i])) - 2))
      }
    }

    if (intercept) facvec <- c(0, facvec)

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
