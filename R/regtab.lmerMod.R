#' Regression tables for lmerMod models
#'
#' regtab method for models of class \code{lmerMod}.
#'
#' @param mod A model of class \code{lmerMod}.
#' @template format
#' @template style_options
#' @template ci_linear
#' @template ci_level
#' @template se_linear
#' @template teststatistic
#' @template df_lmer
#' @template pval
#' @template intercept
#' @template caption_mixed
#' @template n_caption
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @details If \code{pval = TRUE} then the package lmerTest is called.
#' @export
#'
#' @examples
#' bdf.lmerMod <- lme4::lmer(IQ.verb ~ sex + aritPOST + denomina +
#'   Minority + (1|schoolNR), data = nlme::bdf)
#' regtab(bdf.lmerMod)
regtab.lmerMod <- function(mod, format = "latex", style_options = list(),
                           ci = TRUE, ci_level = 0.95,
                           se = FALSE, teststatistic = FALSE, df = TRUE,
                           pval = TRUE, intercept = FALSE, caption = NULL,
                           n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                           digits = 3, ...) {
  if (pval) {
    coefsm <- stats::coef(summary(lmerTest::lmer(eval(mod@call[[2]]),
      data = eval(mod@call[[3]]))))
    colnames(coefsm) <- c("Estimate", "Std.Error", "df", "t-Value", "p-Value")
    inc.col <- c(1, which(c(se, df, teststatistic, pval) != 0) + 1)
    coefsm <- coefsm[, inc.col, drop = FALSE]
  } else {
    coefsm <- stats::coef(summary(mod))
    colnames(coefsm) <- c("Estimate", "Std.Error", "t-Value")
    inc.col <- which(c(estimate, se, teststatistic) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
  }

  if (ci) {
    estci <- stats::confint(mod, level = ci_level, ...)
    estci <- estci[(nrow(estci) - nrow(coefsm) + 1):nrow(estci), , drop = FALSE]
    coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
      "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
  }

  if (!intercept) coefsm <- coefsm[-1, , drop = FALSE]
  if (is.null(caption)) caption <- "Mixed Model Regression"

  if (pval) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (df & pval) coefsm[, "df"] <- round(coefsm[, "df"], 1)
  if (pval) coefsm[highsig, ncol(coefsm)] <- "<0.001"

  if (addref) {
    modframe <- mod@frame[, -c(1, ncol(mod@frame)- length(mod@flist) + 1:ncol(mod@frame)),
      drop = FALSE]
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
