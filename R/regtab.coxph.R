#' Regression tables for coxph models
#'
#' regtab method for models of class \code{coxph}.
#'
#' @param mod A model of class \code{coxph}.
#' @template format
#' @template style_options
#' @template hr
#' @template loghr
#' @template ci_hr
#' @template ci_level
#' @template se_loghr
#' @template teststatistic
#' @template pval
#' @template caption_cox
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
#' lung <- survival::lung
#' lung.coxph <- survival::coxph(survival::Surv(time, status) ~
#'   age + factor(sex) + meal.cal + ph.karno, data = lung)
#' regtab(lung.coxph)
regtab.coxph <- function(mod, format = "latex", style_options = list(),
                         hr = TRUE, loghr = FALSE, ci = TRUE,
                         ci_level = 0.95, se_loghr = FALSE,
                         teststatistic = FALSE, pval = TRUE, caption = NULL,
                         n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                         digits = 3, ...) {
  sm <- summary(mod, conf.int = ci_level)
  coefsm <- stats::coef(sm)[, c(2, 1, 3:5), drop = FALSE]
  colnames(coefsm) <- c("Hazard Ratio", "log HR", "SE (log HR)", "z-Value",
    "p-Value")
  inc_col <- which(c(hr, loghr, se_loghr, teststatistic, pval) != 0)
  coefsm <- coefsm[, inc_col, drop = FALSE]

  if (ci & hr) {
    ci_low <- sm$conf.int[,3]
    ci_up <- sm$conf.int[,4]
    coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = ci_low,
      "Upper CL" = ci_up, coefsm[, -1, drop = FALSE])
  }

  if (is.null(caption)) caption <- "Cox Regression"

  if (pval) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval) coefsm[highsig, ncol(coefsm)] <- "<0.001"

  if (addref) {
    facrows <- sapply(stats::model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    faclevs <- sapply(stats::model.frame(mod)[,facrows],
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

    facvec <- facvec[-1]
    if (sum(facvec != 0)) {
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
