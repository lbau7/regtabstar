#' Regression tables for mira model matrices
#'
#' regtab method for a matrix of models of class \code{mira}.
#'
#' @param mod A matrix of models of class \code{mira}, usually generate by \code{mice::with.mids}.
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
#' @param n_caption Whether the sample size and the number of imputed data sets that were
#'  used to compute the model should be added to the caption of the table.
#' @template rowlabs
#' @param rowlabs_auto data frame used for automatically labeling rows using the
#' \code{"label"}-attributes of the columns
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @details Models of class \code{mira} are currently only supported for
#'   logistic regression models fit with \code{glm}.
#' @export
#'
#' @examples
#' library(mice)
#' # Basic example taken from the MICE documentation
#' imp <- mice(nhanes2, m = 2, print = FALSE, seed = 14221)
#' mod <- with(imp, glm(hyp ~ age + chl, family = binomial))
#' regtab(mod)
#' # Example for the use of labels
#' data <- nhanes2
#' attr(data$age, "label") <- "Age [years]"
#' attr(data$age, "label") <- "BMI [kg/m^2]"
#' attr(data$hyp, "label") <- "Hypertension"
#' attr(data$chl, "label") <- "Total serum cholesterol [mg/dl]"
#' imp2 <- mice(data, m = 2, print = FALSE, seed = 14221)
#' mod2 <- with(imp2, glm(hyp ~ age + chl, family = binomial))
#' regtab(mod2, rowlabs_auto = data)
#'

regtab.mira <- function(mod, format = "latex", style_options = list(),
                        or = TRUE, logor = FALSE, ci = TRUE, ci_level = 0.95,
                        se_logor = FALSE, vcov = NULL, teststatistic = FALSE,
                        pval = TRUE, intercept = FALSE, caption = NULL,
                        n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                        digits = 3, rowlabs_auto = NULL,
                        ...) {
  mod1 <- mod$analyses[[1]]

  if (!("glm" %in% class(mod1))){
    stop("Only GLMs are currently supported")
  }

  if (mod1$family$family != "binomial") {
    stop("Only GLMs of family binomial are currently supported")
  }

  if (mod1$family$link != "logit") {
    stop("Only binomial regressions with logit-link are currently supported")
  }

  if (is.null(vcov)) {
    pooled <- mice::pool(mod)
    coefsm <- summary(pooled)[, c("estimate", "std.error", "statistic", "p.value")]
    coefsm <- cbind(exp(coefsm[, 1]), coefsm)
    colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "statistic",
      "p-Value")
    rownames(coefsm) <- summary(pooled)[, "term"]
    inc_col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc_col, drop = FALSE]

    if (ci & or) {
      estci <- exp(summary(pooled, conf.int = TRUE, conf.level = ci_level)[, c(7, 8)])
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
      faclevs <- sapply(stats::model.frame(mod1),
                        function(x) levels(x)[1])[facrows]

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
      facrlabs <- paste0(names(faclevs), faclevs)

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
  if (!is.null(rowlabs_auto) & is.null(rowlabs)) {
    # By default, rowlabs as usual
    rowlabs <- rownames(coefsm)
    # Search for covariates with label in the data set rowlabs_auto
    # and replace the variable name in the respective rows
    covar_names <- names(stats::model.frame(mod1))[-1]
    covar_classes <- sapply(stats::model.frame(mod1), class)
    i <- 1
    if(intercept) i <- 2
    for(name in covar_names){
      label <- attr(rowlabs_auto[[name]], "label")
      j <- 1
      if(covar_classes[name] %in% c("factor", "ordered")){
        j <- length(levels(stats::model.frame(mod1)[,name]))
        if(!addref){
          j <- j - 1
        }
      }
      if(!is.null(label)) {
        rowlabs[(i:(i+j-1))] <-
          paste0(gsub(pattern = paste0("^", name),
               replacement = paste(label,"("),
               x = rowlabs[(i:(i+j-1))]), ")")

      }
      i <- i + j
    }
    # Remove empty brackets
    rowlabs <- gsub(pattern = " \\(\\)$", replacement = "", rowlabs)
  }

  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  if (n_caption) {
    caption <- paste0(caption, " (n subjects = ", nrow(stats::model.frame(mod1)),
                      ", n imputations = ", pooled$m,")")
  }

  out <- kableExtra::kbl(coefsm, format = format, booktabs = TRUE,
    caption = caption, ...)

  if (length(style_options) > 0) {
    do.call(kableExtra::kable_styling, c(list(out), style_options))
  } else {
    out
  }
}
