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
#' @template rowlabs_auto
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
#' # Example taken from the MICE documentation
#' library(mice)
#'
#' imp <- mice::mice(nhanes2, m = 2, print = FALSE, seed = 14221)
#' mod <- mice:::with.mids(imp, glm(hyp ~ age + chl, family = binomial))
#' regtab(mod)
#' # Example for the use of labels
#' data <- nhanes2
#' attr(data$age, "label") <- "Age [years]"
#' attr(data$age, "label") <- "BMI [kg/m^2]"
#' attr(data$hyp, "label") <- "Hypertension"
#' attr(data$chl, "label") <- "Total serum cholesterol [mg/dl]"
#' imp2 <- mice::mice(data, m = 2, print = FALSE, seed = 14221)
#' mod2 <- mice:::with.mids(imp2, glm(hyp ~ age + chl, family = binomial))
#' regtab(mod2, rowlabs_auto = data)
#'
#' # Example for polr
#' data(housing, package="MASS")
#' housing_amp <- mice::ampute(housing)$amp
#' housing_amp$Sat <- ordered(housing_amp$Sat,
#'                            levels = (1:3),
#'                            labels = levels(housing$Sat))
#' housing_amp$Infl <- factor(housing_amp$Infl,
#'                            levels = (1:3),
#'                            labels = levels(housing$Infl))
#' housing_amp$Type <- factor(housing_amp$Type,
#'                            levels = (1:4),
#'                            labels = levels(housing$Type))
#' housing_amp$Cont <- factor(housing_amp$Cont,
#'                            levels = (1:2),
#'                            labels = levels(housing$Cont))
#' attr(housing_amp$Sat, "label") <- "Satisfaction"
#' attr(housing_amp$Infl, "label") <- "Degree of influence"
#' attr(housing_amp$Type, "label") <- "Type"
#' attr(housing_amp$Cont, "label") <- "Contact with other residents"
#' imp3 <- mice::mice(housing_amp, m = 2, print = FALSE, seed = 14221)
#' mod3 <- mice:::with.mids(imp3,
#'                          MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq,
#'                                     Hess = TRUE))
#' regtab(mod3, rowlabs_auto = housing_amp)
#'
regtab.mira <- function(mod, format = "latex", style_options = list(),
                        or = TRUE, logor = FALSE, ci = TRUE, ci_level = 0.95,
                        se_logor = FALSE, vcov = NULL, teststatistic = FALSE,
                        pval = TRUE, intercept = FALSE, caption = NULL,
                        n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                        digits = 3, rowlabs_auto = NULL,
                        ...) {
  mod1 <- mod$analyses[[1]]
  modclass <- ""

  if (("glm" %in% class(mod1))){
    if (mod1$family$family != "binomial") {
      stop("Only GLMs of family binomial are currently supported")
    }

    if (mod1$family$link != "logit") {
      stop("Only binomial regressions with logit-link are currently supported")
    }
    modclass <- "glm"
    if (is.null(caption)) caption <- "Pooled estimates of logistic regression estimates on multiply imputed data sets"
  } else if (("polr" %in% class(mod1))) {
    if (mod1$method != "logistic") {
      stop("Only logistic proportional odds models are currently supported")
    }
    modclass <- "polr"
    if (is.null(caption)) caption <- "Pooled estimates of ordinal logistic regression estimates on multiply imputed data sets"
  } else {
    stop("Only mira object for GLMs or for POLRs are currently supported")
  }


  if (is.null(vcov)) {
    pooled <- mice::pool(mod)
    coefsm <- mice:::summary.mipo(pooled)[, c("estimate", "std.error", "statistic", "p.value")]
    coefsm <- cbind(exp(coefsm[, 1]), coefsm)
    colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "statistic",
      "p-value")
    rownames(coefsm) <- mice:::summary.mipo(pooled)[, "term"]
    inc_col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc_col, drop = FALSE]

    if (ci & or) {
      estci <- exp(mice:::summary.mipo(pooled, conf.int = TRUE, conf.level = ci_level)[, c(7, 8)])
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
        "Upper CL" = estci[,2 ], coefsm[, -1, drop = FALSE])
    }
  } else {
      stop("currently only vcov = NULL is supported")
  }
  # coef.type denotes which rows are intercepts and which are coefficients
  if (modclass == "glm"){
    coefsm <- cbind(coefsm, coef.type = c("intercept", rep("coefficient", nrow(coefsm)-1)))
  } else if (modclass == "polr") {
    coefsm <- cbind(coefsm,
                    coef.type = gsub(pattern = "scale",
                      replacement = "intercept",
                      x = broom::tidy(mod1)$coef.type))
  }
  # Round to specified number of digits
  if (pval) highsig <- which(coefsm[, "p-value"] < 0.001)
  coefsm[,-which(names(coefsm) == "coef.type")] <-
    round(coefsm[,-which(names(coefsm) == "coef.type")], digits = digits)
  if (pval) coefsm[highsig, "p-value"] <- "<0.001"
  # Add reference level
  if (addref) {
    coefsm <- add_reference_levels(coefsm, mod1, or)
  }
  if (!is.null(rowlabs_auto) & is.null(rowlabs)) {
    # Add labels to names of covariates
    covar_pos = which(coefsm$coef.type == "coefficient")
    covar_names = names(stats::model.frame(mod1))[-1]
    if(!is.null(mod1$model$`(weights)`)){
      covar_names <- covar_names[-length(covar_names)]
    }
    rowlabs <- generate_rowlabs(rownames = rownames(coefsm),  mod = mod1,
                                addref = addref, rowlabs_auto = rowlabs_auto,
                                covar_names = covar_names,
                                covar_pos = covar_pos)
    # POLRs also have levels for the different outcome scale levels
    # Give them a level name as well
    if(intercept & modclass == "polr"){
      outcome_label <- attr(rowlabs_auto[[names(stats::model.frame(mod1))[1]]], "label")
      if(is.null(outcome_label)){
        rowlabs[which(coefsm$coef.type == "intercept")] <-
          paste0("Outcome scale level (", rowlabs[which(coefsm$coef.type == "intercept")], ")")
      } else{
        rowlabs[which(coefsm$coef.type == "intercept")] <-
          paste0(outcome_label, ": outcome scale level (", rowlabs[which(coefsm$coef.type == "intercept")], ")")
      }

    }
  }
  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  # Remove intercept rows if unwanted
  if (!intercept) coefsm <- coefsm[coefsm$coef.type != "intercept", , drop = FALSE]
  # Add caption
  if (n_caption) {
    caption <- paste0(caption, " (n subjects = ", nrow(stats::model.frame(mod1)),
                      ", n imputations = ", pooled$m,")")
  }
  # Remove coef.type column
  coefsm <- coefsm[, which(names(coefsm) != "coef.type"), drop = FALSE]
  # Output of table using kable
  out <- kableExtra::kbl(coefsm, format = format, booktabs = TRUE,
    caption = caption, ...)

  if (length(style_options) > 0) {
    do.call(kableExtra::kable_styling, c(list(out), style_options))
  } else {
    out
  }
}
