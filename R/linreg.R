#### linreg class

linreg <- setRefClass("linreg",
    fields = list(
      l_X = "matrix",
      l_y = "matrix",
      l_beta = "matrix", # Coefficients
      l_y_fitted_values = "matrix",
      l_e = "matrix", # Residuals
      l_n = "numeric",
      l_p = "numeric",
      l_df = "numeric", # Degrees of Freedom
      l_sigma_s = "matrix",
      l_var_beta = "matrix",
      l_formula = "formula",
      l_p_values = "numeric",
      l_data_set_name = "character"),
    methods = list(
      # Constructor
      initialize = function(formula, data) {

        # Input Validation
        #if (!is.formula(formula)) stop("Error messgage")
        #stopifnot(class(formula) == "formula", error = stop("formula is invalid."))

        l_X <<- model.matrix(formula, data)
        l_y <<- as.matrix(data[all.vars(formula)[1]])

        # Calculate Regressions Coefficients
        ## Remember he told something about that solve doesn't always works, depends on the eigenvalues.
        l_beta <<- as.matrix((solve((t(l_X) %*% l_X)) %*% t(l_X) %*% l_y))

        # The Fitted Values
        l_y_fitted_values <<- l_X %*% l_beta

        # The Residuals
        l_e <<- l_y - l_y_fitted_values

        # Calculate The degrees of freedom
        # Where n is the number of observations and p is the number of parameters in the model.
        l_n <<- nrow(l_X)
        l_p <<- ncol(l_X)

        # Degrees of Freedom
        l_df <<- l_n - l_p

        # The Residual Variance
        l_sigma_s <<- (t(l_e) %*% l_e) / l_df

        # Variance of Regression Coefficients
        l_var_beta <<- as.numeric(l_sigma_s) * solve(t(as.matrix(l_X)) %*% as.matrix(l_X))

        # Calculate The t-values for each coefficient:
        t_l_beta = l_beta / as.numeric(sqrt(var(l_beta)))

        ## Saving
        l_formula <<- formula
        l_data_set_name <<- deparse(substitute(data))
        l_p_values <<- sapply(l_y, pt, q = ncol(l_X), df = l_df)
      },
      print = function() {

        # Formula
        cat(paste("linreg(formula = ", format(formula), ", data = ", l_data_set_name, ")\n\n", sep = ""))

        # Coefficients
        cat(paste("Coefficients:\n\n "))

        # Headings
        for (i in 1:length(l_beta)) {
          cat(paste("\t", rownames(l_beta)[i], sep = ""))
        }
        cat("\n\t")

        # Values
        for (i in 1:length(l_beta)) {
          cat(paste(round(l_beta[i], digits = 3), "\t\t"))
        }
        cat("\n")
      },
      plot      = function() {},
      resid     = function() { return(residuals) },
      pred      = function() { return(predicted_values) },
      coef      = function() { return(coefficients) },
      summary   = function() {

        cat(paste("\nCall:\n"))
        cat(paste("linreg(formula = ", format(formula), ", data = ", data_set_name, ")\n\n", sep = ""))

        ## Coefficients
        temp_out = paste("Coefficients:\n\t")
        temp_out = paste(temp_out, "\tEstimate\t", "Std. Error\t", "t value\t", "Pr(>|t|)\n", sep = "")

        for (i in 1:length(l_beta)) {
          # Beta (coefficients), std error, t values, p values

          local_t_value = l_beta[i]/sqrt(l_var_beta[i, i])

          temp_out = paste(temp_out,
                           rownames(l_beta)[i],
                           "\t", round(l_beta[i], 2),
                           "\t", round(sqrt(l_var_beta[i, i]), 2),
                           "\t", round(local_t_value, 2),
                           #"\t", round(pt(local_t_value, q = 1, df = g_df), 2),
                           "\n")
        }
        cat(temp_out)
        cat(paste("\n\nResidual standard error:", g_sigma, "on", g_df, "degrees of freedom"))
      }
    )
)

linreg_mod = linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_mod$print()
