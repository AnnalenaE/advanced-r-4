#### linreg class

linreg <- setRefClass("linreg",
    fields = list(
      l_X = "matrix",
      l_y = "matrix",
      l_beta = "matrix", # Coefficients
      l_y_fitted_values = "matrix", # Predicted Values
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
        cat(paste("Coefficients:\n\n"))

        # Values
        table = setNames(data.frame(matrix(ncol = length(l_beta), nrow = 0)), rownames(l_beta))
        for (i in 1:length(l_beta)) {
          table[1,i] = round(l_beta[i], 3)
        }
        cPrint(table)
      },
      plot      = function() {},
      resid     = function() { return(l_e) },
      pred      = function() { return(l_y_fitted_values) },
      coef      = function() { return(l_beta) },
      summary   = function() {

        cat("\nCall:\n")
        cat(paste("linreg(formula = ", (format(l_formula)), ", data = ", l_data_set_name, ")\n\n", sep = ""))

        # Coefficients
        cat("Coefficients:\n\n")

        # Values
        table = data.frame(matrix(ncol = 4, nrow = 0))
        for (i in 1:length(l_beta)) {
          # Beta (coefficients), std error, t values, p values
          local_t_value = l_beta[i]/sqrt(l_var_beta[i, i])
          newRow = data.frame(round(l_beta[i], 2), round(sqrt(l_var_beta[i, i]), 2), round(local_t_value, 2), 0)
          table = rbind(table, newRow)
        }

        colnames(table) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
        cPrint(table)
        cat(paste("\nResidual standard error:", sqrt(l_sigma_s), "on", l_df, "degrees of freedom"))
      }
    )
)

# This function is needed becasue R in uncapable of printing inside of an RC class
cPrint = function(x) {
  if (is.data.frame(x)) {
    print(x, row.names = FALSE)
  }
  else {
    print(x)
  }
}

linreg_mod = linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#linreg_mod$print()
linreg_mod$summary()
