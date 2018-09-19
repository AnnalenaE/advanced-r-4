#### linreg class

linreg <- setRefClass("linreg",
                      fields = list(
                        p_values = "numeric",
                        residuals = "matrix",
                        predicted_values = "matrix",
                        coefficients = "matrix",
                        formula = "formula",
                        g_df = "numeric",
                        g_sigma = "matrix",
                        g_x = "matrix",
                        g_var_beta = "matrix",
                        data_set_name = "character",
                        g_y = "matrix"),

                      methods = list(
                        initialize = function(formula, data) {

                          # This contructor will be called like this
                          #linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

                          #if (!is.formula(formula)) stop("Error messgage")
                          #stopifnot(class(formula) == "formula", error = stop("formula is invalid."))

                          X = model.matrix(formula, data) ## Create the X matrix with dummy variables. ~ necessary?
                          #y = data[,names(data) == all.vars(formula)[1]] ## Get all dependent variables
                          y = as.matrix(data[all.vars(formula)[1]])
                          #y = data[which(names(data) == y)]

                          # Calculate Regressions coefficients:
                          beta = as.matrix((solve((t(X) %*% X)) %*% t(X) %*% y)) ## Remember he told something about that solve doesn't always works, depends on the eigenvalues

                          # Calculate The fitted values:
                          y_circ = X %*% beta

                          # Calculate The residuals:
                          e_circ = as.matrix(y - y_circ)

                          # Calculate The degrees of freedom:
                          # where n is the number of observations and p is the number of parameters in the model.
                          n = nrow(X)
                          p = ncol(X)
                          df = n - p # We still need to get n and p, maybe it's just length(X) - length(y)
                          #df = df.residual(y_circ)

                          # Calculate The residual variance:
                          sigma_squared = (t(as.matrix(e_circ)) %*% as.matrix(e_circ)) / df

                          # Calculate The variance of the regression coefficients:
                          var_beta = as.numeric(sigma_squared) * solve(t(as.matrix(X)) %*% as.matrix(X)) ## Why not use var(beta)?

                          # Calculate The t-values for each coefficient:
                          t_beta = beta / as.numeric(sqrt(var(beta)))

                          # Use pt() function the calcualte the p values and then store them inside
                          # of our object for each regression coefficient
                          #pt()

                          #customObject = linreg$new()
                          # x, q	vector of quantiles
                          # df number of freedoms
                          # ncp non-centrality parameter delta; currently except for rt(), only for abs(ncp) <= 37.62. If omitted, use the central t distribution.

                          ## Saving
                          formula <<- formula
                          p_values <<- sapply(y, pt, q = ncol(X), df = df)
                          residuals <<- e_circ
                          predicted_values <<- y_circ
                          coefficients <<- beta
                          g_df <<- df
                          g_y <<- y
                          g_x <<- X
                          g_var_beta <<- var_beta
                          g_sigma <<- sqrt(sigma_squared)
                          data_set_name <<- deparse(substitute(data))
                        },
                        print = function() {
                          ## Formula
                          cat(paste("linreg(formula = ", format(formula), ", data = ", data_set_name, ")\n\n", sep = ""))

                          ## Coefficients:
                          temp_out = paste("Coefficients:\n\n ")

                          for (i in 1:length(coefficients)) {
                            temp_out = paste(temp_out, rownames(coefficients)[i], "\t", sep = "")
                          }
                          cat(temp_out)
                          temp_out = paste("\n")

                          for (i in 1:length(coefficients)) {
                            temp_out = paste(temp_out, "\t", round(coefficients[i], digits = 3))
                          }

                          cat(paste(temp_out, "\n"))
                        },
                        plot      = function() {},
                        resid     = function() { return(residuals) },
                        pred      = function() { return(predicted_values) },
                        coef      = function() { return(coefficients) },
                        summary_custom = function() {
                          cat("p_values\n", p_values, "\n")
                          cat("residuals\n", residuals, "\n")
                          cat("predicted_values\n", predicted_values, "\n")
                          cat("coefficients\n", coefficients, "\n")
                        },
                        summary   = function() {

                          #expect_output(linreg_mod$summary(), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")
                          #expect_output(linreg_mod$summary(), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
                          #expect_output(linreg_mod$summary(), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
                          #expect_output(linreg_mod$summary(), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")

                          cat(paste("\nCall:\n"))
                          cat(paste("linreg(formula = ", format(formula), ", data = ", data_set_name, ")\n\n", sep = ""))

                          ## Coefficients:
                          temp_out = paste("Coefficients:\n\t")
                          temp_out = paste(temp_out, "\tEstimate\t", "Std. Error\t", "t value\t", "Pr(>|t|)\n", sep = "")

                          for (i in 1:length(coefficients)) {
                            # Beta (coefficients), std error, t values, p values
                            temp_out = paste(temp_out,
                                             rownames(coefficients)[i],
                                             "\t", round(coefficients[i], 2),
                                             "\t", round(sqrt(g_var_beta[i, i]), 2),
                                             "\n")
                          }
                          cat(temp_out)

                          cat(paste("\n\nResidual standard error:", g_sigma, "on", g_df, "degrees of freedom"))

                        }
                      )
)

linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#print(linreg_mod$p_values)
#print(linreg_mod$resid())
#print(linreg_mod$predicted_values())
#print(linreg_mod$coefficients
#linreg%print()
linreg_mod$summary()


