#' Linreg Class
#'
#' This class does linear regression and provided different methods to display the data.
#'
#' @field l_X matrix. Independent Values.
#' @field l_y matrix. Depenent Values.
#' @field l_beta matrix. Regression Coeffcients.
#' @field l_y_fitted_values matrix. Fitted Values.
#' @field l_e matrix. Residuals.
#' @field l_n numeric. Number of Independant Values (Number of Observations).
#' @field l_p numeric. Number of Dependant Values (Number of Parameters in the Model).
#' @field l_df numeric. Degrees of freedom.
#' @field l_sigma_s matrix. Residual Variance.
#' @field l_var_beta matrix. Variance of the Regression Coefficients.
#' @field l_formula formula. The Formula for the Linear Regression.
#' @field l_p_values numeric. P-Values.
#' @field l_t_beta matrix. T-Values for each Coefficient.
#' @field l_data_set_name character. The Given Data.
#'
#' @return Nothing.
#' @export

linreg <- setRefClass("linreg",
                      fields = list(
                        l_X = "matrix",
                        l_y = "matrix",
                        l_beta = "matrix",
                        l_y_fitted_values = "matrix",
                        l_e = "matrix",
                        l_n = "numeric",
                        l_p = "numeric",
                        l_df = "numeric",
                        l_sigma_s = "matrix",
                        l_var_beta = "matrix",
                        l_formula = "formula",
                        l_p_values = "numeric",
                        l_t_beta = "matrix",
                        l_data_set_name = "character"),

                      # Methods ----------------------------
                      methods = list(

                        # Constructor ----------------------
                        initialize = function(formula, data) {
                          "Constructor for creating the object. Arguments are the formlua and the corresponding data frame."

                          # Input Validation
                          if (class(formula) != "formula") stop("Argument 'formula' must have the class formula.")
                          if (class(data) != "data.frame") stop("Argument 'data' must have the class data.frame")

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

                          # Calculate The t-values for each coefficient
                          l_t_beta <<- l_beta / as.numeric(sqrt(var(l_beta))) ## These calculated values are wrong

                          ## Saving
                          l_formula <<- formula
                          l_data_set_name <<- deparse(substitute(data))
                          l_p_values <<- sapply(l_y, pt, q = ncol(l_X), df = l_df)
                        },

                        # print function ------------------
                        print = function() {
                          "Prints some basic information like the coefficients."

                          # Formula
                          cat(paste("linreg(formula = ", format(l_formula), ", data = ", l_data_set_name, ")\n\n", sep = ""))

                          # Coefficients
                          cat(paste("Coefficients:\n\n"))

                          # Values
                          table = setNames(data.frame(matrix(ncol = length(l_beta), nrow = 0)), rownames(l_beta))
                          for (i in 1:length(l_beta)) {
                            table[1,i] = round(l_beta[i], 3)
                          }
                          cPrint(table)
                        },

                        # plot function -------------------
                        plot  = function() {
                          "Plots the Fitted Values one time with the Residuals and one time with standardized Residuals"

                          #resfit = as.data.frame(cbind(l_e, l_y))
                          #names(resfit) = c("residuals", "fitted")

                          plot1 = ggplot(data.frame(l_e, l_y_fitted_values), aes(y = l_e, x = l_y_fitted_values))+
                            geom_point()+
                            xlab(paste("Fitted Values\n", "lm(", format(l_formula), ")", ""))+
                            ylab("Residuals")+
                            stat_summary(aes(y = l_e, x = l_y_fitted_values ,group=1),
                                         fun.y=median, colour="red", geom="line",group=1)
                          # geom_smooth(aes(y = l_e, x = l_y_fitted_values),
                          #             formula = y~x,
                          #             se = FALSE,
                          #             span = 1,
                          #             color = "red",
                          #             method = "auto")

                          cPrint(plot1)

                          stdresfit = as.data.frame(cbind(sqrt(abs((l_e - mean(l_e)))), l_y_fitted_values))
                          names(stdresfit) = c("stdResiduals", "fitted")

                          plot2 = ggplot(stdresfit, aes(x = fitted, y = stdResiduals))+
                            geom_point()+
                            xlab(paste("Fitted Values\n", "lm(", format(l_formula), ")", ""))+
                            ylab(expression(sqrt("|Standardized residuals|")))+
                            stat_summary(aes(y = stdResiduals, x = fitted ,group=1),
                                         fun.y= mean, colour="red", geom="line",group=1)



                          # geom_smooth(aes(y = stdResiduals, x = fitted),
                          #             formula = y~x,
                          #             se = FALSE,
                          #             span = 1,
                          #             color = "red",
                          #             method = "auto")
                          cPrint(plot2)
                        },

                        # resid function -------------------
                        resid     = function() {
                          "Returns the Residuals"
                          return(l_e)
                        },

                        # pred function --------------------
                        pred      = function() {
                          "Returns the Fitted Values"
                          return(l_y_fitted_values)
                        },

                        # ceof function --------------------
                        coef      = function() {
                          "Returns the Coefficients"
                          return(l_beta)
                        },

                        # summary function -----------------
                        summary   = function() {
                          "Prints a summary of the Linear Regression"

                          cat("\nCall:\n")
                          cat(paste("linreg(formula = ", (format(l_formula)), ", data = ", l_data_set_name, ")\n\n", sep = ""))

                          # Coefficients
                          cat("Coefficients:\n\n")

                          # Values
                          table = data.frame(matrix(ncol = 5, nrow = 0))
                          for (i in 1:length(l_beta)) {
                            # Beta (coefficients), std error, t values, p values
                            local_t_value = l_beta[i]/sqrt(l_var_beta[i, i])
                            local_p_value = 2 * pt(abs(local_t_value), l_df, lower.tail = FALSE)
                            newRow = data.frame(round(l_beta[i], 2), round(sqrt(l_var_beta[i, i]), 2), round(local_t_value, 2), formatC(local_p_value, format = "e", digits = 2), calculateMagicRainbowStars(local_p_value))
                            rownames(newRow)[1] = rownames(l_var_beta)[i]
                            table = rbind(table, newRow)
                          }

                          colnames(table) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
                          cPrint(table, TRUE)
                          cat(paste("\nResidual standard error:", sqrt(l_sigma_s), "on", l_df, "degrees of freedom"))
                        }
                      )
)

#' cPrint (custom print)
#'
#' Prints the value in \code{x}. This class can be used to print inside RC classes, which is not possible otherwise.
#'
#' @param x Any object that normally can be printed.
#' @param stripoff If set to TRUE and \code{x} is a data.frame, the column names will be stripped off.
#'
#' @return Nothing.
cPrint = function(x, stripoff = FALSE) {
  if (is.data.frame(x)) {
    print(x, row.names = stripoff)
  }
  else {
    print(x)
  }
}

#' calculateMagicRainbowStars
#'
#' Returns different notations depending which value the p_value holds.
#'
#' @param p_value Obviously the p_value.
#'
#' @return Returns: Signif. codes:  0 "***" 0.001 "**" 0.01 "*" 0.05 "." 0.1 " " 1
#'
calculateMagicRainbowStars = function(p_value) {
  if (p_value > 0.1) return(" ")
  if (p_value > 0.05) return(".")
  if (p_value > 0.01) return("*")
  if (p_value > 0.001) return("**")
  return("***")
}
