#### linreg class

linreg <- setRefClass("linreg",
  fields = list(pValues = "numeric"),
  methods = list(
    initialize = function(formula, data) {

      # This contructor will be called like this
      #linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

      #if (!is.formula(formula)) stop("Error messgage")
      #stopifnot(class(formula) == "formula", error = stop("formula is invalid."))

      X = model.matrix(formula,data) ## Create the X matrix with dummy variables. ~ necessary?
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
      var_beta = as.numeric(sigma_squared) * (t(as.matrix(X)) %*% as.matrix(X)) ## Why not use var(beta)?

      # Calculate The t-values for each coefficient:
      t_beta = beta / as.numeric(sqrt(var(beta)))

      # Use pt() function the calcualte the p values and then store them inside
      # of our object for each regression coefficient
      #pt()

      #customObject = linreg$new()
      # x, q	vector of quantiles
      # df number of freedoms
      # ncp non-centrality parameter delta; currently except for rt(), only for abs(ncp) <= 37.62. If omitted, use the central t distribution.

      pValues <<- sapply(y, pt, q = ncol(X), df = df)
    },
    print     = function() {},
    plot      = function() {},
    resid     = function() {},
    pred      = function() {},
    coef      = function() {},
    summary   = function() {},
    addPValue = function(value) {
      pValues <<- c(pValues, value)
    },
    cPValues  = function () {
      pValues <<- list()
    }
  )
)

linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
print(linreg_mod$pValues)
