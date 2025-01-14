# coefixr 1.0.0

- NEWS tracking starts.
- Change column names for unexponentiated coefficients and CIs from "log_coef",
  "log_ci.95lwr", and "log_ci.95upr" to "coef", "ci.95lwr", and "ci.95upr". This 
  is because coefficients do not always come out of models as log values. **This
  is a breaking change for old code**, hence a new major version number.
- Add `is.top` and `is.intx` columns to the output of `adjust_interaction_model()`
  as conveniences for subsetting the dataframe. 
- Add `intercept` arg to `adjust_interaction_model()` to keep/suppress reporting 
  of the `(Intercept)` term.
