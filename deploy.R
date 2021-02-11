rsconnect::deployApp(appDir = ".", #account = "bcgov-env", 
                     account = "poissonconsulting",
                     appName = "bc_wqg_test", forceUpdate = TRUE)



# Warning in loadSupport(appDir, renv = sharedEnv, globalrenv = NULL) :
#   Loading R/ subdirectory for Shiny application, but this directory appears to contain an R package. Sourcing files in R/ may cause unexpected behavior.
# Loading shinywqg
# Warning: 
#   ── Conflicts ───────────────────────────────────────────── shinywqg conflicts ──
# ✖ .__global__() masks shinywqg::.__global__()
# 
# Did you accidentally source a file rather than using `load_all()`?
#   Run `rm(list = c(".__global__"))` to remove the conflicts.
# It seems that the version of `phantomjs` installed is greater than or equal to the requested version.To install the requested version or downgrade to another version, use `force = TRUE`.
# Error in value[[3L]](cond) : is.null(a) || is.list(a) is not TRUE
# Calls: local ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
#   Execution halted



# Warning in loadSupport(appDir, renv = sharedEnv, globalrenv = NULL) :
#   Loading R/ subdirectory for Shiny application, but this directory appears to contain an R package. Sourcing files in R/ may cause unexpected behavior.
# Error in value[[3L]](cond) : there is no package called ‘shinywqg’
# Calls: local ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
#   Execution halted