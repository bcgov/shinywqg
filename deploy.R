#run the test first
rsconnect::deployApp(appDir = ".", account = "bcgov-env",
                     appName = "bc_wqg_test", forceUpdate = TRUE)


#live site
rsconnect::deployApp(appDir = ".", account = "bcgov-env",
                     appName = "bc_wqg", forceUpdate = TRUE)