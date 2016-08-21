HOMEBREW_PROFILE_ID = "120682403"

.onLoad = function(...) {
  authorize()
}

authorize = function() {
  RGA::authorize(
    cache=path.expand("~/.config/HomebrewGA"),
    client.id="359112247345-q8niiacq2n9h5d5napdhgi01k6oijlou.apps.googleusercontent.com",
    client.secret="_jq5IxkGszRUhHW_hM0hoDzC")
}

fetch_install_invocations = function(start.date="30daysAgo") {
  df = RGA::get_ga(
    profileId=HOMEBREW_PROFILE_ID,
    start.date=start.date,
    end.date="yesterday",
    metrics="ga:totalEvents",
    dimensions="ga:eventCategory,ga:eventAction",
    sort="-ga:totalEvents",
    filters="ga:eventCategory==install",
    samplingLevel="HIGHER_PRECISION",
    include.empty.rows="FALSE"
  )
  # Fixed more holistically by https://github.com/Homebrew/brew/pull/659
  df$eventAction = stringr::str_replace(df$eventAction, "--c 11", "--c++11")
  df$eventAction = stringr::str_replace(df$eventAction, "gtk 3", "gtk+3")
  df
}

popularity_by_package = function(df) {
  invocations = stringr::str_split(df$eventAction, " ")
  df$package = sapply(invocations, function(x) x[[1]])
  df %>% count(package, wt=totalEvents)
}

install_invocations_by_package = function(df) {
  invocations = stringr::str_split(df$eventAction, " ")
  package_names = invocations %>%
    sapply(function(x) x[[1]]) %>%
    unique

  # Create a list of lists for packages and their arguments, to look like:
  # options_by_package[["gd"]][["none"]] = x,
  # options_by_package[["gd"]][["--universal"]] = y, ...
  options_by_package = replicate(length(package_names), list())
  names(options_by_package) = package_names
  for(i in seq_along(invocations)) {
    package_name = invocations[[i]][[1]]
    nargs = length(invocations[[i]])
    if(nargs == 1) {
      options_by_package[[package_name]][["none"]] = df[i, "totalEvents"]
    } else {
      for(j in 2:nargs) {
        arg = invocations[[i]][[j]]
        prior = if(exists(arg, options_by_package[[package_name]])) options_by_package[[package_name]][[arg]] else 0
        options_by_package[[package_name]][[arg]] = prior + df[i, "totalEvents"]
      }
    }
  }

  # Crush into a data.frame
  package_name_col = names(options_by_package) %>%
    sapply(function(x) rep.int(x, length(options_by_package[[x]]))) %>%
    unlist(use.names=FALSE)
  option_col = options_by_package %>% sapply(function(x) names(x)) %>% unlist(use.names=FALSE)
  count_col = options_by_package %>% unlist(use.names=FALSE)
  data.frame(package=package_name_col, option=option_col, count=count_col)
}
