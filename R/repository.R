brew_prefix = function() {
  system2("brew", "--prefix", stdout=TRUE)[1]
}

brew_repository = function() {
  system2("brew", "--repository", stdout=TRUE)[1]
}

commits_since = function(formula_name, timespec="30 days ago") {
  tap_owner = "homebrew"
  tap_name = "core"
  if(grepl("/", formula_name)) {
    matches = stringr::str_split(formula_name, "/", n=3, simplify=TRUE)
    tap_owner = matches[1]
    tap_name = matches[2]
    formula_name = matches[3]
  }
  tap_root = file.path(brew_repository(), "Library", "Taps",
                       tap_owner, paste0("homebrew-", tap_name))
  formula_file = list.files(tap_root, pattern=paste0("^", formula_name, "\\.rb"),
                            recursive=TRUE, full.names=FALSE)
  if(length(formula_file) == 0) {
    return(as.POSIXct(character(0)))
  }
  args = c("-C", tap_root,
           "log",
           "--since", shQuote(timespec),
           paste0("--pretty=", shQuote("format:%aI")),
           formula_file)
  system2("git", args, stdout=TRUE) %>%
    stringr::str_replace("([+-][0-9]{2}):([0-9]{2})", "\\1\\2") %>%
    strptime("%FT%T%z") %>%
    as.POSIXct
}
