
read_config <- function(config) {
  the$cfg <- if (!is.null(config)) {
    yaml::read_yaml(config)
  } else if (file.exists("project.yml")) {
    yaml::read_yaml("project.yml")
  } else {
    NULL
  }
}


