library(devtools)
library(gitcreds)
library(usethis)

usethis::create_project(getwd())

usethis::use_git()

use_git_config("GabrielaSales-23", "gabrielasales@discente.ufg.br")
