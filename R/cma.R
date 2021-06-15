#' cma: COVID mortality Ascertainment
#'
#' @description
#' cma is a reproducible R compendium for analysing impact of COVID-19 vaccination
#' to date.
#'
#' @docType package
#' @name cma
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom stats median na.omit quantile dgamma dnorm dunif dweibull lag
#' @importFrom rlang .data
#' @importFrom utils head tail
#' @importFrom dplyr group_by summarise ungroup left_join mutate select
#'
"_PACKAGE"
