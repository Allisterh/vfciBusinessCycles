get_pc <- function(
    data,
    var_names,
    date_begin = "1962 Q1",
    date_end = "2022 Q3",
    cor = TRUE,
    scores = TRUE,
    center = TRUE,
    scale. = TRUE,
    match_stata = FALSE,
    ...
) {
  prcomp_tidy <- function(x,...){ 
    #preferred method is use prcomp, using princomp to match stata
    if (match_stata){
      princomp(x, cor = cor ,scores = scores, ...) #covmat = MASS::cov.rob(x)  
    }
    else{
      prcomp(x, center = center, scale. = scale., ...) 
    }
  }
  pcs <- data %>%
    tsibble::as_tsibble() %>% 
    tsibble::filter_index(date_begin ~ date_end) %>% 
    tsibble::as_tibble() %>% 
    dplyr::select(all_of(var_names)) %>%
    prcomp_tidy()
}