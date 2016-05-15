#' @title Converts FLIndices object to rbya
#'
#' @description The rbya (results by year and age) is a long \code{data.frame}
#' popular in Reykjavik
#'
#' @export
#'
#' @param x An FLIndices object
#'
flindices_to_rbya <- function(x) {
  
  indices <- x@names
  x <- x@.Data
  
  x2 <- vector("list", length(indices))
  names(x2) <- indices
  
  for(i in seq_along(indices)) {
    x2[[i]] <- reshape2::melt(x[[i]]@index)[,c("year","age","value")] %>%
      dplyr::mutate(age = ifelse(age == "all",-9,age),
                    sur = indices[i])
  }
  
  x <- x2 %>% purrr::map_df(as_data_frame)
  
  
  return(x)
  
}