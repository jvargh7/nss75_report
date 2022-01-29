nss75_read <- function(tabula_page,title){
  
  page_variables = nss75_variables %>% 
    dplyr::filter(page == (tabula_page+1)) %>% 
    dplyr::select(variable) %>% 
    pull()
  

  csv_data = read_csv(paste0("raw/tabula-2017-18 Report Appendix A-",tabula_page,".csv")) %>% 
    mutate_all(~as.character(.))
  
  if("X1" %in% colnames(csv_data)){
    x1_data = csv_data %>% 
      dplyr::filter(!is.na(X1)) 
    
    
    
    x1_data <- x1_data[,-length(names(x1_data))]
    
    names(x1_data) <- names(csv_data)[-1]
    nonx1_data = csv_data %>% 
      dplyr::filter(is.na(X1)) 
    
    csv_data <- bind_rows(nonx1_data,
                          x1_data)
    
    
    
  }
  
  csv_data %>% 
    dplyr::select(-one_of("X1")) %>% 
    rename_all(~page_variables) %>% 
    mutate(region = title) %>% 
    return(.)
  
}
