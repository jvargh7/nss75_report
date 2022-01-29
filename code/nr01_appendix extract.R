# Function to parse district factsheets
require(tidyverse)
source("code/nr_functions.R")

nss75_mapping <- readxl::read_excel("code/NSS75 Variable List.xlsx",sheet="nss75 mapping")
nss75_variables <- readxl::read_excel("code/NSS75 Variable List.xlsx",sheet="variables")

table1 <- map2_dfr(c(0,1),c("Rural","Urban"),
                   function(p,t){
                     nss75_read(p,t)
                     
                   }
) %>% 
  left_join(nss75_mapping,
            by=c("STATE"="nss75_state")) %>% 
  dplyr::select(-STATE) %>% 
  dplyr::select(contains("nfhs4"),everything())


table2 <- map2_dfr(c(2,3),c("Rural","Urban"),
                   function(p,t){
                     nss75_read(p,t) %>% 
                       mutate_at(vars(starts_with("P")),~as.numeric(.))
                     
                   }
) %>% 
  left_join(nss75_mapping,
            by=c("STATE"="nss75_state")) %>% 
  dplyr::select(-STATE) %>% 
  dplyr::select(contains("nfhs4"),everything())

table3 <- map2_dfr(c(4:19),rep(c("Rural","Urban"),each=8),
                   function(p,t){
                     print(p);
                     nss75_read(p,t) %>% 
                       mutate_at(vars(-ITEM_CATEGORY,-region),~as.numeric(.)) %>% 
                       pivot_longer(cols=-one_of("SL_NO","ITEM_CATEGORY","region"),
                                    names_to="nfhs4_statecode",values_to="mpce")
                     
                   }
) %>% 
  left_join(nss75_mapping,
            by=c("nfhs4_statecode")) %>% 
  dplyr::select(-nss75_state) %>% 
  dplyr::select(contains("nfhs4"),everything())


table4 <- map2_dfr(c(20:23),rep(c("Rural","Urban"),each=2),
                   function(p,t){
                     print(p);
                     nss75_read(p,t) %>% 
                       mutate_at(vars(starts_with("P"),one_of("ALL")),~as.numeric(.)) %>% 
                       pivot_longer(cols=-one_of("SL_NO","ITEM_CATEGORY","region"),
                                    names_to="percentile",values_to="mpce")
                     
                   }
) %>% 
  pivot_wider(names_from=percentile,values_from=mpce) %>% 
  mutate(nfhs4_state = "India",
         nfhs4_statecode = "IN") %>% 
  dplyr::select(contains("nfhs4"),everything())

# All in Value by Row x Column.csv ---------
write_csv(table1,"data/Table 1 Persons by States x Classes of MPCE.csv")
write_csv(table2,"data/Table 2 Persons by States x Fractiles of MPCE.csv")
write_csv(table3,"data/Table 3 MPCE by States x Items.csv")
write_csv(table4,"data/Table 4 MPCE by Classes of MPCE x Items.csv")