####################################################################################################
#                       Centro Nacional de Conservação da Flora - CNCFlora                         #
# FB2020 get taxon scientificname query using API -                                                #
# http://servicos.jbrj.gov.br/flora/taxon                                                          #
# Pablo Hendrigo Alves de Melo (melo.hendrigo@gmail.com)                                           #  
# Last update : 16-01-2020                                                                         #  
####################################################################################################

# Load packages
require(stringr)
require(jsonlite)
require(lubridate)
require(plyr)
require(flora)

# binomialSearch="Ternstroemia cuneifolia"
# binomialSearch = "Pouteria macrocarpa"
# binomialSearch = "Aspidosperma occidentale Malme"
# binomialSearch <- "Aspidosperma occidentale"
# binomialSearch <- "Gardnerina angustata"

FB2020_get_taxon_scientificname_from_API <- function(binomialSearch) 
{  
   
  thirdWord <- word(binomialSearch,3)
  withAuthor <- (!is.na(thirdWord)) & (! tolower(thirdWord) %in% c('form.', 'var.', 'subsp.'))
   
  FB2020URL <- sprintf("http://servicos.jbrj.gov.br/flora/taxon/%s",  list(page = binomialSearch))
  returnFB2020 <- jsonlite::read_json(FB2020URL, simplifyVector=TRUE)
  FB2020 <- list(data.frame(found = FALSE))
  
  if ( ! is.null(returnFB2020$result) )
  {  
     acceptedName <- returnFB2020$result[1:14] %>%
        dplyr::mutate(scientificNameWithoutAuthor = 
                         str_remove(
                            scientificname %>%   
                               stringr::str_replace_all("[/' '()]", " ") %>% 
                               stringr::str_replace_all(" +", " ") %>%        
                               stringr::str_replace(" $", ""),                
                            paste0(' ', scientificnameauthorship) %>%    
                               stringr::str_replace_all("[/' '()]", " ") %>% 
                               stringr::str_replace_all(" +", " ") %>%        
                               stringr::str_replace(" $", "")) ) %>%
        dplyr::mutate(Search_Notes='') 
     
     if (withAuthor == FALSE) 
     { 
        acceptedName <- acceptedName %>%
           dplyr::filter( scientificNameWithoutAuthor %in% binomialSearch)
     } else 
     {
        acceptedName <- acceptedName %>%
           dplyr::filter(scientificname  %in% binomialSearch)
    }
         
    if (nrow(acceptedName)>1)
    {
       acceptedName$Search_Notes <- 'Enter binomial + author for exact resolution'
       print('Enter binomial + author for exact resolution')
       FB2020 <- list(found = data.frame(found = TRUE),
                      acceptedName = data.frame(acceptedName),
                      synonyms = data.frame(NULL))
       return(FB2020)
    } 
    
    if (acceptedName$taxonomicstatus == "")
    {acceptedName$Search_Notes <- 'Name without resolution'} 
    
    if (acceptedName$taxonomicstatus == "SINONIMO" & is.na(acceptedName$acceptednameusage) )
    {acceptedName$Search_Notes <- 'Database inconsistency, check website'}
    
    synonyms <-  plyr::ldply(returnFB2020$result$SINONIMO, data.frame)
    
    if ( NROW(synonyms)>0)
    {  
      
      synonyms <- synonyms %>%
        mutate(scientificNameWithoutAuthor =  str_remove(
           scientificname %>%   
              stringr::str_replace_all("[/' '()]", " ") %>% 
              stringr::str_replace_all(" +", " ") %>%        
              stringr::str_replace(" $", ""),                
           paste0(' ', scientificnameauthorship) %>%    
              stringr::str_replace_all("[/' '()]", " ") %>% 
              stringr::str_replace_all(" +", " ") %>%        
              stringr::str_replace(" $", "")))
    }  
    
    FB2020 <- list(found = data.frame(found = TRUE),
                   acceptedName = data.frame(acceptedName),
                   synonyms = data.frame(synonyms))
  }
  return(FB2020)
}  

