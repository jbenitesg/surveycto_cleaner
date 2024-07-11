surveyCTO_cleaner = function(data,  fields, choices){
  # data: Data
  # fields: Field structure of data based on SurveyCTO
  # choices: Choices of data based on SurveyCTO
  require("dplyr")
  require("haven")
  require("rsurveycto")
  
  for(r in 1:nrow(fields)) {
    temporal = fields[r,]
    
    # Column name
    col_obj = fields[r,2] %>%
      pull()
    # Label column
    label_obj = fields[r,3] %>%
      pull()
    
    
    # Repeat variables
    ## Identify: begin/end repeat  
    indices_begin <- which(fields$type == "begin repeat")
    indices_end <- which(fields$type == "end repeat")
    
    ## Save in a list
    repeat_list <- list()
    
    ## Save by index group 
    if (length(indices_begin) == length(indices_end)) {
      ### Variables
      for (i in seq_along(indices_begin)) {
        df_filtrado <- fields[(indices_begin[i] + 1):(indices_end[i] - 1), ]
        repeat_list[[i]] <- df_filtrado
      }
      
      # Combinar todas las subsecciones filtradas en un solo dataframe (opcional)
      repeat_variables <- map_df(.x = repeat_list,
                                 .f = dplyr::bind_rows) %>%
        pull(name)
    }
    
    # Normal variables
    if(col_obj %in% colnames(data)) {
      temporal = fields %>%
        dplyr::filter(name %in% col_obj)
      
      
      # Select one/multiple
      type1 = temporal %>%
        dplyr::filter(grepl("^select_", type)) %>%
        dplyr::mutate(type = str_extract(type, "(?<=\\s).*"))
      
      # Select integer
      type2 = temporal %>%
        dplyr::filter(type=='integer')
      
      # Select decimal
      type3 = temporal%>%
        dplyr::filter(type=='decimal')
      
      
      if(nrow(type1) > 0) {
        
        tmp_labs = choices %>%
          dplyr::filter(list_name %in% type1$type) %>%
          mutate_at(vars(value), as.numeric) %>%
          dplyr::select(value, label)
        
        labels = setNames(tmp_labs$value,
                          tmp_labs$label)
        
        data = data %>%
          mutate_at(
            vars(all_of(col_obj)),
            ~ haven::labelled(
              x = as.numeric(.x),
              label = label_obj,
              labels = labels
            )
          )
      }
      
      # Integer
      if((nrow(type2) > 0)) {
        data = data %>%
          mutate_at(
            vars(all_of(col_obj)),
            ~ haven::labelled(
              x = as.integer(.x),
              label = label_obj
            )
          )
      }
      # Decimal
      if((nrow(type3) > 0)) {
        data = data %>%
          mutate_at(
            vars(all_of(col_obj)),
            ~ haven::labelled(
              x = as.double(.x),
              label = label_obj
            )
          )
      }
      else{
        data = data %>%
          mutate_at(
            vars(all_of(col_obj)),
            ~ haven::labelled(
              x = .x,
              label = label_obj
            )
          )
      }
    }
    
    if(col_obj %in% repeat_variables) {
      temporal = fields %>%
        dplyr::filter(name %in% col_obj)
      
      
      # Select one/multiple
      type1 = temporal %>%
        dplyr::filter(grepl("^select_", type)) %>%
        dplyr::mutate(type = str_extract(type, "(?<=\\s).*"))
      
      # Select integer
      type2 = temporal %>%
        dplyr::filter(type=='integer')
      
      # Select decimal
      type3 = temporal%>%
        dplyr::filter(type=='decimal')
      
      
      if(nrow(type1) > 0) {
        
        tmp_labs = choices %>%
          dplyr::filter(list_name %in% type1$type) %>%
          mutate_at(vars(value), as.numeric) %>%
          dplyr::select(value, label)
        
        labels = setNames(tmp_labs$value,
                          tmp_labs$label)
        
        data = data %>%
          mutate_at(
            vars(matches(paste0("^",col_obj,"_\\d{1,2}$"))),
            ~ haven::labelled(
              x = as.numeric(.x),
              label = label_obj,
              labels = labels
            )
          )
      }
      
      # Integer
      if((nrow(type2) > 0)) {
        data = data %>%
          mutate_at(
            vars(matches(paste0("^",col_obj,"_\\d{1,2}$"))),
            ~ haven::labelled(
              x = as.integer(.x),
              label = label_obj
            )
          )
      }
      # Decimal
      if((nrow(type3) > 0)) {
        data = data %>%
          mutate_at(
            vars(matches(paste0("^",col_obj,"_\\d{1,2}$"))),
            ~ haven::labelled(
              x = as.double(.x),
              label = label_obj
            )
          )
      }
      else{
        data = data %>%
          mutate_at(
            vars(matches(paste0("^",col_obj,"_\\d{1,2}$"))),
            ~ haven::labelled(
              x = .x,
              label = label_obj
            )
          )
      }
      
    }
  }
  
  
  return(as_tibble(data))
}

