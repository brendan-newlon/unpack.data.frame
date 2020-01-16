#' unpack.df.columns
#'
#' Function for unpacking dataframes where columns also contain dataframes. It returns a dataframe with appropriately-named atomic columns.
#'
#' @param df the data.frame to unpack
#'
#' @return the unpacked, flattened data.frame
#'
#' @export
#'
#' @examples
#' unpack.df.columns(df)


# function for unpacking dataframes where columns also contain dataframes. It returns a dataframe with atomic columns

############
unpack.df.columns <- function (df, all_as_character = FALSE) 
{
  `%notin%` <- Negate(`%in%`)
# PREP  
  # delete_list <- as.vector(c())
  col_name_orig_order_list <- as.vector(c())
  col_name_order_list <- as.vector(c())
    col_name_orig_order_list <- names(df)
  
    repeat {
  
    if ("data.frame" %notin% lapply(df, class)) {   # that means we're done unpacking
      break 
    } else {
      d <- lapply(df, class)
  dnames <- names(df[d == "data.frame"])
  simple_df <- df[!names(df) %in% dnames]
  df <- df[c(dnames)]
        col_name_order_list <- names(df)
     
      for (i in 1:ncol(df)) {
        if (class(df[i]) != "data.frame") {
          next
        }
        # delete_list <- append(delete_list, names(df[i])) # what column are we about to unpack? It will be redundant.
        # actually, since we're unpacking one df col at a time, we don't need this as a list. we can just use this_col_name
        
        this_col_name <- names(df[i]) 
        sub_df_names <- names(df[, i])
        if (is.null(names(df[, i]))) {
          sub_df_names <- "unnamed"
          
        }
        new_df_names <- paste0(this_col_name, "_", sub_df_names)
        
        # is this throwing error? maybe i already ran this step, so it has changed...
        replace_col_index <- match(this_col_name, col_name_order_list)
        
        col_name_order_list <- append(col_name_order_list, new_df_names, replace_col_index)
        col_name_order_list <- col_name_order_list[-replace_col_index]
        
        # prob here
        replace_col_orig_index <- match(this_col_name, col_name_orig_order_list)

          col_name_orig_order_list <- append(col_name_orig_order_list, 
          new_df_names, replace_col_orig_index)
          
          col_name_orig_order_list <- col_name_orig_order_list[-replace_col_orig_index]
        new_df <- as.data.frame(df[, i], stringsAsFactors = FALSE) %>% 
          setNames(new_df_names)
        
        df2 <- df %>% select(-c(this_col_name))
        df <- cbind(df2, new_df)
        new_names <- names(df)
        
        df <- as.data.frame(df[, col_name_order_list],stringsAsFactors = FALSE) %>% 
          setNames(new_names)
    
      }
        
      df <- cbind(simple_df, df)
      
    names(df) <- names(df) %>% gsub("_unnamed", "", .)
  
    col_name_orig_order_list <- col_name_orig_order_list %>% 
      gsub("_unnamed", "", .)

    } # end else (ie. not breaking the repeat)
  } # end repeat
    
  df <- df[, col_name_orig_order_list] 
  
  names(df) <- names(df) %>%
    gsub("_{2, }","_",.) # underscore 2 or more times
  
  names(df) <- names(df) %>%
    gsub("^_","",.) # underscore as first char
  
  
  if(all_as_character = TRUE){
    df <- df %>%
      mutate_all(as.character)
  }

    return(df)
}
