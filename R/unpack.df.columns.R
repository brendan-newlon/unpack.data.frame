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

unpack.df.columns <- function(df) {
  `%notin%` <- Negate(`%in%`)

  delete_list <- as.vector(c())
  col_name_orig_order_list <- as.vector(c())
  col_name_order_list <- as.vector(c())

  # original column order
  col_name_orig_order_list <- names(df)

  # Get class for each col
  d <- lapply(df, class)
  # Identify which cols are data.frame
  dnames <- names(df[d == "data.frame"])

  # Split into separate dataframes, one for df cols and one for non-df cols
  simple_df <- df[!names(df) %in% dnames]

  # non_df_names <- names(non_df_cols)
  complex_df <- df[c(dnames)]
  # df_names <- names(df_cols)

  df <- complex_df

  repeat {
    col_name_order_list <- names(df)
    if ("data.frame" %notin% lapply(df, class)) {
      break
    } else {
      for (i in 1:ncol(df)) {
        if (class(df[i]) != "data.frame") {
          # col_name_order_list <- append(col_name_order_list,names(df[i]))
          next
        }
        delete_list <- append(delete_list, names(df[i]))

        this_col_name <- names(df[i])
        sub_df_names <- names(df[, i])

        if (is.null(names(df[, i]))) {
          sub_df_names <- "unnamed"
        }

        new_df_names <- paste0(this_col_name, "_", sub_df_names)

        # replace the df col name with its new sub col names
        replace_col_index <-
          match(this_col_name, col_name_order_list) # get index of that col name in the list
        col_name_order_list <-
          append(col_name_order_list, new_df_names, replace_col_index)
        col_name_order_list <-
          col_name_order_list[-replace_col_index]

        #
        # Same for orig order list
        replace_col_orig_index <-
          match(this_col_name, col_name_orig_order_list) # get index of that col name in the list
        col_name_orig_order_list <-
          append(col_name_orig_order_list,
                 new_df_names,
                 replace_col_orig_index)
        col_name_orig_order_list <-
          col_name_orig_order_list[-replace_col_index]

        # The temporary new df which is the sub df inside this df column
        new_df <- as.data.frame(df[, i], stringsAsFactors = FALSE) %>%
          setNames(new_df_names)

        # remove the df column
        df2 <- df %>%
          select(-c(delete_list))

        # Put the unpacked new columns in place of the df col
        df <- cbind(df2, new_df)   #?

        df <- df[, col_name_order_list]

        delete_list <- as.vector(c())
      }

    } # end else  # therefore end the for i loop

    names(df) <- names(df) %>%
      gsub("_unnamed", "", .)

    col_name_orig_order_list <- col_name_orig_order_list %>%
      gsub("_unnamed", "", .)

  } # end repeat

  df <- cbind(simple_df, df)
  df <- df[, col_name_orig_order_list]

  return(df)
} # end function
