library(tidyr)
library(stringr)

## function to check if the list of given "to" and "from" edges 
## with edge weights form a symmetric matrix 
## format: same rownames and column names (need not be sorted)

f_check_symmetric <- function(df_edge){
  
  # sort rows and columns by index name
  df_edge_sorted <- df_edge[order(rownames(df_edge)), 
                            order(colnames(df_edge))]
  
  # Check if data frame is symmetric
  is_symmetric <- function(df) {
    # Convert data frame to matrix and check if it is equal to its transpose
    return(identical(as.matrix(df), t(as.matrix(df))))
  }
  
  # Apply function
  return(is_symmetric(df_edge_sorted))
}

## Main

# get list of edge files
directory_path <- "/restricted/projectnb/ai4ad/sahelijo/network/results/wgcna/networks"
files <- list.files(directory_path, pattern = "*_edge.tsv$")


# loop to check symm status of each edge file
for (file in files) {
  file_path <- file.path(directory_path, file)
  print(file_path)
  
  # read
  df_edges <- read.csv(
    file_path,
    sep = "\t"
  )
  
  # format data frame for function input
  # pivot "to" column and keep toms
  df_edges_wide <- pivot_wider(
    df_edges[, c(1,2,5)], names_from = to, values_from = tom
  )
  
  # convert from tibble to dataframe (tibble does not allow rownames)
  df_edges_wide <- as.data.frame(df_edges_wide)
  
  # set rownames
  rownames(df_edges_wide) <- df_edges_wide$from
  df_edges_wide <- df_edges_wide[, -1]
  
  print("Is symmetric?")
  print(f_check_symmetric(df_edges_wide))
}



