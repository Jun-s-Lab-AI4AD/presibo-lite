library(igraph)

## wrapper function to simplify to and from edge list dataframes
## of symmetric (undirected) edge lists
## Format: from, to, tom (weight)

f_simplify_edgelist_df <- function(df_edge){
  # Create a graph from the data frame
  g <- igraph::graph_from_data_frame(df_edge, directed = FALSE)
  
  # Simplify the graph, keeping the first weight as symmetric
  g_simplified <- igraph::simplify(g, edge.attr = list(tom = "first"))
  
  # Get the simplified edges data frame
  df_edge_simplified <- igraph::as_data_frame(g_simplified)
  
  return(df_edge_simplified)
} 

## Main

# get list of edge files
directory_path <- "/restricted/projectnb/ai4ad/sahelijo/network/results/wgcna/networks"
files <- list.files(directory_path, pattern = "*_edge.tsv$")

# custom function for _edge.tsv files
extract_string_before_edge <- function(string) {
  # Use regular expression to match the pattern
  match <- stringr::str_match(string, "^(.+?)_edge\\.tsv$")[, 2]
  
  # Return the extracted string or NULL if no match
  return(ifelse(is.na(match), NULL, match))
}


# loop to check symm status of each edge file
for (file in files){
  file_path <- file.path(directory_path, file)
  out_file_path <- paste(extract_string_before_edge(file), 
                        "_nathan_simplified_edge.tsv", sep = "")
  
  cat("Reading from...", file_path, "\n")
  # read
  df_edge <- read.csv(
    file_path,
    sep = "\t"
  )

  df_edge_ensg <- df_edge[, c("from", "to", "tom")]

  df_edge_ensg_simplified <- f_simplify_edgelist_df(df_edge_ensg)
  cat("Dimensions:", dim(df_edge_ensg_simplified), "\n")
  
  cat("Writing to...", out_file_path, "\n")
  write.csv(df_edge_ensg_simplified, out_file_path, row.names = F)
}

