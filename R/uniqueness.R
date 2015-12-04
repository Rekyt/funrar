# Function to compute uniqueness on various Databases Authors: Pierre Denelle & Matthias Grenié

# Uniqueness function --------------------------------------------------------- Arguments: com_table, a data frame of community
# with the species present in the commu- -nity sp_col, a character vector indicating the name of the species column in
# 'com_table' dist_matrix, a matrix of functional distance between species, rows and cols should be named as species Outputs: The
# same table as 'com_table' with an 'Ui' column added for uniqueness
uniqueness = function(com_table, sp_col, dist_matrix) {
    
    if (!(sp_col %in% colnames(com_table))) {
        stop(paste0("'", sp_col, "' species column not in column names"))
    }
    
    if (nrow(dist_matrix) != ncol(dist_matrix)) {
        stop("Distance matrix is not square.")
    }
    
    # Extract all species in community
    com_species = com_table[[sp_col]] %>% unique() %>% as.character()
    
    # Submatrix containing distance of species in community
    com_dist = dist_matrix[com_species, com_species]
    
    # Replace diagonal by 'NA' for computation reasons
    diag(com_dist) = NA
    
    # Get minimum for each line
    u_index = apply(com_dist, 1, min, na.rm = T)
    
    # Data frame of species name and uniqueness
    u_df = data.frame(sp_name = names(u_index), Ui = u_index)
    
    # Add Uniqueness column by species
    com_table = com_table %>% dplyr:::left_join_impl(u_df, sp_col, "sp_name")
    
    return(com_table)
} 
