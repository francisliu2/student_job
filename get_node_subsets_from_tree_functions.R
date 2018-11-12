#There will be 3 main functions to perform the tree spasing functions: 
#1. getBag --> To retrieve bag elements based on the keep.inbag information of the getTree function. 
#   The bag will also be reindexed. 
#2. split_bag_from_node --> To use the split var and split point of one row of result from getTree 
#   to split the elements of the previous node, and write the spliting result to the corresponding left and right daughter.
#3. get_node_elements --> To take one bag and one tree, splits the nodes recursively, and return 
#   the getTree dataframe with node appended. 

# Retrieve the inbag data by filtering original data by inbag function of randomForest
getBag <- function(original_data, inbag){
  ii = rep(1:nrow(original_data),inbag) # inbag has shape [number of rows, 1]
  bag = subset(original_data[ii,])
  rownames(bag) = 1:nrow(bag)
  return(bag)
}   

# Split in node elements and return lists of rownames.
split_bag_from_node = function(in_node_df, tree_r, verbose=1){
  # Get the split variable
  split_var = as.character(tree_r[,'split var'])
  
  # Get a vector of in node elements
  column_to_split = in_node_df[,split_var]
  
  if (verbose >1) browser()
  
  diagnostics <- function(in_node_df, left_daughter, right_daughter, split_var){
    #print('####diagnostics from function split_bag_from_node: ####', quote=F)
    print(paste('Split Var:', split_var), quote=F)
    print("Left daughter:", quote=F)
    print(table(in_node_df[left_daughter, split_var]))
    print("Right daughter:", quote=F)
    print(table(in_node_df[right_daughter, split_var]))
  }
  
  if (is.factor(in_node_df[,split_var])){
    levels = unique(column_to_split)
    levels = sort(levels, decreasing = T)
    if (length(levels)!=1){
      n = length(levels)} else{
      n=0
      }
    
    split_point = levels[c(as.binary(tree_r[,'split point'],n=n))] %>% as.character #rF encodes male as 10, female as 01
    mask =  column_to_split %in% split_point
    left_daughter = rownames(in_node_df)[mask]
    right_daughter = rownames(in_node_df)[!mask]
    
    if (verbose >0) {diagnostics(in_node_df, left_daughter, right_daughter, split_var)} 
    
  } else{
    split_point = tree_r[,'split point']
    mask = column_to_split <= split_point
    left_daughter = rownames(in_node_df)[mask]
    right_daughter = rownames(in_node_df)[!mask]
    if (verbose >0) {diagnostics(in_node_df, left_daughter, right_daughter, split_var)} 
  }
  return(list(left_daughter = (left_daughter), right_daughter = (right_daughter)))
}

# Main function
get_node_elements <- function(tree, bag, verbose=1){
  # create new column for storing the index of data in the node
  tree$node = NA
  tree$node[1] = list(rownames(bag)) # initialise the first row by all the inbag elements i.e. before any split
  tree$gini_index = NA
  tree$gini_index[1] = gini_process(bag[tree$node[[1]], 'Survived'])
  
  # filter out non-terminal nodes, which their 'split var' = NA
  mask = is.na(tree[,'split var'])
  non_terminal_node_in_tree = c(1:nrow(tree))[!mask]
  
  if (verbose >2) browser()
  # Loop over non terminal nodes
  for (i in non_terminal_node_in_tree){
    left_daughter = tree[i, 'left daughter']
    right_daughter = tree[i, 'right daughter']
    node_to_split_df = bag[unlist(tree$node[i]),]
    if (verbose>0){ print(paste('----------On #', i, ' node in tree----------', sep = ''), quote=F)}
    left_and_right_daughter = split_bag_from_node(in_node_df = node_to_split_df, tree_r = tree[i,], verbose=verbose)
    
    tree$node[left_daughter] = list(left_and_right_daughter$left_daughter)
    tree$node[right_daughter] = list(left_and_right_daughter$right_daughter)
    nLeft = length(left_and_right_daughter$left_daughter)
    nRight = length(left_and_right_daughter$right_daughter)
    
    tree$gini_index[left_daughter] = gini_process(bag[tree$node[[left_daughter]], 'Survived'])
    tree$gini_index[right_daughter] = gini_process(bag[tree$node[[right_daughter]], 'Survived'])
    
    if (verbose >0) {
      
      childGini = (tree$gini_index[left_daughter]*nLeft + tree$gini_index[right_daughter]*nRight)/(nLeft+nRight)
      if (childGini >= tree$gini_index[i] ) {
      cat("WARNING, GINI DID NOT DECREASE for node", i, "\n")
      cat("parent, Gini", tree$gini_index[i], "\n")
      cat("left_daughter, Gini", tree$gini_index[left_daughter], "\n")
      cat("right_daughter, Gini", tree$gini_index[right_daughter], "\n\n")
      } 
      
      else{
      cat('')
      cat('GINI decrease in this node', "\n\n")}
      
    }
  } 
  return(tree)
}

#Gini index 
gini_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,2]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

#tests:

if (0){
  gini_process(titanic_train[, 'Survived'])#0.473013
  gini_process(subset(titanic_train, Sex==1)[, 'Survived'])#0.382835
  gini_process(subset(titanic_train, Sex==2)[, 'Survived'])#0.3064437
  
}