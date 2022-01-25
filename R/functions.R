#' View Corner of a Matrix
#'
#' This function does for a matrix what head() does for a data frame.
#' @param x Matrix to view
#' @param min Minimum row and column indices to view.  Defaults to c(0,0)
#' @param max Maximum row and column indices to view.  Defaults to c(10,10)
#' @keywords classification
#' @export
#' @examples
#' plotTermCoefs()

corner <- function(x,
                   min=c(0,0),
                   max=c(10,10)
                   ) {
  return(x[c(min[1]:max[1]),c(min[2]:max[2])])
}


#' Rescale Variable
#'
#' A function to rescale a numeric input to the 0-1 interval.
#' @param input Numeric input to rescale
#' @keywords utility
#' @export
#' @examples
#' myRescale()

myRescale <- function(input){
  newinput <- (input-min(input, na.rm = T))
  return((newinput)/max(newinput, na.rm = T))
}

#' Centered Sequence
#'
#' Make a centered sequence for histogram breaks
#' @param l Lower bound
#' @param u Upper bound
#' @param b Bandwidth
#' @keywords utility
#' @export
#' @examples
#' cSeq()


cSeq <- function(l=-2, u=2, b=.1){
  r <- c(l,u)
  return(seq(r[1]-b/2, r[2]+b/2, by=b))
}

#' Centered Histogram
#'
#' Make a centered histogram breaks.
#' @param x Data to plot.
#' @param b Bandwidth, defaults to 1.
#' @param main Plot title, defaults to NULL.
#' @keywords utility
#' @export
#' @examples
#' chist()

# chist <- function(x, b=1, main=NULL){
#   hist(x,
#        breaks = cSeq(min(x, na.rm = T), max(x, na.rm = T), b),
#        main = ifelse(test = is.null(main), yes = deparse1(substitute(x)), no = main))
# }

chist <- function(x, b=1, main=NULL){
  hist(x,
       breaks = cSeq(floor(min(x, na.rm = T)+b/2), ceiling(max(x, na.rm = T)-b/2), b),
       main = ifelse(test = is.null(main), yes = deparse1(substitute(x)), no = main))
}


#
# #explore extreme values
# extremes <- function(input, n=10){
#   my_length <- length(input)
#   my_order <- order(input, decreasing = T)
#   return(rbind(cbind(rownames(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(rownames(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))
# }
#
# extremes <- function(input, n=10){
#   my_length <- length(input)
#   my_order <- order(input, decreasing = T)
#   if(is.array(input)){return(rbind(cbind(rownames(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(rownames(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))}
#   if(class(input)=="dgCMatrix"){return(rbind(cbind(rownames(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(rownames(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))}
#   if(is.vector(input)){return(rbind(cbind(names(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(names(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))}
# }

#
# #####
#
# #generate colors for a base plot by a categorical variable
# plotColors <- function(data, colors=c("red","blue","orange","green","cyan","brown", "black", "magenta", "purple"), seed=4711){
#   set.seed(4711)
#   color_list <- sample(colors)
#   data_levels <- unique(data)
#   out_vector <- rep(NA, length(data))
#   for (i in 1:length(out_vector)) {
#     out_vector[i] <- colors[which(data_levels==data[i])]
#   }
#   return(out_vector)
# }
#
# #cosine similarity
# cosineSimilarity <- function(a, b){
#   return(t(a)%*%b/(sqrt(sum(diag(a %*% t(a))))*sqrt(sum(diag(b %*% t(b))))))
# }
#
# #generate random colors
# randomColors <- function(my_count=10, my_seed=7, max_lightness=2, opacity=.8){
#   set.seed(my_seed)
#   output <- rep(NA, my_count)
#   for(i in 1:my_count){
#     for(j in 1:20){
#       attempt <- c(runif(1),runif(1),runif(1))
#       if (sum(attempt)<max_lightness){break}
#     }
#     output[i] <- rgb(attempt[1],attempt[2],attempt[3],opacity)
#   }
#   return(output)
# }
#
#
# randomColors <- function(my_count=2, my_seed=7, max_lightness=5, min_lightness=0, opacity=.75, max_similarity=.95){
#   attempt <- rep(NA,3)
#   set.seed(my_seed)
#   stored <- matrix(NA, my_count, 3)
#   output <- rep(NA, my_count)
#   for(j in 1:1000){
#     attempt <- runif(3)
#     if ((sum(attempt)<max_lightness) & (sum(attempt)>min_lightness)){break}
#   }
#   stored[1,] <- attempt
#   output[1] <- rgb(stored[1,1],stored[1,2],stored[1,3],opacity)
#   for(j in 1:1000){
#     attempt <- runif(3)
#     if ((sum(attempt)<max_lightness) & (sum(attempt)>min_lightness) & (cosineSimilarity(attempt, stored[1,])<max_similarity)){break}
#   }
#   stored[2,] <- attempt
#   output[2] <- rgb(stored[2,1],stored[2,2],stored[2,3],opacity)
#   if (my_count>2){
#     for(i in 3:my_count){
#       for(j in 1:10000){
#         attempt <- runif(3)
#         if ((sum(attempt)<max_lightness) & (sum(attempt)>min_lightness) & (!any(apply(stored[which(!is.na(stored[,1])),], MARGIN = 1, cosineSimilarity, attempt)>max_similarity))){break}
#       }
#       stored[i,] <- attempt
#       output[i] <- rgb(stored[i,1],stored[i,2],stored[i,3],opacity)
#     }
#   }
#   return(output)
#   #return(stored)
# }
#
#
# plotColors <- function(data, colors=c("red","blue","orange","green","cyan","brown", "black", "magenta", "purple"), seed=4711){
#   set.seed(4711)
#   #color_list <- sample(colors)
#   data_levels <- unique(data)
#   color_list <- randomColors(length(data_levels))
#   out_vector <- rep(NA, length(data))
#   for (i in 1:length(out_vector)) {
#     out_vector[i] <- color_list[which(data_levels==data[i])]
#   }
#   return(out_vector)
# }
#



#' Gradients for Plots
#'
#' A function to make color gradients for plots.
#' @param input Input values to map to a color spectrum
#' @keywords graphics
#' @export
#' @examples
#' plotGradient()


# version below should be robust to NAs in the input, allow custom selection of backup color for NAs
plotGradient <- function(input, left_color=c(0,0,1,.5), right_color=c(1,0,0,.5), NA_color=c(.5,.5,.5,.5), transparency = NULL, reference_scale = NA){
  input <- myRescale(input)
  input_length <- length(input)
  input_std <- (input-min(input, na.rm=TRUE))/max(input, na.rm=TRUE) #Why isn't this line redundant with myRescale in the first line...?
  if (!is.na(reference_scale)){
    input <- myRescale(c(input,reference_scale))
    input_std <- (input-min(input, na.rm=TRUE))/max(input, na.rm=TRUE)
    input_std <- input_std[1:input_length]
  }
  lr_diff <- right_color-left_color
  l_values <- matrix(rep(left_color, input_length), nrow = input_length, ncol = 4, byrow = T)
  c_values <- input_std %*% t(lr_diff) + l_values
  out <- rep(NA, input_length)
  if (!is.null(transparency)){
    transparency_rescaled <- myRescale(transparency)
  }
  for(i in 1:input_length){
    if(any(is.na(c_values[i,]))){
      out[i] <- rgb(NA_color[1], NA_color[2], NA_color[3], NA_color[4])
    } else {
      if (is.null(transparency)) {out[i] <- rgb(c_values[i,1], c_values[i,2], c_values[i,3], c_values[i,4])
      } else {
        out[i] <- rgb(c_values[i,1], c_values[i,2], c_values[i,3], transparency_rescaled[i])
      }
    }
  }
  return(out)
}


#' Batcher (for vectors)
#'
#' A function to batch another (simple) function that takes a vector.
#' @param X Data to input
#' @param FUN Function to apply
#' @param batch_size Size of vatches
#' @param ... Additional arguments to FUN (not yet implemented)
#' @keywords efficiency
#' @export
#' @examples
#' batcher()

batcher <- function (X, FUN, batch_size, ...) {
  FUN <- match.fun(FUN)
  nBatches <- length(X)/batch_size
  nWholeBatches <- floor(nBatches)
  if (nWholeBatches < 1) {
    return(forceAndCall(1,FUN, X))
  }
  else {
    p_list <- list()
    for (i in 1:nWholeBatches) {
      if (nWholeBatches > 1) {
        pb <- txtProgressBar(min = 1, max = nWholeBatches,
                             style = 3)
        setTxtProgressBar(pb, i)
      }
      p_list[[i]] <- forceAndCall(1, FUN, X[((i -
                                                1) * batch_size + 1):((i) * batch_size)])
    }
    message("Doing final batch...")
    if (nBatches != nWholeBatches) {
      p_list[[i + 1]] <- forceAndCall(1, FUN, X[((i) *
                                                   batch_size + 1):length(X)])
    }
    return(p_list)
  }
}

#' Batcher (for arrays)
#'
#' A function to batch another (simple) function that takes an array.
#' @param X Data to input
#' @param FUN Function to apply
#' @param batch_size Size of vatches
#' @param ... Additional arguments to FUN (not yet implemented)
#' @keywords efficiency
#' @export
#' @examples
#' batcher_array()

batcher_array <- function (X, FUN, batch_size, ...) {
  FUN <- match.fun(FUN)
  nBatches <- nrow(X)/batch_size
  nWholeBatches <- floor(nBatches)
  if (nWholeBatches < 1) {
    return(forceAndCall(1,FUN, X))
  }
  else {
    p_list <- list()
    for (i in 1:nWholeBatches) {
      if (nWholeBatches > 1) {
        pb <- txtProgressBar(min = 1, max = nWholeBatches,
                             style = 3)
        setTxtProgressBar(pb, i)
      }
      p_list[[i]] <- forceAndCall(1, FUN, X[((i -
                                                1) * batch_size + 1):((i) * batch_size),])
    }
    message("Doing final batch...")
    if (nBatches != nWholeBatches) {
      p_list[[i + 1]] <- forceAndCall(1, FUN, X[((i) *
                                                   batch_size + 1):nrow(X),])
    }
    return(p_list)
  }
}
