#' Clean PRL raw data for modelling use
#'
#' This function takes the CAPR raw prl data and transform it into a long format 
#' version ready to use in computational modelling (i.e., trial by trial), also 
#' a second output is in wide format containing win or loss and stay or switches. 
#' Finally, it contains an exclusion criterion based on a random permutation of 
#' rewards given random responses in 160 trials of PRL (see use of p_threshold).
#'
#' @param prl_raw data.frame, raw data from getTask function
#' @param p_threshold Numeric, compare the participant ith p(reward) against the null distribution
#' @return a list with two data.frames one long the other wide.
#' @examples
#' dataRequest("prl_long_wide")
#' 
#' #' @author Santiago Castiello De Obeso <santiago.castiellodeobeso@yale.edu>

cleanPrl <- function(prl_raw, p_threshold = .05) {
  # participants vector
  participants <- unique(prl_raw$PROLIFIC_PID)
  participants <- participants[-1]
  # p_threshold <- 0.5
  
  # nda relevant columns
  nda_vars <- c("PROLIFIC_PID","interview_date")
  
  # relevant columns
  rel_cols <- c(nda_vars,"index","deck_probabilities","key_press","response","reward_type")
  
  # counter how many bad participants (trials with no response)
  counter <- 0
  
  # # # # Null distribution with random permutations (see p_chance_game.R) # # # # 
  # we are importing the null distribution with 10000 permutations. From an agent 
  # playing the PRL completely at chance. We will use this to classify good from 
  # bad participants.
  p_correct_at_chance <- read.table("null_distribution.txt", col.names=F)
  # number of permutations
  n_permutations <- nrow(p_correct_at_chance)
  
  # for looped them to clean and combine
  for (i in 1:length(participants)) {
    # read file ith
    temp <- prl_raw[prl_raw$PROLIFIC_PID==participants[i],]
    temp <- subset(temp, !is.na(index))
    
    # change variables
    temp$reward_type <- ifelse(tolower(temp$reward_type)=="true",1,0)
    
    # Is this a good participant? 
    # option A. Binomial test
    # test <- binom.test(sum(temp$reward_type),nrow(temp),1/2)
    # p.value <- test$p.value
    # option B.random permutation (see p_chance_game.R). More elegant!
    p.value <- sum(p_correct_at_chance > mean(temp$reward_type))/n_permutations
    
    if (p_threshold > p.value) {
      # get only rows with information
      temp <- temp[,rel_cols]
      
      # change response label to response number (left=1, middle=2, right=3)
      # temp$response <- as.integer(dplyr::recode(temp$response_position, "left"="1",
      #                                           "middle"="2","right"="3"))
      # temp$response_position <- NULL
      
      # # # create wide format with win-switch rate and loose-stay rate # # #
      temp_wide <- winLossSwitchStay(outcome = temp$reward_type,
                                     response = temp$response)
      temp_wide <- data.frame(temp[1,nda_vars],temp_wide)
      
      # # # create long format with probabilities in each column # # #
      # Split the string_column into a list of vectors
      split_strings <- strsplit(temp$deck_probabilities, ",")
      # Convert the list to a data.frame with three columns
      split_df <- do.call(rbind, split_strings)
      # remove punctuation
      split_df <- gsub("\\[|\\]", "", split_df)
      # # Rename the new columns
      # colnames(split_df) <- c("left", "middle", "right")
      # for (j in 1:ncol(split_df)) {
      #   split_df[1:80,j] <- as.numeric(recode(split_df[1:80,j],"ulow"=".1","umedium"=".5","uhigh"=".9"))
      #   split_df[81:160,j] <- as.numeric(recode(split_df[81:160,j],"ulow"=".2","umedium"=".4","uhigh"=".8"))
      # }
      
      # Assign the new columns back to the original data.frame
      temp_long <- cbind(temp, split_df)
      
      # combine all participants
      if (i == 1 | !exists("lf")) {
        lf <- temp_long
        wf <- temp_wide
      } else {
        lf <- rbind(lf,temp_long)
        wf <- rbind(wf,temp_wide)
      }
      
      # if bad participant  
    } else {
      counter <- counter + 1
      message(paste0(counter,"; bad number of trials: ", participants[i]))
    }
  } # end for
  
  # calculate rates
  wf$w_sw_r <- wf$win_switch / (wf$win_switch + wf$win_stay)
  wf$w_st_r <- wf$win_stay / (wf$win_switch + wf$win_stay)
  wf$l_sw_r <- wf$loss_switch / (wf$loss_switch + wf$loss_stay)
  wf$l_st_r <- wf$loss_stay / (wf$loss_switch + wf$loss_stay)
  
  # return behaviour (long format) and scores (wide format)
  return(list(lf=lf,wf=wf))
  return(as.data.frame)
}
