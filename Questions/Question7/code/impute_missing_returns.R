# deal with missing values

impute_missing_returns <- function(return_mat_q7, impute_returns_method = "NONE",
                                   Seed = 1234){
    # Make sure we have a date column called date:
    if( !"date" %in% colnames(return_mat_q7) ) stop("No 'date' column
provided in return_mat_q7. Try again please.")

    # Note my use of 'any' below...
    # Also note that I 'return' return_mat_q7 - which stops the function and returns return_mat_q7.
    if( impute_returns_method %in% c("NONE", "None", "none") ) {
        if( any(is.na(return_mat_q7)) ) warning("There are missing values in the return matrix
Consider maybe using impute_returns_method =
'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
        return(return_mat_q7)
    }

    if( impute_returns_method  == "Average") {
        return_mat_q7 <-
            return_mat_q7 %>% gather(Stocks, Returns, -date) %>%
            group_by(date) %>%
            mutate(Avg = mean(Returns, na.rm=T)) %>%
            mutate(Avg = coalesce(Avg, 0)) %>%
            ungroup() %>%
            mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>%
            spread(Stocks, Returns)

    } else

        if( impute_returns_method  == "Drawn_Distribution_Own") {

            set.seed(Seed)
            N <- nrow(return_mat_q7)
            return_mat_q7 <- left_join(return_mat_q7 %>% gather(Stocks, Returns, -date),
                                    return_mat_q7 %>% gather(Stocks, Returns, -date) %>% group_by(Stocks) %>%
                                        do(Dens = density(.$Returns, na.rm=T)) %>%
                                        ungroup() %>% group_by(Stocks) %>% # done to avoid warning.
                                        do(Random_Draws = sample(.$Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y)),
                                    by = "Stocks") %>%
                group_by(Stocks) %>% mutate(Row = row_number()) %>%
                mutate(Returns = coalesce(Returns, Random_Draws[[1]][Row])) %>%
                select(-Random_Draws, -Row) %>% ungroup() %>% spread(Stocks, Returns)

        } else

            if( impute_returns_method  == "Drawn_Distribution_Collective") {
                set.seed(Seed)
                NAll <- nrow(return_mat_q7 %>% gather(Stocks, Returns, -date))

                return_mat_q7 <-
                    bind_cols(
                        return_mat_q7 %>% gather(Stocks, Returns, -date),
                        return_mat_q7 %>% gather(Stocks, Returns, -date) %>%
                            do(Dens = density(.$Returns, na.rm=T)) %>%
                            do(Random_Draws = sample(.$Dens[[1]]$x, NAll, replace = TRUE,
                                                     prob=.$Dens[[1]]$y)) %>% unnest(Random_Draws)) %>%
                    mutate(Returns = coalesce(Returns, Random_Draws)) %>%
                    select(-Random_Draws) %>% spread(Stocks, Returns)

            } else

                if( impute_returns_method  == "Zero") {
                    warning("This is probably not the best idea but who am I to judge....")
                    return_mat_q7[is.na(return_mat_q7)] <- 0

                } else
                    stop("Please provide a valid impute_returns_method method.
Options include:\n'Average', 'Drawn_Distribution_Own',
'Drawn_Distribution_Collective' and 'Zero'.")
}