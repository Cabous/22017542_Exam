
rolling_cor_func <- function(data = df, time_frame = 60){

    pairwise_combos <- data %>%

        full_join(data, by = "date")

    pairwise_combos <- pairwise_combos %>%

        ungroup() %>%

        # drop diagonal
        filter(Tickers.x != Tickers.y) %>%

        # remove duplicate pairs

        mutate(Tickers = ifelse(Tickers.x < Tickers.y,
                                glue("{Tickers.x}, {Tickers.y}"),
                                glue("{Tickers.y}, {Tickers.x}"))) %>%

        distinct(date, Tickers, .keep_all = TRUE)

period <- time_frame

pairwise_corrs <- pairwise_combos %>%

        group_by(Tickers) %>%

        arrange(date, .by_group = TRUE) %>%

        mutate(rollingcor = slider::slide2_dbl(
            .x = Return.x,
            .y = Return.y,
            .f = ~cor(.x, .y),
            .before = period,
            .complete = TRUE)
        ) %>%

        select(date, Tickers, rollingcor)

pairwise_corrs[is.na(pairwise_corrs)] <- 0

pairwise_corrs

}