pretty_table <- function(df) {

    require(kableExtra)

    df %>%

        kable() %>%

        kable_styling(full_width = TRUE, position = 'center') %>%

        scroll_box(height = '300px')

}