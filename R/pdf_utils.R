#' Function to print PDF table
#'
#' @param dat data.frame dati
#' @param footnote_arg list, footnotes info
#'
#' @import kableExtra dplyr
#'
#' @return kableExtra object
#' @export
#'
kable_fun <- function(dat, footnote_arg = list(general_title =  "gg: Giorni Stagionatura", general = "" )) {
  kableExtra::kable(dat, #align = 'l',
                    booktabs = TRUE, format = "latex", escape = T) %>%
    #kable_paper(full_width = F) %>%
    kable_classic("striped", full_width = F) %>%

    # column_spec(3, image = spec_image("LaFonte_IClassici1024_1.png", 1150, 1150)) %>%
    #     knitr::knit_print()#%>%
    column_spec(1, width = "3.25cm", bold = F, italic = T) %>%
    column_spec(2, width = "2.25cm") %>%
    collapse_rows(columns=1:3, valign="top", row_group_label_position = 'identity', latex_hline = "custom", custom_latex_hline = 1)  %>%
    kable_styling(position = "left",  latex_options = c("hold_position"), html_font = "Ubuntu Bold", font_size = 9.5) %>% row_spec(row = 0:1, bold = TRUE) %>% footnote(footnote_arg)
}
