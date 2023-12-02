# all product prices per producer
#'
#' @importFrom stringr str_to_title

lafonte_iclassici <- function() {
  IClassici <- paste("Sua Eccellenza",c("il Fresco", "il Semi Stagionato",
                                        "lo Stagionato"))
  stagio <- c(20, 60, 90)
  IClassici <- paste(IClassici, paste0("(", stagio, "gg)"))
  list(p_la_f_semistag_orig = 16.96,
                 p_la_f_ilfresc_orig = 15.53,
                 p_la_f_stag_orig = 18.09) %>% setNames(IClassici)

}

lafonte_gliaromatici <- function() {
  GliAromatici <- paste("Sua Eccellenza",c("Il Pepe Nero", "Il Pistacchio", "Il Tartufo", "Il Piccante", "La Castagna"))
  stagio <- c(100, 20, 20, 20, 60)
  GliAromatici <- paste(GliAromatici, paste0("(", stagio, "gg)"))

  list(  p_la_f_aro_pnero_orig =  18.43,
         p_la_f_aro_pist_orig =  18.43,
         p_la_f_aro_truf_orig =  22.81,
         p_la_f_aro_pic_orig =  17.41,
         p_la_f_aro_cast_orig =  18.43) %>% setNames(GliAromatici)
}

lafonte_gliaffinati <- function() {
  GliAffinati <- paste("Sua Eccellenza",c("Il Foglia di Noce", "Il Re Fieno", "Il Germe di grano", "Le Vinacce", "Il Peparello"))
  stagio <- c(90, 70, "60+45", "90+4", 90)
  GliAffinati <- paste(GliAffinati, paste0("(", stagio, "gg)"))

  list(
    p_la_f_aff_fognoc_orig = 18.43,
    p_la_f_aff_refie_orig = 18.65,
    p_la_f_aff_germdg_orig = 18.43,
    p_la_f_aff_vina_orig = 18.43,
    p_la_f_aff_pepa_orig = 18.43
  ) %>% setNames(GliAffinati)
}

lafonte_ilgrande_e_i_re <- function(){
  IGrandieRe <- paste("Sua Eccellenza",c("Il Grande", "Il Re Sole", "Il Re Grotta"))
  stagio <- c(90, 200, "90+60")
  IGrandieRe <- paste(IGrandieRe, paste0("(", stagio, "gg)"))

  list(
    p_la_f_grare_gra_orig = 18.31,
    p_la_f_grare_resol_orig = 22.36,
    p_la_f_grare_regrot_orig = 18.76
  ) %>% setNames(IGrandieRe)
}

lafonte_lattesiena <- function() {
  LatteSiena <- paste(c("Cecco", "Nobile", "Mangia", "Balzana", "Tolomeo", "Fieno"), "Latte Siena")
  stagio <- c(20, 40, "70", "60", 90, 70)
  LatteSiena <- paste(LatteSiena, paste0("(", stagio, "gg)"))

  list(
    p_la_f_latt_cec_orig = 16.29,
    p_la_f_latt_nob_orig = 16.78,
    p_la_f_latt_man_orig = 17.50,
    p_la_f_latt_balz_orig = 17.98,
    p_la_f_latt_tol_orig = 19.19,
    p_la_f_latt_fie_orig = 19.54
  ) %>% setNames(LatteSiena)

}

monte_san_savino <- function() {

  Mac_mon_S_Savino <- c("Tronchetto Cotto a Legna", "Finocchiona IGP")

  list(
    p_s_mons_por_orig = 11.42,
    p_s_mons_fin_orig =  9.58
  ) %>% setNames(Mac_mon_S_Savino)
}

daluca_sughi <- function() {

  products <- c("Sugo Aglione", #"Pate' Fegatini",
                "Ribollita", "Scottiglia",
                "Sugo Cinghiale", "Cinta Senese e Porri")
  grams <- c(300, #200,
             300, 300, 300, 300)
  products <- paste(products, paste0("(", grams, "gr)"))

  list(
    products =   list(
      p_luca_agl_orig = 4.52,
      # p_luca_pate_orig = 3.66,
      p_luca_ribo_orig = 4.70,
      p_luca_scot_orig = 5.18,
      p_luca_cing_orig = 5.46,
      p_luca_cintpor_orig = 5.46
    ) %>% setNames(products),
    weight = as.list(grams) %>% setNames(products)
  )
}

daluca_pate <- function() {
  products <- c("Pate' Fegatini")
  grams <- c(200)
  products <- paste(products, paste0("(", grams, "gr)"))
  list(
    products =   list(p_luca_pate_orig = 3.66) %>% setNames(products),
    weight = as.list(grams) %>% setNames(products)
  )
}

famiata <- function() {

  products <- c("carciofi in olio" =  6.3,
                "porcini in olio" = 12.7,
                "peperoni arrosto" = 5.3,
                "zuppa di ceci" = 2.9,
                "zuppa di funghi" = 5.2,
                "crema di porcini" = 5.3,
                "patè di carciofi" = 3.5,
                "ragù di cinghiale" = 3.9,
                "crostino toscano" = 4,
                "porcini secchi" = 15
                )
  grams <- c(280, 280, 280, 390, 290, 180, 180, 180, 180, 100)
  names(products) <- paste(names(products), paste0("(", grams, "gr)")) %>%
    str_to_title()

  list(
    products =   as.list(products),
    weight = as.list(grams) %>% setNames(names(products))
  )
}


DIGITS.PRICE <- 1

# Margin for B2C/B2B
B2B_B2C_Margin <- 1.25

#' Calcolo prezzi
#'
#' @param pec numeric cost
#' @param marg numeric margin on cost
#' @param digits integer rounding
#' @param dazi numeric dazi
#' @param b2b logical if b2b prices = 1
#'
#' @return numeric vector
#' @export
#'
price_calc <-  function(pec,  marg = 2, digits = DIGITS.PRICE, dazi = 0, b2b = TRUE) {
  # (costo con dazi doganali) * margin, where margin is >1
  b2b_b2c <- ifelse(b2b, 1, B2B_B2C_Margin)
  round(marg * (pec + pec* dazi) * b2b_b2c, digits)
}
