## Load data and explore

library(devtools)
library(ggiraph)

devtools::load_all("../biplaRseb")

pool24 <- get_seb(2024) %>%
  dplyr::select(
    type, school, profil, lgkg,
    activity, age, sex,
    zufrieden, vorb_fach, vorb_pers, vorb_selbst,
    
    ict = stand_ict, starts_with("tis_")) %>%
  dplyr::mutate(year = 2024)

pool21 <- get_seb(2021) %>%
  dplyr::select(
    type, school, profil, lgkg,
    activity, age, sex,
    zufrieden, vorb_fach, vorb_pers, vorb_selbst,
    ict = stand_ict, starts_with("tis_")) %>%
  dplyr::mutate(year = 2021)

p2 <- rbind(pool24, pool21) %>% 
  rename(digi_effizient = tis_unt_eff,
         digi_besser_lernen = tis_unt_lern,
         digi_spannender = tis_unt_span,
         lehrp_beraten = tis_lehrp_bera,
         lehrp_passend = tis_lehrp_faecher,
         lehrp_mitarbeit = tis_lehrp_konz,
         lehrp_positiv = tis_lehrp_pos,
         lehrp_umgang = tis_lehrp_umge,
         digi_erk = tis_verw_erkl,
         digi_ueb_anw = tis_verw_lern,
         digi_produkte = tis_verw_prod,
         digi_gruppen_dis = tis_verw_disk,
         digi_plan = tis_verw_plan,
         digi_pruef = tis_verw_kont,
         schule_gefahren = tis_lern_gefahr,
         schule_zwecke = tis_lern_zweck,
         schule_aufträge = tis_lern_schul,
         schule_tools = tis_lern_tools,
         schule_selbständig = tis_lern_selbst,
         vor_andere = tis_vorr_andere,
         vor_eltern = tis_vorr_eltern,
         vor_inter = tis_vorr_inter,
         vor_software = tis_vorr_soft,
         vor_support = tis_vorr_support,
         vor_lehrperson = tis_vorr_unterstlp)

seb24 <- get_seb(2024, T) %>% 
  rename(digi_effizient = tis_unt_eff,
         digi_besser_lernen = tis_unt_lern,
         digi_spannender = tis_unt_span,
         lehrp_beraten = tis_lehrp_bera,
         lehrp_passend = tis_lehrp_faecher,
         lehrp_mitarbeit = tis_lehrp_konz,
         lehrp_positiv = tis_lehrp_pos,
         lehrp_umgang = tis_lehrp_umge,
         digi_erk = tis_verw_erkl,
         digi_ueb_anw = tis_verw_lern,
         digi_produkte = tis_verw_prod,
         digi_gruppen_dis = tis_verw_disk,
         digi_plan = tis_verw_plan,
         digi_pruef = tis_verw_kont,
         schule_gefahren = tis_lern_gefahr,
         schule_zwecke = tis_lern_zweck,
         schule_aufträge = tis_lern_schul,
         schule_tools = tis_lern_tools,
         schule_selbständig = tis_lern_selbst,
         vor_andere = tis_vorr_andere,
         vor_eltern = tis_vorr_eltern,
         vor_inter = tis_vorr_inter,
         vor_software = tis_vorr_soft,
         vor_support = tis_vorr_support,
         vor_lehrperson = tis_vorr_unterstlp)


names(seb24)

digi_schule_t <- p2 %>%
  filter(!is.na(profil)) %>%
  select(year, starts_with("schule")) %>%
  group_by(year) %>% 
  summarise(across(starts_with("schule"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("schule"), names_to = "indicator", values_to = "mean") %>%
  mutate(
    profil = "Total",
    indicator = case_when(
      indicator == "schule_zwecke" ~ "Nutzung digitaler Medien für private Zwecke",
      indicator == "schule_aufträge" ~ "Nutzung digitaler Medien für schulische Aufträge",
      indicator == "schule_selbständig" ~ "Nutzung digitaler Medien für selbständiges Lernen",
      indicator == "schule_gefahren" ~ "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
      indicator == "schule_tools" ~ "Kenntnis der technischen Seite digitaler Tools",
      TRUE ~ indicator),
    text = paste0(profil, ": ", round(mean, 2)),
    profil = fct_relevel(profil, "Total"),
    indicator = factor(indicator, levels = c(
      "Nutzung digitaler Medien für private Zwecke",
      "Kenntnis der technischen Seite digitaler Tools",
      "Nutzung digitaler Medien für selbständiges Lernen",
      "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
      "Nutzung digitaler Medien für schulische Aufträge")))

digi_schule_p <- p2 %>% 
  group_by(profil)%>% 
  filter(!is.na(profil)) %>%  
  select(profil, year, starts_with("schule"))%>% 
  group_by(year, profil) %>% 
  summarise(across(starts_with("schule"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("schule"), names_to = "indicator", values_to = "mean") %>%
  mutate(indicator = case_when
         (indicator == "schule_zwecke" ~ "Nutzung digitaler Medien für private Zwecke",
           indicator == "schule_aufträge" ~ "Nutzung digitaler Medien für schulische Aufträge",
           indicator == "schule_selbständig" ~ "Nutzung digitaler Medien für selbständiges Lernen",
           indicator == "schule_gefahren" ~ "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
           indicator == "schule_tools" ~ "Kenntnis der technischen Seite digitaler Tools"),
         text = paste0(profil, ": ", round(mean, 2)),
         profil = factor(profil, levels = c("Altsprachlich", "Mathematisch-Naturwissenschaftlich", "Neusprachlich", "Wirtschaftlich-Rechtlich","Musisch", "Total")),
         indicator = factor (indicator, levels = c("Nutzung digitaler Medien für private Zwecke", "Kenntnis der technischen Seite digitaler Tools", "Nutzung digitaler Medien für selbständiges Lernen", "Kenntnis der Auswirkungen und Gefahren digitaler Medien", "Nutzung digitaler Medien für schulische Aufträge"))) 

digi_schule <- bind_rows(digi_schule_p, digi_schule_t)

plot_6 <-ggplot(digi_schule, aes(x=indicator, y=mean, colour=profil, tooltip = text, data_id = profil)) +
  geom_point_interactive(size=2) +
  facet_wrap(~ year, ncol = 1) +
  coord_flip() +
  geom_jitter(width = 0.02, height = 0.02) +
  scale_y_continuous(limits = c(1, 6.1), breaks = seq(1, 6.1, 1), 
                     labels = function(y) str_wrap(c("1 sehr wenig", "2", "3", "4", "5", "6 sehr viel"), 10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 8, angle = 90), 
        axis.text.y = element_text(size = 8),  
        axis.title = element_blank(), legend.position = "bottom", legend.justification = "right", legend.title = element_blank())+
  guides(color = guide_legend(ncol = 2))

girafe(ggobj = plot_6, height_svg = 5) %>% 
  girafe_options(opts_hover("opacity:0.7;"),opts_zoom(max = 10), opts_tooltip(use_fill =TRUE))

