plot_participants_change <- function(refs_adult_list, refs_youth_list, single_sex = FALSE, 
                                     metric1="fev1", metric2="fvc", includedRace=c(1,2,3,4,5,6,7)) {
  
  # Combine refs_df_adult and refs_df_youth
  combine_refs_df <- function(refs_list) {
    refs_df <- do.call(rbind, lapply(seq_along(refs_list), function(i) refs_list[[i]]$variables))
    refs_df %>%
      mutate(
        # ref=ifelse(ref!="reference", str_c("+ ", ref), ref), 
        ref=factor(ref, levels=c("reference",
                                 "+ job unexposed", 
                                 "+ physically active", 
                                 "+ no maternal smoke", 
                                 "+ no 2ndhand smoke", 
                                 "+ not obese", 
                                 "+ homeowner", 
                                 "+ insured",
                                 "+ educated", 
                                 "+ healthy diet"))) %>%
      group_by(ref) %>%
      count(race_text_nhanes, wt = MEC6YR) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()
  }
  
  refs_df_percent_adult <- combine_refs_df(refs_adult_list) 
  refs_df_percent_youth <- combine_refs_df(refs_youth_list)
  
  plot_gap_perc <- function(refs_df_percent, title, suppress_legend=FALSE) {
    p <- ggplot(refs_df_percent) +
      geom_bar(aes(x=ref, y=n, fill=race_text_nhanes), stat="identity", position = "stack") +
      geom_text(aes(x=ref, y=n, fill=race_text_nhanes, label=scales::percent(perc, accuracy = 1)), 
                position = position_stack(vjust = 0.5), color = "white", size = 2) +
      labs(x = "", y = "Representative Population Size") +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))  +
      ggtitle(title) +
      guides(fill=guide_legend(title="Race")) +
      theme_few() + theme(axis.text.x = element_text(angle = 90, size=7, hjust=0.95,vjust=0.2))
    
    if (suppress_legend) {
      p <- p +  theme(legend.position="none")
    } else {
      p <- p +  theme(legend.position="bottom") 
    }
    return(p)
  }
  
  gap_perc_black_adult <- plot_gap_perc(refs_df_percent_adult, "Adults (≥20 yrs)")
  gap_perc_black_youth <- plot_gap_perc(refs_df_percent_youth, "Children (6-19 yrs)")
  
  p <- (((gap_perc_black_youth) + 
           plot_layout(axes = "collect")) | 
          ((gap_perc_black_adult) + 
             plot_layout(axes = "collect")) ) + 
    plot_annotation(tag_levels = 'A') 
  
  p <- ((gap_perc_black_youth + gap_perc_black_adult +  
           plot_layout(axes="collect"))/guide_area()) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")
  return(p)
}

p <- plot_participants_change(refs_adult_all_races, refs_youth_all_races, includedRace = c(1,2,3,4,5,6,7))

ggsave("plot1.pdf", p, width=10, height=8, units="in", dpi=300)