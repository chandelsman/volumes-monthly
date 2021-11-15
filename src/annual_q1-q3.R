by_year <- 
  volumes %>% 
  filter(!is.na(cl_grp), 
         mth != "Oct",
         mth != "Nov",
         mth != "Dec") %>% 
  group_by(yr, cl_grp) %>% 
  summarize(n = sum(cases, na.rm = TRUE))

ggplot(by_year, aes(x = factor(yr), y = n, fill = yr)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  facet_wrap(~cl_grp, 
             scales = "free",
             nrow = 2) +
  labs(
    title = paste0("Annual Volumes Q1-Q3: 2016 - ", year(Sys.Date())),
    x = "", 
    y = "Number of Cases Signed Out"
  ) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 20) + 
  theme(aspect.ratio = 1)
  # theme(axis.text.x = element_text(size = 12,
  #                                  angle = 0, hjust = .5, vjust = .5),
  #       axis.text.y = element_text(size = 12),
  #       strip.text = element_text(size = 12),
  #       strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
  #       text = element_text(size = 16))

# Save PDF of plot
ggsave(filename = 
         here("output", 
              paste0(
                "Annual_2016-", 
                year(Sys.Date()), 
                ".pdf"
                )
              ),
       height = 20, width = 30, units = "in", dpi = 500)
       