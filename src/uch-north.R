# Plot year over year cases by facility

# UCHealth North group plot ------
volumes %>%
  filter(cl_grp == "UCH North Hospitals") %>% 
  group_by(yr, mth) %>% 
  summarize(n = sum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(x = mth, y = n, group = yr, color = as.factor(yr))) +
  geom_point(size = 3) + 
  geom_line(
    size = 1.5,
    linetype = "solid",
    na.rm = TRUE,
  ) +
  scale_x_discrete(
    breaks = volumes$mth, 
    labels = volumes$mth
  ) +
  labs(
    title = "UCHealth North Facilities: 2016 - 2020",
    x = "", 
    y = "Number of Cases Signed Out", 
    color = "Year"
  ) + 
  theme_bw(base_size = 20) + 
  theme(aspect.ratio = 1) +
  # theme(axis.text.x = element_text(size = 12, 
  #                                  angle = 0, hjust = .5, vjust = .5),
  #       axis.text.y = element_text(size = 12),
  #       strip.text = element_text(size = 12),
  #       strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
  #       text = element_text(size = 16)) + 
  scale_color_brewer(palette = "Paired")
ggsave(filename = "output/uch-north_all.pdf",
       height = 20, width = 20, units = "in", dpi = 500)

# UCHealth North hospitals
volumes %>% 
  filter(cl_grp == "UCH North Hospitals", cases > 0) %>%
  group_by(facility, yr, mth) %>% 
  summarize(n = sum(cases)) %>% 
  ggplot(aes(x = mth, y = n, group = yr, color = as.factor(yr))) +
  geom_point(size = 3) + 
  geom_line(
    size = 1.5,
    linetype = "solid",
    na.rm = TRUE,
  ) +
  scale_x_discrete(
    breaks = volumes$mth, 
    labels = volumes$mth
  ) +
  facet_wrap(
    ~ facility,
    nrow = 2, 
    scales = "free_y"
  ) +
  labs(
    title = "UCHealth North Facilities: 2016 - 2020",
    x = "", 
    y = "Number of Cases Signed Out", 
    color = "Year"
  ) + 
  theme_bw(base_size = 20) + 
  theme(aspect.ratio = 1) +
  # theme(axis.text.x = element_text(size = 12, 
  #                                  angle = 0, hjust = .5, vjust = .5),
  #       axis.text.y = element_text(size = 12),
  #       strip.text = element_text(size = 12),
  #       strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
  #       text = element_text(size = 16)) + 
  scale_color_brewer(palette = "Paired")
ggsave(filename = "output/uch-north_facilities.pdf",
       height = 20, width = 30, units = "in", dpi = 500)
