#Figures
library(ggplot2)

# Figure 1
pre  = "NP_MGFR_screening"
post = "NP_MGFR_nacontrole"
n = nrow(donors)
long = data.frame(
  id   = rep(seq_len(n), times = 2),
  time = factor(rep(c("Pre", "Post"), each = n), levels = c("Pre","Post")),
  gfr  = c(donors[[pre]], donors[[post]])
)
long[which(long$time == 'Pre'), 'gfr'] = long[which(long$time == 'Pre'), 'gfr'] / 2
long$time = factor(long$time,
                   levels = c("Pre","Post"),
                   labels = c('SKGFR Pre-donation','SKGFR Post-donation'))
gap = 1.0
jit = 0.1
long$x0 = ifelse(long$time == "SKGFR Pre-donation", 0, gap)
set.seed(1)
u = sort(unique(long$id))
j = setNames(runif(length(u), -jit, jit), u)
long$xj = long$x0 + j[as.character(long$id)]
ggplot(long, aes(x = xj, y = gfr, group = id)) +
  geom_line(linewidth = 1.0, alpha = 0.35, color = '#F6C5C3') +
  geom_point(size = 3.0, alpha = 0.35, color = '#F6C5C3') +
  geom_boxplot(aes(group = time),
               linewidth = 1.0,
               alpha = 0.2,
               outlier.shape = NA,
               fill = 'white',
               color = "#7A160D") +
  stat_summary(aes(x = x0, group = 1, color = "Average renal resilience", linetype = "Average renal resilience"),
               fun = mean, geom = "line", linewidth = 1.0) +
  scale_color_manual(values = c("Average renal resilience" = "#7A160D"), name = NULL) +
  scale_linetype_manual(values = c("Average renal resilience" = "11"), name = NULL) +
  labs(x = NULL, y = "Measured GFR (mL/min)", title = NULL) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(0, gap),
    labels = c('Pre-donation\nSingle-kidney GFR', 'Post-donation\nSingle-kidney GFR'),
    expand = expansion(mult = 0.20)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.text  = element_text(size = 12),
        legend.position = 'top',
        legend.box.spacing = unit(0, "pt"),)
ggsave(filename = paste0(dir, 'figures/figure_1.pdf'), width = 5.0, height = 5.0, units = 'in')

# Figure 2
library(patchwork)
fig_2a = ggplot(donors, aes(x = skdelta_gfr, y = LABCORP_KETBOD1_screening)) +
  geom_point(alpha = 0.35, color = '#7A160D') +
  geom_smooth(method = "loess", se = TRUE, color = '#7A160D', fill = '#F6C5C3') +
  labs(x = 'Renal resilience (ml/min)', y = 'Ketone bodies (µmol/L)') + 
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line())
fig_2b = ggplot(donors, aes(x = skdelta_gfr_perc, y = LABCORP_KETBOD1_screening)) +
  geom_point(alpha = 0.35, color = '#7A160D') +
  geom_smooth(method = "loess", se = TRUE, color = '#7A160D', fill = '#F6C5C3') +
  labs(x = 'Renal resilience (% change)', y = 'Ketone bodies (µmol/L)') + 
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line())
panel = fig_2a | fig_2b
ggsave(filename = paste0(dir, 'figures/figure_2.pdf'), plot = panel, width = 10, height = 5.0, units = 'in')

#Figure 3
fig_3a = ggplot(donors, aes(x = skdelta_gfr, y = LABCORP_ACAC1_screening)) +
  geom_point(alpha = 0.35, color = '#600F7C') +
  geom_smooth(method = "loess", se = TRUE, color = '#600F7C', fill = '#A27AB0') +
  labs(x = 'Renal resilience (ml/min)', y = 'Acetoacetate (µmol/L)') +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line())
fig_3b = ggplot(donors, aes(x = skdelta_gfr, y = LABCORP_ACET1_screening)) +
  geom_point(alpha = 0.35, color = '#095200') +
  geom_smooth(method = "loess", se = TRUE, color = '#095200', fill = '#779B72') +
  labs(x = 'Renal resilience (ml/min)', y = 'Acetone (µmol/L)') +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line())
fig_3c = ggplot(donors, aes(x = skdelta_gfr, y = LABCORP_BHB1_screening)) +
  geom_point(alpha = 0.35, color = '#00455E') +
  geom_smooth(method = "loess", se = TRUE, color = '#00455E', fill = '#7295A1') +
  labs(x = 'Renal resilience (ml/min)', y = 'Beta-hydroxybutyrate (µmol/L)') +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line())
panel = fig_3a | fig_3b | fig_3c
ggsave(filename = paste0(dir, 'figures/figure_3.pdf'), plot = panel, width = 12, height = 5.0, units = 'in')
