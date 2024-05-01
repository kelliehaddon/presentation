# Presentation

# Load packages
library(tidyverse)
library(haven)
library(forcats)
library(scales)
library(showtext)
library(knitr)

# Read data
 # https://github.com/fivethirtyeight/data/tree/master/college-majors
df = read_csv('recent-grads.csv')

# description
sum(df$Women, na.rm = T)

df %>%
  count(Major_category)
Variable = c('Total', 'Men', 'Women', 'Sample', 'Majors', 'Major Categories')
n = c('6,771,654', '2,876,426', '3,895,228', '61,602', '173', '16')

df2 = tibble(Variable, n)

kable(df2)


# bar chart showing the share of women in each major category
df3 <- df %>%
  group_by(Major_category) %>%
  summarise(ShareWomen = mean(ShareWomen, na.rm = TRUE))

df3 %>%
  ggplot(aes(x = reorder(Major_category, +ShareWomen), y = ShareWomen)) + 
  geom_col(fill = '#41afaa') +
  coord_flip() +
  labs(x = ' ', y = 'Share of Women', caption = ' ', title = 'Share of Women by Major Category',
       subtitle = 'Percentage of full time workers who are women in each major category') +
  geom_text(aes(label = paste0(round(ShareWomen * 100, 1), "%")), 
            vjust = 0.5, hjust = -0.2, color = "black", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 6),
        axis.text.y = element_text(color = 'black', size = 6),
        axis.title.x = element_text(color = 'black', size = 8),
        axis.title.y = element_text(color = 'black', size = 8))


  # old
df %>%
  filter(Rank < 101) %>%
  ggplot(aes(fct_infreq(Major_category))) +
  geom_bar(fill = '#41afaa', alpha = 0.6, color = '#41afaa') +
  labs(x = ' ', y = ' ', caption = ' ', title = '') +
  geom_text(stat='count', aes(label = ..count..), vjust = -0.7, size = 2, color = 'black') +
  scale_y_continuous(limits = c(0, 35)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1.15, hjust = 1, color = 'black', size = 6),
        panel.grid = element_blank(),
        axis.text.y = element_blank())



# scatter plot showing the share of women in majors vs their earnings, by STEM
  # https://www.ice.gov/sites/default/files/documents/stem-list.pdf
df = df %>%
  mutate(STEM = (case_when(Major_category == 'Agriculture & Natural Resources' ~ 'STEM',
                           Major_category == 'Arts' ~ 'Non-STEM',
                           Major_category == 'Biology & Life Science' ~ 'STEM',
                           Major_category == 'Business' ~ 'Non-STEM',
                           Major_category == 'Communications & Journalism' ~ 'Non-STEM',
                           Major_category == 'Computers & Mathematics' ~ 'STEM',
                           Major_category == 'Education' ~ 'Non-STEM',
                           Major_category == 'Engineering' ~ 'STEM',
                           Major_category == 'Health' ~ 'Non-STEM',
                           Major_category == 'Humanities & Liberal Arts' ~ 'Non-STEM',
                           Major_category == 'Industrial Arts & Consumer Services' ~ 'Non-STEM',
                           Major_category == 'Interdisciplinary' ~ 'Non-STEM',
                           Major_category == 'Law & Public Policy' ~ 'Non-STEM',
                           Major_category == 'Physical Sciences' ~ 'STEM',
                           Major_category == 'Psychology & Social Work' ~ 'Non-STEM',
                           Major_category == 'Social Science' ~ 'Non-STEM')))

df$STEM = as_factor(df$STEM)

    # full population
df %>%
  ggplot(aes(x = ShareWomen, y = Median)) +
  geom_point(color = '#af4b91', alpha = 0.3, size = 3) +
  geom_smooth(se = F, color = '#af4b91') +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = 'Share of Women', 
       y = 'Median Earnings',
       title = 'Share of Women vs. Median Earnings',
       subtitle = 'Majors plotted by the share of full time workers who are women \nand the median earnings of workers in each major') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

  # old
df %>%
  ggplot(aes(x = ShareWomen, y = Median)) +
  geom_point(color = '#41afaa', alpha = 0.6) +
  geom_smooth(se = F, color = '#41afaa') +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = 'Share of Women', 
       y = 'Median Earnings') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))




    # sub-group STEM
df %>%
  ggplot(aes(x = ShareWomen, y = Median, color = STEM)) +
  geom_point(alpha = 0.3, size = 3) +
  geom_smooth(se = F) +
  facet_wrap(~ STEM) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values = c('#d7642c', '#e6a532')) +
  labs(x = 'Share of Women', 
       y = 'Median Earnings',
       title = 'Share of Women vs. Median Earnings',
       subtitle = 'Majors plotted by the share of full time workers who are women \n and the median earnings of workers in each major, by degree type') +
  guides(color = FALSE) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 8, face = 'bold'),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.margin = unit(1, "lines"))

  # old
df %>%
  ggplot(aes(x = ShareWomen, y = Median, color = STEM)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = F) +
  facet_wrap(~ STEM) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values = c('#d7642c', '#af4b91')) +
  labs(x = 'Share of Women', 
       y = 'Median Earnings') +
  guides(color = FALSE) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 8, face = 'bold'),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))