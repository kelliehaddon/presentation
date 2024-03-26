# Presentation

library(tidyverse)
library(readxl)

# prepare data
ph = read_xlsx('philippines.xlsx')

ph_wide = ph %>%
  pivot_wider(names_from = "indicator",
              values_from = "value")

ph_wide = 
  ph_wide %>%
  rename(
    tfr = `Total fertility rate 15-49`,
    contraception = `Married women currently using any method of contraception`,
    modern = `Married women currently using any modern method of contraception`,
    need_fp = `Unmet need for family planning`,
    demand_fp = `Demand for family planning satisfied by modern methods`,
    marriage = `Median age at first marriage [Women]: 25-49`,
    sex = `Median age at first sexual intercourse [Women]: 25-49`,
    vacc = `Received all 8 basic vaccinations`,
    bf = `Median duration of exclusive breastfeeding`,
    hiv = `Women receiving an HIV test and receiving test results in the last 12 months`,
    violence = `Physical or sexual violence committed by husband/partner`,
    education = `Women with secondary or higher education`,
    literate = `Women who are literate`,
    hh_electricity = `Households with electricity`,
    pop_electricity = `Population with electricity`
  )

clean = na.omit(ph_wide)

clean %>%
  count(region)

clean$region <- sub("[(].*", "", clean$region)
clean$region <- sub("*.[Regions : ].*", "", clean$region)

# -------------------------------------------------------------

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

font_import()

loadfonts()

showtext_auto()

font_add(family = 'Seaford Display', regular = 'C:/Users/kelliehaddon/Library/Group Containers/UBF8T346G9.Office/FontCache/4/CloudFonts/Seaford Display')

font_import(paths = "/Users/kelliehaddon/Library/Fonts/19111619463.ttf")
fonts()
font <- "/Users/kelliehaddon/Library/Fonts/19111619463.ttf"

font_import(prompt = FALSE)

# description
sum(df$Women, na.rm = T)

df %>%
  count(Major_category)
Variable = c('Total', 'Men', 'Women', 'Sample', 'Majors', 'Major Categories')
n = c('6,771,654', '2,876,426', '3,895,228', '61,602', '173', '16')

df2 = tibble(Variable, n)

kable(df2)


# bar chart showing the frequency of majors in the top 100 by median earning by major category
df %>%
  filter(Rank < 101) %>%
  ggplot(aes(fct_infreq(Major_category))) +
  geom_bar(fill = '#41afaa', alpha = 0.6, color = '#41afaa') +
  labs(x = ' ', y = ' ', caption = ' ', title = 'Major Categories of Top 100 Earning Majors') +
  geom_text(stat='count', aes(label=..count..), vjust=-1, family = 'Courier New') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1.05, hjust = 1, color = 'black', size = 10,
                                   family = 'Seaford Display'),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        title = element_text(size = 12, family = 'Seaford Display', face = 'bold'))


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
  geom_point(color = '#41afaa', alpha = 0.6) +
  geom_smooth(se = F, color = '#41afaa') +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = 'Share of Women', 
       y = 'Median Earnings',
       title = 'Share of Women vs. Median Earnings') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, family = 'Courier New'),
        axis.text.y = element_text(size = 10, family = 'Courier New'),
        axis.title.x = element_text(size = 12, family = 'Courier New'),
        axis.title.y = element_text(size = 12, family = 'Courier New'),
        title = element_text(size = 14, face = 'bold', family = 'Courier New'))

    # sub-group STEM
df %>%
  ggplot(aes(x = ShareWomen, y = Median, color = STEM)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = F) +
  facet_wrap(~ STEM) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values = c('#d7642c', '#af4b91')) +
  labs(x = 'Share of Women', 
       y = 'Median Earnings',
       title = 'Share of Women vs. Median Earnings',
       subtitle = 'by Degree Type') +
  guides(color = FALSE) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 12, face = 'bold', family = 'Courier New'),
        axis.text.x = element_text(size = 10, family = 'Courier New'),
        axis.text.y = element_text(size = 10, family = 'Courier New'),
        axis.title.x = element_text(size = 12, family = 'Courier New'),
        axis.title.y = element_text(size = 12, family = 'Courier New'),
        title = element_text(size = 14, face = 'bold', family = 'Courier New'))


