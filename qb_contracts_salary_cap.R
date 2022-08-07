library(rvest)
library(tidyverse)

qb_contracts <- read_html("https://overthecap.com/contract-history/quarterback") %>% 
    html_table() %>% 
    data.frame() %>% 
    select(-Var.5, -Var.9, -Var.11) %>% 
    rename(
        name               = Player,
        team               = Team,
        year_signed        = Year.Signed,
        value              = Value,
        aav                = APY,
        gtd                = Guaranteed,
        cap_pct_at_signing = APY.as...Of.Cap.At.Signing,
        inflated_value     = Inflated.Value,
        inflated_aav       = Inflated.APY,
        inflated_gtd       = Inflated.Guaranteed
        ) %>% 
    mutate(cap_pct_at_signing = cap_pct_at_signing %>% str_remove_all('%') %>% as.numeric())

nfl_yearly_salary_cap <- read_html("https://www.spotrac.com/nfl/cba/") %>% 
    html_table() %>% 
    data.frame() %>% 
    select(Year, Cap.Maximum)

qb_contracts_10_cap_pct <- qb_contracts %>% 
    select(name, team, year_signed, value, aav, cap_pct_at_signing) %>% 
    filter(cap_pct_at_signing >= 10) %>% 
    arrange(desc(year_signed))

    # bind_cols(
    #     type_of_contract =
    #         c(
    #             'ext', 'ext', 'override', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 
    #             'fa', 'fa', 'ext', 'fa', 'ext', 'ext', 'ext', 'ext', 'fa', 'ext', 'fa', 'ext', 'ext', 'fa', 'ext',
    #             'ext', 'ext', 'fa', 'fa', 'ext', 'ext', 'franchise', 'ext', 'ext', 'ext', 'ext', 'ext', 'franchise',
    #             'trade', 'ext', 'fa', 'trade', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'fa', 'ext', 'fa',
    #             'fa', 'fa', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'fa', 'ext', 'ext', 'ext', 'ext', 'fa',
    #             'ext', 'fa', 'trade', 'ext', 'ext', 'ext', 'fa', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 
    #             'ext', 'ext', 'ext', 'ext', 'ext', 'trade', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 'ext', 
    #             'draft', 'ext', 'fa', 'ext', 'fa', 'ext', 'ext', 'ext', 'fa/ext', 'ext', 'drafted', 'rfa/ext')
    #         ) %>% 
    # mutate(
    #     type_of_contract = type_of_contract %>%
    #         str_replace_all('ext', 'extension') %>% 
    #         str_replace_all('fa', 'free_agent')
    #     )