# Loading library
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggridges)
library(tidyr)
library(slider)
library(forcats)
library(broom)
library(ggeffects)
library(scales)
library(stringi)
library(ggsci)
library(patchwork)
library(sf)
library(ggrepel)

################################################################################
                                # Preprocess
################################################################################

# -------------------------------------------
# 1. PARAMETERS & SETTINGS
# --------------------------------------------------
param <- list(
  # Data file path
  data_path = "/home/bao/Project/OUCRU/Manifest_Vac_Campaign/data_Bao.rds",
  
  # Selected provinces for filtering
  selected_provinces = c("Ha Giang", "Thai Nguyen", "Phu Tho",
                         "Bac Giang", "Quang Ninh", "Yen Bai", "Son La", "Hoa Binh",
                         "Ha Noi", "Ha Nam", "Bac Ninh", "Hai Duong", "Hai Phong",
                         "Hung Yen", "Nam Dinh", "Ninh Binh", "Thai Binh", "Vinh Phuc", "Thanh Hoa", "Nghe An"),
  # Selected district for filtering
  selected_district = c("huyen bac quang", "huyen dong van", "huyen hoang su phi", "huyen meo vac", "huyen quan ba", "huyen quang binh", "huyen vi xuyen", "huyen yen minh",
                                              #"huyen ham yen", "huyen son duong", "huyen yen son", "thanh pho tuyen quang",
                                              "huyen dai tu", "huyen dinh hoa", "huyen dong hy", "huyen phu binh", "huyen phu luong","thanh pho song cong", "thanh pho thai nguyen", "thi xa pho yen",
                                              "huyen cam khe", "huyen doan hung", "huyen ha hoa", "huyen lam thao", "huyen phu ninh", "huyen tam nong", "huyen tan son", "huyen thanh ba", "huyen thanh thuy", "huyen yen lap", "thanh pho viet tri", "thi xa phu tho",
                                              "huyen hiep hoa", "huyen lang giang", "huyen luc nam", "huyen luc ngan", "huyen son dong", "huyen tan yen", "huyen viet yen", "huyen yen dung", "huyen yen the", "thanh pho bac giang",
                                              "thanh pho cam pha", "thanh pho ha long", "thanh pho mong cai", "thanh pho uong bi", "thi xa dong trieu", "thi xa quang yen",
                                              "huyen luc yen", "huyen tram tau", "huyen tran yen", "huyen van chan", "huyen van yen", "huyen yen binh", "thanh pho yen bai", "thi xa nghia lo",
                                              "huyen bac yen", "huyen mai son", "huyen moc chau", "huyen muong la", "huyen quynh nhai", "huyen song ma", "huyen sop cop", "huyen thuan chau", "huyen van ho", "huyen yen chau", "thanh pho son la",
                                              "huyen cao phong","huyen ky son",  "huyen lac thuy", "huyen luong son", "huyen mai chau", "huyen tan lac", "huyen yen thuy", "thanh pho hoa binh",
                                              "huyen ba vi", "huyen chuong my", "huyen dan phuong", "huyen dong anh", "huyen gia lam", "huyen hoai duc", "huyen me linh", "huyen my duc", "huyen phu xuyen", "huyen phuc tho", "huyen quoc oai", "huyen soc son", "huyen thach that", "huyen thanh oai", "huyen thanh tri", "huyen thuong tin", "huyen ung hoa", "quan ba dinh", "quan bac tu liem", "quan cau giay", "quan dong da", "quan ha dong", "quan hai ba trung", "quan hoan kiem", "quan hoang mai", "quan long bien", "quan nam tu liem", "quan tay ho", "quan thanh xuan", "thi xa son tay",
                                              "huyen binh luc", "huyen duy tien", "huyen kim bang", "huyen ly nhan", "huyen thanh liem", "thanh pho phu ly",
                                              "huyen gia binh", "huyen luong tai", "huyen que vo", "huyen thuan thanh", "huyen tien du", "huyen yen phong", "thanh pho bac ninh", "thi xa tu son",
                                              "huyen binh giang", "huyen cam giang", "huyen gia loc", "huyen kim thanh", "huyen kinh mon", "huyen nam sach", "huyen ninh giang", "huyen thanh ha", "huyen thanh mien", "huyen tu ky", "thanh pho hai duong", "thi xa chi linh",
                                              "huyen an duong", "huyen cat hai", "huyen kien thuy", "huyen thuy nguyen", "huyen tien lang", "huyen vinh bao", "quan do son", "quan duong kinh", "quan hai an", "quan hong bang", "quan kien an", "quan le chan", "quan ngo quyen",
                                              "huyen an thi", "huyen khoai chau", "huyen kim dong", "huyen my hao", "huyen phu cu", "huyen tien lu", "huyen van giang", "huyen van lam", "huyen yen my", "thanh pho hung yen",
                                              "huyen giao thuy", "huyen hai hau", "huyen my loc", "huyen nam truc", "huyen nghia hung", "huyen truc ninh", "huyen vu ban", "huyen xuan truong", "huyen y yen", "thanh pho nam dinh",
                                              "huyen gia vien", "huyen hoa lu", "huyen kim son", "huyen nho quan", "huyen yen khanh","thanh pho ninh binh", "thanh pho tam diep",
                                              "huyen dong hung", "huyen hung ha", "huyen kien xuong", "huyen quynh phu", "huyen thai thuy", "huyen tien hai", "huyen vu thu", "thanh pho thai binh",
                                              "huyen binh xuyen", "huyen lap thach", "huyen song lo", "huyen tam dao", "huyen tam duong", "huyen vinh tuong", "huyen yen lac", "thanh pho vinh yen", "thi xa phuc yen",
                                              "huyen dong son", "huyen ha trung", "huyen hau loc", "huyen hoang hoa", "huyen lang chanh", "huyen muong lat", "huyen nga son", "huyen nhu thanh", "huyen nhu xuan", "huyen nong cong", "huyen quang xuong", "huyen thach thanh", "huyen thieu hoa", "huyen tho xuan", "huyen thuong xuan", "huyen tinh gia", "huyen trieu son", "huyen yen dinh", "thanh pho thanh hoa", "thi xa bim son", "thi xa sam son",
                                              "huyen anh son", "huyen con cuong", "huyen dien chau", "huyen do luong", "huyen hung nguyen", "huyen ky son", "huyen nam dan", "huyen nghi loc", "huyen nghia dan", "huyen que phong", "huyen quy chau", "huyen quy hop", "huyen quynh luu", "huyen tan ky", "huyen thanh chuong", "huyen tuong duong", "huyen yen thanh", "thanh pho vinh", "thi xa cua lo", "thi xa hoang mai", "thi xa thai hoa"),
  
  # Date thresholds for age classification
  #age_threshold_1 = as.Date("2017-09-30"),
  #age_threshold_5 = as.Date("2013-01-01"),
  age_threshold_1 = as.Date("2018-01-01"),
  age_threshold_5 = as.Date("2013-01-01"),
  
  # Vaccination campaign period thresholds
  vacc_campaign_start = as.Date("2018-11-01"),
  vacc_campaign_end = as.Date("2019-02-28"),
  vacc_eff = as.Date("2019-03-14"),
  
  # Census parameters
  agecut = c(0, 1, 5, 10),
  #province_tar = c("Ha Noi", "Vinh Phuc", "Ha Nam", "Quang Ninh", "Thai Nguyen"),
  #province_tar = c("Bac Ninh", "Hung Yen", "Thai Nguyen", "Quang Ninh", "Hoa Binh", 
   #                "Nghe An", "Tuyen Quang", "Ha Giang", "Yen Bai", "Ha Noi", 
    #               "Hai Phong", "Thai Binh", "Nam Dinh", "Ha Nam", "Ninh Binh", 
     #              "Thanh Hoa", "Bac Giang", "Phu Tho", "Vinh Phuc", "Hai Duong"),
  #province_tar = c("Thai Nguyen", "Phu Tho", "Son La", "Ha Noi", "Ha Nam", 
   #                "Bac Ninh", "Hai Phong", "Hung Yen", "Thai Binh", "Vinh Phuc", 
    #               "Ninh Binh", "Thanh Hoa", "Nghe An"),
  province_tar = c("Ha Noi", "Nghe An", "Hung Yen", "Bac Ninh", 
                   "Ha Nam", "Nam Dinh", "Thanh Hoa", "Hai Duong", "Hai Phong"),
  district_tar = c("huyen ba vi", "huyen chuong my", "huyen dan phuong", "huyen dong anh", "huyen gia lam", "huyen hoai duc", "huyen me linh", "huyen my duc", "huyen phu xuyen", "huyen phuc tho", "huyen quoc oai", "huyen soc son", "huyen thach that", "huyen thanh oai", "huyen thanh tri", "huyen thuong tin", "huyen ung hoa", "quan ba dinh", "quan bac tu liem", "quan cau giay", "quan dong da", "quan ha dong", "quan hai ba trung", "quan hoan kiem", "quan hoang mai", "quan long bien", "quan nam tu liem", "quan tay ho", "quan thanh xuan", "thi xa son tay",
                   "huyen anh son", "huyen con cuong", "huyen dien chau", "huyen do luong", "huyen hung nguyen", "huyen ky son", "huyen nam dan", "huyen nghi loc", "huyen nghia dan", "huyen que phong", "huyen quy chau", "huyen quy hop", "huyen quynh luu", "huyen tan ky", "huyen thanh chuong", "huyen tuong duong", "huyen yen thanh", "thanh pho vinh", "thi xa cua lo", "thi xa hoang mai", "thi xa thai hoa",
                   "huyen an thi", "huyen khoai chau", "huyen kim dong", "huyen my hao", "huyen phu cu", "huyen tien lu", "huyen van giang", "huyen van lam", "huyen yen my", "thanh pho hung yen",
                   "huyen gia binh", "huyen luong tai", "huyen que vo", "huyen thuan thanh", "huyen tien du", "huyen yen phong", "thanh pho bac ninh", "thi xa tu son",
                   "huyen binh luc", "huyen duy tien", "huyen kim bang", "huyen ly nhan", "huyen thanh liem", "thanh pho phu ly",
                   "huyen giao thuy", "huyen hai hau", "huyen my loc", "huyen nam truc", "huyen nghia hung", "huyen truc ninh", "huyen vu ban", "huyen xuan truong", "huyen y yen", "thanh pho nam dinh",
                   "huyen dong son", "huyen ha trung", "huyen hau loc", "huyen hoang hoa", "huyen lang chanh", "huyen muong lat", "huyen nga son", "huyen nhu thanh", "huyen nhu xuan", "huyen nong cong", "huyen quang xuong", "huyen thach thanh", "huyen thieu hoa", "huyen tho xuan", "huyen thuong xuan", "huyen tinh gia", "huyen trieu son", "huyen yen dinh", "thanh pho thanh hoa", "thi xa bim son", "thi xa sam son",
                   "huyen binh giang", "huyen cam giang", "huyen gia loc", "huyen kim thanh", "huyen kinh mon", "huyen nam sach", "huyen ninh giang", "huyen thanh ha", "huyen thanh mien", "huyen tu ky", "thanh pho hai duong", "thi xa chi linh"),
                   
  age_group_levels = c("<1", "1-5", ">5"),
  age_cases_levels = c("(0,1]", "(1,5]", "(5,10]"),
  census_file = "/home/bao/Project/OUCRU/Manifest_Vac_Campaign/census2019.rds"
)

# --------------------------------------------------
# 3. Define a Preprocessing Function that Uses 'param'
# --------------------------------------------------
preprocess_data <- function(param) {
  # Load the full dataset
  full_data <- readRDS(param$data_path)
  
  # Extract important variables into a data frame
  data_extract <- data.frame(
    province = full_data$province,
    onset_date = full_data$onset_date,
    birth_date = full_data$ngaysinh.x,
    vac_status = full_data$vaccination,
    source_infection = full_data$source_infection,
    stringsAsFactors = FALSE
  )
  
  # Remove rows with missing 'province'
  data_extract <- data_extract %>% filter(!is.na(province))
  
  # Classify age groups based on 'birth_date' using parameters from 'param'
  data_extract <- data_extract %>% mutate(
    age_group = case_when(
      birth_date < param$age_threshold_5 ~ ">5",
      birth_date > param$age_threshold_1 ~ "<1",
      birth_date >= param$age_threshold_5 & birth_date <= param$age_threshold_1 ~ "1-5"
    )
  )
  
  # Define vaccination period based on 'onset_date' using parameters from 'param'
  data_extract <- data_extract %>% mutate(
    period = case_when(
      onset_date < param$vacc_campaign_start ~ "Before",
      onset_date > param$vacc_campaign_end ~ "After",
      onset_date >= param$vacc_campaign_start & onset_date <= param$vacc_campaign_end ~ "During",
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(period))
  
  # Convert date columns and create additional time variables
  data_extract <- data_extract %>% mutate(
    onset_date = as.Date(onset_date),
    birth_date = as.Date(birth_date),
    onset_month = floor_date(onset_date, "month"),
    onset_week  = floor_date(onset_date, "week"),
    onset_year  = floor_date(onset_date, "year")
  )
  
  # Remove rows with missing 'source_infection'
  data_extract <- data_extract %>% filter(!is.na(source_infection))
  
  # Recode 'source_infection' values for clarity
  data_extract <- data_extract %>% mutate(
    source_infection = case_when(
      source_infection %in% c("suspect other h", "nch", "suspect nch", "other h") ~ "hospital",
      source_infection == "community" ~ "community",
      TRUE ~ source_infection
    )
  )
  
  # Combine age groups for further analysis if needed
  data_extract <- data_extract %>% mutate(
    age_combine = ifelse(
      age_group %in% c("<1", ">5"),
      "Other Groups",
      ifelse(age_group == "1-5", "1-5", NA)
    ),
    age_2 = ifelse(
      age_group %in% c("<1", "1-5"),
      "0-5",
      ifelse(age_group == ">5", ">5", NA)
    )
  )
  
  # Filter data for the selected provinces using 'param'
  data_filtered <- data_extract %>% filter(province %in% param$selected_provinces) # No change here
  
  return(data_filtered)
}

# --------------------------------------------------
# 2. Call the Preprocessing Function and Generate Output
# --------------------------------------------------
# Preprocess the data using the 'param' list
data_processed <- preprocess_data(param)

# --------------------------------------------------
# 3. Generate Summary Statistics
# --------------------------------------------------
# Create a summary table: Count of cases per province
province_summary <- data_processed %>%
  group_by(province) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(province_summary)

################################################################################
                        # Measles Cases and Proportion
################################################################################

# ---------------------------
# Function to Compute and Viz Measles Cases and Proportion
# ---------------------------

# Define functions: Calculation proportion 
calc_ts_np <- function(data, varname, time) {
  data %>%
    dplyr::filter(!is.na({{ varname }}), !is.na({{ time }})) %>%
    dplyr::count({{ time }}, {{ varname }}) %>%
    dplyr::group_by({{ time }}) %>%
    dplyr::mutate(prop = n / sum(n))
}

# Define Functions: Visualize with stack bar 

plot_ts_stacked_bar <- function(data, varname, time, style = F) {
  
  # Compute proportion
  df <- calc_ts_np(data, {{ varname }}, {{ time }})
  
  # Ensure the input is data frame to use function complete()
  df <- data.frame(df)
  
  # Use complete to fill missing groups with 0
  df <- df %>%
    tidyr::complete({{ time }}, {{ varname }}, fill = list(n = 0, prop = 0))
  
  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = {{ time }}, y = prop, fill = {{ varname }})) +
    ggplot2::geom_bar(stat = "identity")
  
  if (style == T) {
    p <- p +
      ggsci::scale_fill_jco() +
      theme_minimal()
  }
  
  list(data = df, plot = p)
}

# ---------------------------
# Load - Compute and Viz
# ---------------------------

# Percentage of measles cases 
measles_cases_percent <- data.frame(
  onset_month = data_processed$onset_month, 
  age_group = data_processed$age_group
)

# Integrate with the function 
measles_cases_percent_result <- calc_ts_np(data = measles_cases_percent, varname = age_group, time = onset_month)

measles_cases_percent_viz <- plot_ts_stacked_bar(data = measles_cases_percent, varname = age_group, time = onset_month, style = F)
measles_cases_percent_viz$plot

################################################################################
                                        #Number of Measles Cases
################################################################################
measles_cases_num <- data_processed %>% 
  group_by(onset_month) %>% 
  summarise(total_cases = n(), .groups = "drop")

ggplot(measles_cases_num, aes(x = onset_month, y = total_cases)) +
  geom_bar(stat = "identity", fill = "gray40", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Number of Measles Cases Over Time in Northern Provinces",
    x = "Time of Onset",
    y = "Number of Cases"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
                              # Slider Analysis
################################################################################
slider_analysis <- function(data_processed, param) {
  # --- Process Census Data ---
  # Load census data and clean province and age columns
  census <- readRDS(param$census_file) %>%
    mutate(
      province = gsub("^(Tỉnh|Thành phố) ", "", province),
      province = stri_trans_general(province, "Latin-ASCII"),
      district = tolower(stri_trans_general(district, "Latin-ASCII")),
      age = gsub(" tuổi| \\+", "", age)
    ) %>%
    mutate(
      age = as.numeric(age),
      age_cases_attack = cut(age, breaks = param$agecut, right = TRUE)
    ) %>%
    filter(!is.na(age_cases_attack)) %>%
    mutate(
      age_cases_attack = factor(
        age_cases_attack,
        levels = param$age_cases_levels,
        labels = param$age_group_levels
      )
    ) %>%
    # Filter to selected (northern) provinces
    filter(province %in% param$province_tar & district %in% param$district_tar) # Change to selected_provinces
  
  # Summarize population by age group (from census)
  census_attack <- census %>% 
    group_by(age_cases_attack) %>% 
    summarise(n_census = sum(n), .groups = "drop")
  
  # --- Process Case Data ---
  # Filter and rename case data (only keep rows in target provinces)
  data_census <- data_processed %>%
    filter(province %in% param$province_tar) %>% # Change to selected_provinces
    mutate(
      #province = "Northern Provinces",
      age_group = factor(age_group, levels = param$age_group_levels)
    )
  
  # Count the number of patients by onset_date and age group
  data_attack <- data_census %>% 
    group_by(age_group, onset_date) %>% 
    count() %>% 
    ungroup() %>% 
    complete(onset_date, age_group, fill = list(n = 0))
  
  # Compute cumulative cases and attack rate
  result_north <- data_attack %>% 
    left_join(census_attack, by = c("age_group" = "age_cases_attack")) %>% 
    group_by(age_group) %>% 
    arrange(onset_date, .by_group = TRUE) %>% 
    mutate(
      cum_cases = cumsum(n),
      lag_cum_cases = lag(cum_cases, default = 0),
      attack_rate = n / (n_census - lag_cum_cases)
    ) %>% 
    ungroup()
  
  # --- Smooth the Attack Rate ---
  result_smooth <- result_north %>% 
    group_by(age_group) %>% 
    arrange(onset_date, .by_group = TRUE) %>% 
    mutate(
      attack_rate_ma = slide_dbl(
        .x = attack_rate,
        .f = mean, 
        .before = 13,
        .after = 0, 
        .step = 1,
        .complete = TRUE
      )
    ) %>% 
    ungroup()
  
  # --- Create the Plot ---
  p_slider <- result_smooth %>% 
    mutate(age_group = fct_rev(age_group)) %>% 
    ggplot(aes(
      x = onset_date, 
      y = log(attack_rate_ma * 100000),
      color = age_group
    )) +
    geom_vline(xintercept = param$vacc_campaign_start, linetype = "dashed") +
    geom_vline(xintercept = param$vacc_campaign_end, linetype = "dashed") +
    geom_vline(xintercept = param$vacc_eff, linetype = "dashed") +
    geom_line() +
    scale_color_observable() +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%Y",
      limits = c(ymd("2017-07-06"), ymd("2020-01-01")),
      expand = c(0, 0)
    ) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Date of symptom onset", y = "Log attack rate", color = "Age(years)") +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Return all results as a list so you can inspect each step if needed
  list(
    census = census,
    census_attack = census_attack,
    result_smooth = result_smooth,
    plot = p_slider
  )
}

slider <- slider_analysis(data_processed, param)
slider$plot

################################################################################
                                    # Ridgeline Distribution
################################################################################
plot_density <- function(df,
                         province_col = "province",
                         date_col = "onset_month",
                         age_col = "age_combine",
                         value_col = "total_cases",
                         vertical_line_date = as.Date("2019-03-14"),
                         start_line = as.Date("2018-11-01"),
                         end_line = as.Date("2019-02-28"),
                         date_label_format = "%Y",
                         plot_title = "Trend of Measles Cases by Age Group and Northern Provinces",
                         y_axis_label  = "Number of Cases")
{
  # 1) Convert columns to symbols for tidy evaluation
  province_var <- rlang::sym(province_col)
  date_var <- rlang::sym(date_col)
  age_var <- rlang::sym(age_col)
  value_var <- rlang::sym(value_col)
  
  # 2) Reorder age groups
  df <- df %>% 
    mutate(
      !!age_var := factor(
        !!age_var,
        levels = c("1-5", "Other Groups")
      )
    )
  # 3) Create Ridgeline Viz
  p <- ggplot(df, aes(x = !!date_var, y = !!age_var, fill = !!age_var)) +
    geom_density_ridges(
      # aes(weight = !!value_var),
      alpha = 0.7,
      scale = 1.2
    ) +
    facet_wrap(vars(!!province_var), scales = "free_x") +
    # 4) Add dashed line for vaccination campaign
    geom_vline(
      xintercept = as.numeric(start_line),
      linetype = "dashed",
      color = "lightblue"
    ) +
    geom_vline(
      xintercept = as.numeric(vertical_line_date),
      linetype = "dashed", 
      color = "green"
    ) +
    # 5) Labels 
    labs(
      title = paste0(plot_title, "(Blue: Start of Vaccination Campaign; Green: Vaccination Campaign show effects)"),
      x = "Time(Year)",
      y = y_axis_label,
      fill = "Age Group"
    ) +
    # 6) Choose color palette for fill
    scale_fill_brewer(palette = "Dark2") +
    # 7) Handle x-axis scale
    scale_x_date(
      date_breaks = "12 months",
      date_labels = date_label_format,
      expand = expansion(mult = c(0.01, 0.05))
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  return(p)
}

# Create data frame to input to the function plot_density
density_distribution <- data_processed %>% 
  mutate(province = ifelse(province %in% param$province_tar, province, "Other provinces")) %>%  #Change to selected_provinces
  # Create order
  mutate(province = factor(province, levels = c(intersect(param$province_tar, unique(province)), "Other provinces")))

density <- density_distribution %>% 
  group_by(province, onset_month, age_combine) %>% 
  summarise(total_cases = n(), .groups = "drop") %>% 
  # Reapply the factor levels 
  mutate(province = factor(province, levels = c(setdiff(levels(density_distribution$province), "Other provinces"), "Other provinces")))

# Integrate to function 
plot_density(
  df = density,
  province_col = "province",
  date_col = "onset_month",
  age_col = "age_combine",
  vertical_line_date = as.Date("2019-03-14"),
  start_line = as.Date("2018-11-01"),
  #end_line = as.Date("2018-12-31"),
  date_label_format = "%Y",
  plot_title = "Ridgeline Distribution of Cases in Northern Provinces",
  y_axis_label = "Age Group"
)

################################################################################
               # Hospital Rate Analysis (01-01-2013 to 01-01-2018)
################################################################################

# Add the coverage to the list 
cov_01_2017 <- c(
  "Ha Noi" = 81.5,
  "Nghe An" = 70.1,
  "Nam Dinh" = 85.5,
  "Hung Yen" = 80.3,
  "Thanh Hoa" = 80.4,
  "Vinh Phuc" = 88.8,
  "Ha Nam" = 85.4,
  "Hai Phong" = 75.1,
  "Hai Duong" = 78.4,
  "Bac Ninh" = 85.8,
  "Phu Tho" = 92.2,
  "Bac Giang" = 81.7,
  "Ninh Binh" = 83.5,
  "Son La" = 15.3,
  "Thai Nguyen" = 88.4,
  "Hoa Binh" = 85.1,
  "Thai Binh" = 77.7,
  "Ha Giang" = 73.8,
  "Quang Ninh" = 78.6,
  "Yen Bai" = 81.6
)

cov_all_2017 <- c(
  "Ha Noi"      = 93.8,
  "Nghe An"     = 78.9,
  "Nam Dinh"    = 93,
  "Hung Yen"    = 89.7,
  "Thanh Hoa"   = 90.9,
  "Vinh Phuc"   = 95,
  "Ha Nam"      = 92.7,
  "Hai Phong"   = 84.5,
  "Hai Duong"   = 89.9,
  "Bac Ninh"    = 93.9,
  "Phu Tho"     = 98.1,
  "Bac Giang"   = 88.9,
  "Ninh Binh"   = 90.6,
  "Son La"      = 15.3,
  "Thai Nguyen" = 92.8,
  "Hoa Binh"    = 92,
  "Thai Binh"   = 87.3,
  "Ha Giang"    = 86.9,
  "Quang Ninh"  = 91.7,
  "Yen Bai"     = 94.4
)

cov_01_2018 <- c(
  "Ha Noi" = 0.844,
  "Nghe An" = 0.603,
  "Nam Dinh" = 0.808,
  "Hung Yen" = 0.794,
  "Thanh Hoa" = 0.749,
  "Vinh Phuc" = 0.826,
  "Ha Nam" = 0.798,
  "Hai Phong" = 0.614,
  "Hai Duong" = 0.808,
  "Bac Ninh" = 0.88,
  "Phu Tho" = 0.892,
  "Bac Giang" = 0.742,
  "Ninh Binh" = 0.767,
  "Son La" = 0.389,
  "Thai Nguyen" = 0.122,
  "Hoa Binh" = 0.724,
  "Thai Binh" = 0.694,
  "Ha Giang" = 0.678,
  "Quang Ninh" = 0.762,
  "Yen Bai" = 0.809
)

cov_all_2018 <- c(
  "Ha Noi" = 0.919,
  "Nghe An" = 0.727,
  "Nam Dinh" = 0.915,
  "Hung Yen" = 0.932,
  "Thanh Hoa" = 0.872,
  "Vinh Phuc" = 0.922,
  "Ha Nam" = 0.896,
  "Hai Phong" = 0.741,
  "Hai Duong" = 0.919,
  "Bac Ninh" = 0.97,
  "Phu Tho" = 0.984,
  "Bac Giang" = 0.809,
  "Ninh Binh" = 0.855,
  "Son La" = 0.389,
  "Thai Nguyen" = 0.139,
  "Hoa Binh" = 0.852,
  "Thai Binh" = 0.817,
  "Ha Giang" = 0.831,
  "Quang Ninh" = 0.927,
  "Yen Bai" = 0.938
)

# Save it into the param list:
param$cov_01_2017 <- cov_01_2017
param$cov_all_2017 <- cov_all_2017
param$cov_01_2018 <- cov_01_2018
param$cov_all_2018 <- cov_all_2018



# ------------------------------------
# 1. Function to compute hospital rate
# ------------------------------------

compute_hospital_rate <- function(data, source_type = "hospital") { # Change to community if needed
  # Summarize total cases per province, age group and source infection
  hospital_cases <- data %>% 
    group_by(province, age_combine, source_infection) %>%  # Change to age_2 column for 0-5 and >5
    summarise(total_cases = n(), .groups = "drop")
  
  # Summarize total cases without age group
  #hospital_cases <- data %>% 
   # group_by(province, source_infection) %>% 
    #summarise(total_cases = n(), .groups = "drop")
  
  # Compute hospital rate of cases without age group
 # hospital_rate <- hospital_cases %>% 
  #  group_by(province) %>% 
   # mutate(
    #  total_all = sum(total_cases)
  #  ) %>% 
   # ungroup() %>% 
    #filter(source_infection == source_type,
     #      province %in% param$province_tar,
      #     province != "Son La") %>% 
    #mutate(rate = (total_cases / total_all) * 100)
  
  # Compute hospital rate for age group 
  hospital_rate <- hospital_cases %>% 
    group_by(province) %>% 
    mutate(rate = (total_cases / sum(total_cases)) * 100) %>% 
    ungroup() %>% 
    filter(source_infection == source_type)
  
  return(hospital_rate)
}

# ------------------------------------
# 1.1. Compute workflow
# ------------------------------------
hospital_rate <- compute_hospital_rate(data = data_processed)

# -------------------------------------------
# 2. Function to filter age and add coverage
# -------------------------------------------
filter_add_cov <- function(hospital_rate, age_group, cov_01, cov_all) { 
  # Filter target age 
  filtered <- hospital_rate %>% filter(age_combine == age_group) # Change to age_2 or age_combine
  
  # Add coverage by province 
  filtered <- filtered %>% mutate(
    cov_01_2017 = cov_01[province],
    cov_all_2017 = cov_all[province]
    #cov_01_2018 = cov_01[province]
    #cov_all_2018 = cov_all[province]
  )
  
  return(filtered)
}

# -------------------------------------------
# 2.1. Without Age
# -------------------------------------------
hospital_rate <- hospital_rate %>% mutate(
  coverage = param$cov_all_2017[province])

# -------------------------------------------
# 2.2. Compute workflow
# -------------------------------------------
target_age <- filter_add_cov(hospital_rate = hospital_rate, age_group = "1-5", cov_01 = param$cov_01_2017, cov_all = param$cov_all_2017) # Change to 0-5 if use age_2 + Change to cov_2018 if needed

# -------------------------------------------
# 3. Function to generate scatter plot (With Age)
# -------------------------------------------
scatter_plot <- function(data, 
                         xvar = "cov_all_2017", # Change to cov_01_2017 or cov_all_2017 or 2018
                         yvar = "rate",
                         title = "Hospital Rate vs Coverage",
                         filter_provinces = NULL) { # Change to specific provinces 
  
  # Optional: Filter for specific provinces
  if(!is.null(filter_provinces)) {
    data <- data %>% filter(province %in% filter_provinces)
  }
  
  # Create scatter plot
  scatter <- ggplot(data, aes_string(x = xvar, y = yvar, label = "province")) +
    geom_point(size = 3, color = "blue") +
    geom_text_repel(size = 3, segment.color = "grey50") +
    geom_smooth(method = "lm", color = "red") +
    labs(title = title,
         x = "Coverage (%)", 
         y = "Rate") +
    theme_minimal()
  
  return(scatter)
}


# -------------------------------------------
# 3.1 Compute workflow
# -------------------------------------------
target_age <- target_age %>% filter(province != "Nam Dinh")

scatter <- scatter_plot(data = target_age, # Change if needed
                      xvar = "cov_all_2017", # Change to cov_01_2017 or cov_all_2017
                      yvar = "rate",
                      title = "Hospital Rate vs Coverage (all) in Group 1-5",
                      filter_provinces = param$province_tar) # Change to specific provinces 

scatter

# -------------------------------------------
# 3.2 Visualize without age
# -------------------------------------------
ggplot(hospital_rate, aes(x = coverage, y = rate, label = province)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(size = 3, segment.color = "grey50") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Test",
       x = "Coverage (%)", 
       y = "Rate") +
  theme_minimal()


# Linear fit 
test_lm <- target_age %>% filter(total_cases >= 9,
                                 province %in% param$province_tar)
#province != "Nam Dinh")
model <- lm(rate ~ cov_01_2017 , data = test_lm)
summary(model)

################################################################################
            # Before and After Vaccination Campaign
################################################################################
before_after <- data_processed %>% mutate(
  province = ifelse(province %in% param$province_tar, province, "Other provinces") # Can change to selected provinces
) %>% 
  group_by(province, age_combine, source_infection, period) %>%  # Can add/remove age_combine
  mutate(period = case_when(
    period == "During" ~ "Before",
    period == "Before" ~ "Before",
    period == "After" ~ "After"
  )) %>% 
  summarise(total_cases = n(), .groups = "drop") %>% 
  filter(age_combine == "1-5",
         source_infection == "hospital")


before_after_rate <- before_after %>% 
  group_by(province) %>% 
  mutate(total_sum = sum(total_cases)) %>% 
  ungroup() %>%
  mutate(rate = (total_cases / total_sum) * 100) %>% 
  mutate(
    cov_01_2017 = param$cov_01_2017[province],
    cov_all_2017 = param$cov_all_2017[province]
  ) #%>%
  #filter(province !=  "Other provinces",
   #      period == "Before")

# Visualize with coverage
ggplot(before_after_rate, aes(y = rate, x = cov_all_2017, label = province)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(size = 3, segment.color = "gray50") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Hospital Infection Rate vs Coverage Before Vaccination Campaign",
    x = "Coverage",
    y = "Rate"
  ) +
  theme_minimal()

hi <- lm(rate ~ cov_01_2017 , data = before_after_rate)
summary(hi)

# Ensure the order 
order <- before_after_rate %>% 
  filter(period == "Before") %>% 
  arrange(desc(rate)) %>% 
  pull(province) %>% 
  unique()

# Convert province to a factor with the specified order
before_after_rate <- before_after_rate %>%
  #mutate(province = factor(province, levels = c("Bac Ninh", "Ha Nam", "Ha Noi", "Hai Duong", "Hai Phong", "Hung Yen", "Nam Dinh", "Nghe An", "Thanh Hoa", "Other provinces"))) %>%  # Can change to order
  mutate(province = factor(province, levels = order)) %>% 
  mutate(province = fct_rev(province))

# Visualize with horizontal stack bar 
ggplot(before_after_rate, aes(x = province, y = rate, fill = period)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(before_after_rate$rate, 1)),
            position = position_dodge(width = 0.9),
            hjust = ifelse(before_after_rate$rate > 0.15, 1.1, -0.1),
            size = 3) +
  coord_flip() +
  labs(
    title = "Hospital Infection Rate all Group Age Before and After Vac Campaign",
    x = "Province", 
    y = "Rate (%)", 
    caption = "Unit: Percentage"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Statistical Tests 
stat_rate <- before_after_rate %>% 
  select(province, period, rate) %>% 
  pivot_wider(names_from = period, values_from = rate)

# Check normality
differences <- stat_rate$Before - stat_rate$After
shapiro <- shapiro.test(differences)
shapiro

# Paired T test: Compares means of 2 related group
t_test <- t.test(stat_rate$Before, stat_rate$After, paired = TRUE)
t_test

# Wilcox Test
wilcox_test <- wilcox.test(stat_rate$Before, stat_rate$After, paired = TRUE)
wilcox_test
