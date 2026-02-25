pkgs <- c("dplyr","lubridate","broom","car","pROC","pander","ggplot2","officer","rvg")
#' Install an R package if it is missing
#'
#' Checks whether a package is available and installs it from CRAN if not.
#'
#' @param p Character. Package name to check and install if missing.
#' @return Invisible NULL.
#'
#' @details
#' Intended for lightweight bootstrapping of this script. In production or locked environments, consider using renv.
#'
#' @examples
#' install_if_missing("dplyr")

install_if_missing <- function(p){
  if(!requireNamespace(p, quietly=TRUE)){
    install.packages(p, dependencies=TRUE)
  }
}
invisible(lapply(pkgs, install_if_missing))
library(dplyr); library(lubridate);library(tidyr); library(broom); library(car); library(ggplot2)

# ------------------ 1. 队列参数 ------------------
set.seed(12345)
N <- 10000  
dob_start <- as.Date("2010-01-01")
dob_end   <- as.Date("2021-12-31")
censor_date <- as.Date("2021-12-31")

# ------------------ 2. 模拟数据 ------------------
children <- data.frame(
  child_id = 1:N,
  dob = sample(seq(dob_start, dob_end, by="day"), N, replace = TRUE), #随机设置出生日期在随访时间内任取
  sex = sample(c("Male","Female"), N, replace = TRUE), #性别随机
  indigenous = rbinom(N,1,0.06), #原住民6%
  premature = rbinom(N,1,0.10), #早产率10%
  chronic = rbinom(N,1,0.05), #慢性病患病率5%
  stringsAsFactors = FALSE
)

children <- children %>%
  mutate(
    start_follow = dob,
    end_follow = pmin(dob + years(5), censor_date),# 随访结束时间取满5岁或研究截止日期的较小值（处理了右删失）
    person_days = as.numeric(end_follow - start_follow) + 1, # +1确保包含第一天
    person_days = ifelse(person_days < 0, 0, person_days),
    person_years = person_days / 365.25
  )
# 只保留天数>0的
children <- children %>% filter(person_days > 0)

# ------------------ 3. 模拟每个儿童的测试次数------------------
# 现实中测试数基本为0，1和2
# 使用多项分布模拟
set.seed(54321)
u <- runif(nrow(children)) 
test_n <- integer(nrow(children))
test_n[u < 0.45] <- 0 # 45%的儿童进行0次检测
test_n[u >= 0.45 & u < 0.80] <- 1 # 35%的儿童进行1次检测
test_n[u >= 0.80 & u < 0.92] <- 2 # 12%的儿童进行2次检测
test_n[u >= 0.92 & u < 0.97] <- 3 # 5%的儿童进行3次检测
test_n[u >= 0.97 & u < 0.99] <- 4 # 2%的儿童进行4次检测
test_n[u >= 0.99] <- sample(5:8, sum(u>=0.99), replace = TRUE) # 1%的儿童随机进行5-8次检测
children$test_n <- test_n
total_tests <- sum(children$test_n)

# ------------------ 4. 在随访窗口中生成测试事件 ------------------
test_rows_child <- rep(children$child_id, times = children$test_n)
# 为每个儿童在 [start_follow, end_follow]内分配测试时间
test_dates <- as.Date(rep(NA, length(test_rows_child)))
pos <- 1
for(i in seq_len(nrow(children))){
  k <- children$test_n[i]
  if(k == 0) next
  start <- children$start_follow[i]
  endd <- children$end_follow[i]
  possible_days <- as.integer(endd - start)
  if(possible_days < 0) next
  offs <- sample(0:possible_days, size = k, replace = TRUE)
  test_dates[pos:(pos+k-1)] <- start + offs
  pos <- pos + k
}
tests <- data.frame(child_id = test_rows_child, test_date = as.Date(test_dates))
tests <- tests %>% arrange(child_id, test_date) # 按儿童ID和检测日期排序

# ------------------ 5. week_of_year,year,season,age_months,age_group------------------
tests <- tests %>%
  mutate(
    week_of_year = isoweek(test_date),
    year = year(test_date),
    season = case_when(
      month(test_date) %in% c(12,1,2) ~ "Summer",
      month(test_date) %in% c(3,4,5) ~ "Autumn",
      month(test_date) %in% c(6,7,8) ~ "Winter",
      TRUE ~ "Spring"
    )
  ) %>%
  left_join(children %>% select(child_id, dob, sex, indigenous, premature, chronic), by = "child_id") %>%
  mutate(
    age_months = as.numeric((test_date - dob)/30.44), # 计算检测时的月龄 
    age_group = cut(age_months, breaks=c(-1,12,24,60,9999), labels=c("<12m","12-23m","24-59m","60m+")) # 将月龄分组
  )
# ------------------ 6. encounter,region,admission,ICD (ALRI) ------------------
set.seed(111)
tests <- tests %>%
  mutate(
    encounter = sample(c("ED","Hospital","Outpatient"), n(), replace = TRUE, prob=c(0.45,0.25,0.30)), # 就诊类型：急诊、住院、门诊
    region = sample(c("Metropolitan","Regional","Remote"), n(), replace = TRUE, prob=c(0.7,0.25,0.05)), # 地区分类：大都市、地方区域、偏远地区
    has_admission = ifelse(encounter %in% c("ED","Hospital"), rbinom(n(),1,0.6), 0) #是否有入院记录：如果是ED或Hospital，60%概率有入院记录
  )
# ICD
alri_codes <- c("J21","J12","J20") # 对应细支气管炎、病毒性肺炎和支气管炎
other_codes <- c("A09","R50","H66") # 对应其他胃肠炎和结肠炎、发热、化脓性和未特指的中耳炎
set.seed(222)
tests$icd <- NA_character_ # 初始化ICD列
ad_idx <- which(tests$has_admission == 1)
tests$icd[ad_idx] <- sample(c(alri_codes, other_codes), length(ad_idx), replace = TRUE, # 样本量 = 入院记录数，允许重复（同一个人多次入院）
                            prob = c(rep(0.15,length(alri_codes)), rep(0.175,length(other_codes)))) # 每个 alri_codes 15%概率，other_codes 17.5%概率
tests$is_ALRI <- ifelse(tests$icd %in% alri_codes, 1, 0) # 如果icd代码在 alri_codes中，则 is_ALRI=1，否则=0

# ------------------ 7. 用季节和年龄模拟病毒 ------------------
w <- tests$week_of_year
p_base <- 0.03 + 0.25 * sin(2*pi*w/52) # 3%的基础感染率 + 25%的季节性波动，理论上在[-0.22, 0.28]，但会被截断
p_base <- pmax(pmin(p_base, 0.9), 0)  # 截去负值
age_mod <- ifelse(tests$age_months < 12, 1.5, ifelse(tests$age_months < 24, 1.2, 0.8)) # <1岁婴儿：风险增加50%；1-2岁幼儿：风险增加20%；≥2岁儿童：风险降低20%
p_RSV <- pmin(p_base * 1.2 * age_mod, 0.99) # RSV季节性强，婴儿风险高
p_FluA <- pmin(p_base * 0.9, 0.99) # 甲流比基线低10%
p_FluB <- pmin(p_base * 0.6, 0.99) # 乙流比基线低40%
p_PIV  <- pmin(p_base * 0.3, 0.99) # 副流感比基线低70%
p_hMPV <- pmin(p_base * 0.25,0.99) # 偏肺病毒比基线低75%

set.seed(333)
tests$RSV <- rbinom(nrow(tests), 1, p_RSV)
tests$FluA <- rbinom(nrow(tests), 1, p_FluA)
tests$FluB <- rbinom(nrow(tests), 1, p_FluB)
tests$PIV  <- rbinom(nrow(tests), 1, p_PIV)
tests$hMPV <- rbinom(nrow(tests), 1, p_hMPV)

# 如果存在RSV，减少合并感染(原文中没有考虑这一点)
set.seed(444)
for(i in seq_len(nrow(tests))){
  if(tests$RSV[i]==1){
    if(runif(1) < 0.8) tests$FluA[i] <- 0 # 80%概率抑制FluA
    if(runif(1) < 0.85) tests$FluB[i] <- 0 # 85%概率抑制FluB
    if(runif(1) < 0.9) tests$PIV[i]  <- 0 # 90%概率抑制PIV
    if(runif(1) < 0.9) tests$hMPV[i] <- 0 # 90%概率抑制hMPV
  }
}
tests$any_target <- as.integer(rowSums(tests[,c("RSV","FluA","FluB","PIV","hMPV")]) > 0) # 如果任一病原体阳性，则any_target=1，否则=0

# ------------------ 8. 发作合并去重 (14天) ------------------
# 原文按照14天进行发作合并去重，14天也是呼吸道疾病的窗口期
tests <- tests %>% arrange(child_id, test_date)
# 为每个儿童计算检测间隔并识别新发作
tests <- tests %>%
  group_by(child_id) %>%
  mutate(days_since_prev = as.numeric(test_date - lag(test_date)), # 计算与上一次检测的天数间隔
         new_episode = ifelse(is.na(days_since_prev) | days_since_prev > 14, 1L, 0L), # 判断是否为新发作：首次检测或间隔>14天
         episode_idx = cumsum(new_episode)) %>%  # 累积计数，为每个发作分配唯一索引
  ungroup()

episodes <- tests %>% 
  group_by(child_id, episode_idx) %>%
  summarise(
    episode_date = min(test_date),   # 发作日期：取发作内最早检测日期
    # 窗口期内有一次检测出发作就算
    any_target = as.integer(any(any_target == 1)),  # 病原体检测结果：发作内任一检测阳性即为阳性
    any_RSV = as.integer(any(RSV == 1)),  # 临床诊断：发作内任一诊断为RSV即有RSV发作
    any_ALRI = as.integer(any(is_ALRI == 1)), # 临床诊断：发作内任一诊断为ALRI即为ALRI发作
    # 基线特征：取第一次就诊的信息
    encounter = first(encounter), 
    region = first(region),
    dob = first(dob),
    sex = first(sex),
    indigenous = first(indigenous),
    premature = first(premature),
    chronic = first(chronic),
    # 发作时的年龄：基于发作日期重新计算
    age_months = as.numeric((min(test_date) - first(dob))/30.44),
    .groups="drop")
# 只保留随访期内发病的episodes数据
# 合并随访时间信息
episodes <- episodes %>%
  left_join(children %>% select(child_id, start_follow, end_follow, person_years), by="child_id") %>%
  # 筛选有效随访期内的发作
  filter(episode_date >= start_follow & episode_date <= end_follow) %>%
  # 添加时间和年龄分组变量
  mutate(year = year(episode_date),
         age_group = cut(age_months, breaks=c(-1,12,24,60,9999), labels=c("<12m","12-23m","24-59m","60m+")))

# ------------------ 9. 每年每千人发病率 ------------------
total_py <- sum(children$person_years, na.rm=TRUE) # 计算总人年数

events_by_year <- episodes %>% 
  filter(any_target==1) %>% # 只统计病原体阳性的发作
  group_by(year) %>%  # 按发作年份分组
  summarise(n_events = n()) %>%  
  complete(year = 2010:2021, fill = list(n_events = 0)) %>% # 补充缺失年份
  arrange(year)

# 使用随访期中点所在的年份，将整个 person-years 快速分配到这个年份
# 计算每个儿童的随访期中点年份
children <- children %>% mutate(mid_y = year(start_follow + (end_follow - start_follow)/2))
# 按中点年份汇总人年数
py_alloc <- children %>% group_by(mid_y) %>% summarise(person_years = sum(person_years)) %>%
  rename(year = mid_y)
# 补充缺失年份
py_alloc <- py_alloc %>% complete(year = 2010:2021, fill = list(person_years = 0))

# 合并数据并计算发病率
# 合并发作数和人年数
rates <- left_join(events_by_year, py_alloc, by="year")
# 计算年度发病率（每1000人年）
rates <- rates %>% mutate(rate_per_1000py = 1000 * n_events / person_years)

# 计算CI
alpha <- 0.05
#' Compute a Poisson 95% confidence interval for an incidence rate
#'
#' Returns the lower and upper 95% CI for a rate per 1,000 person-years using the chi-square method.
#'
#' @param k Integer. Number of events (case count).
#' @param T Numeric. Person-time denominator (e.g., person-years).
#' @return Numeric vector of length 2: c(lower, upper) rate per 1,000 person-years.
#'
#' @details
#' If T is missing or zero, returns c(NA, NA). For k = 0, the lower bound is set to 0.
#'
#' @examples
#' poisson_ci_rate(k = 10, T = 2500)

poisson_ci_rate <- function(k, T){ # K事件数，T人年数
  if(is.na(T) || T==0) return(c(NA,NA))
  lower_k <- if(k==0) 0 else 0.5 * qchisq(alpha/2, 2*k) # 下界：当k=0时为0，否则用卡方分布计算
  upper_k <- 0.5 * qchisq(1-alpha/2, 2*(k+1)) # 上界：使用k+1的卡方分布
  return(c(1000*lower_k/T, 1000*upper_k/T))  # 转换为每1000人年的率
}
ci <- t(mapply(poisson_ci_rate, rates$n_events, rates$person_years))
rates$rate_low95 <- ci[,1]; rates$rate_high95 <- ci[,2]

total_events <- sum(episodes$any_target)
overall_rate <- 1000 * total_events / total_py
ci_tot <- poisson_ci_rate(total_events, total_py)

# ALRI
alri_events <- sum(episodes$any_ALRI) # 计算ALRI总事件数
alri_rate <- 1000 * alri_events / total_py # 计算ALRI发病率（每1000人年）
alri_ci <- poisson_ci_rate(alri_events, total_py) # 计算ALRI发病率的95%置信区间

# ------------------ 10. 回归分析 ------------------
# 10.1 构建回归模型
# 双变量筛选 p-values (在模型中使用最大p值)
vars_to_test <- c("sex", "indigenous", "premature", "chronic", "age_group")
biv <- data.frame(variable = vars_to_test, p_value = NA)

for (v in vars_to_test) {
  # 构造公式
  f_full <- as.formula(paste("any_target ~", v))
  f_null <- as.formula("any_target ~ 1")
  
  # 先尝试拟合模型（用 tryCatch 捕获错误）
  mod_null <- tryCatch(glm(f_null, data = episodes, family = binomial()), error = function(e) NULL)
  mod_full <- tryCatch(glm(f_full, data = episodes, family = binomial()), error = function(e) NULL)
  
  if (is.null(mod_full) || is.null(mod_null)) {
    biv$p_value[biv$variable == v] <- NA_real_
    next
  }
  # LRT 比较（若模型有警告/不收敛也可能产生 NA）
  lrt <- tryCatch(anova(mod_null, mod_full, test = "Chisq"), error = function(e) NULL)
  # 检查lrt是否为NULL或者lrt的行数是否小于2（即没有得到有效的检验结果）
  if (is.null(lrt) || nrow(lrt) < 2) {
    biv$p_value[biv$variable == v] <- NA_real_
  } else {
    pval <- lrt$`Pr(>Chi)`[2]
    # 若 pval NA，则退回到取系数 p 的最大值（备选）
    if (is.na(pval)) {
      s <- summary(mod_full)
      if (!is.null(s$coefficients) && nrow(s$coefficients) >= 2) {
        pval_alt <- max(s$coefficients[-1,4], na.rm = TRUE)
        biv$p_value[biv$variable == v] <- as.numeric(pval_alt)
      } else {
        biv$p_value[biv$variable == v] <- NA_real_
      }
    } else {
      biv$p_value[biv$variable == v] <- as.numeric(pval)
    }
  }
}
# 选择 p < 0.2 的进入多变量logistic回归模型
selected_vars <- biv$variable[!is.na(biv$p_value) & biv$p_value < 0.2]
selected_vars
#最终indigenous和chronic进入模型，他们的交互意义不大，暂不纳入交互项，后面的VIF也说明了这一点

# 构建最终多变量logistic回归模型
install.packages("logistf")
library(logistf)
# 构建多变量模型公式
form <- as.formula(
  paste("any_target ~", paste(selected_vars, collapse = " + "))
)
# 拟合 Firth logistic 模型
final_firth <- logistf(form, data = episodes)

# 10.2 回归模型解读
summary(final_firth)
# 输出 OR 和 95% CI
coef_tab <- data.frame(
  term = names(final_firth$coefficients),
  OR = exp(final_firth$coefficients),
  lower_CI = exp(final_firth$ci.lower),
  upper_CI = exp(final_firth$ci.upper),
  p = final_firth$prob
)
coef_tab
## indigenous的OR值为0.86(0.687,1.07)非常显著,但p = 0.173差异不具有统计学意义
## chronic的OR值为0.81(0.64, 1.01)边缘显著，但p = 0.067有点大，说明慢性疾病状态可能与病毒检测阳性正相关，但解释能力有限

# 10.3 VIF共线性诊断
glm_for_vif <- glm(form, data = episodes, family = binomial)
vif(glm_for_vif)
#两个变量的 VIF 都 ≈ 1，说明不存在多重共线性

# 10.4 Pearson χ² 拟合优度
# logistf包中lolik的第二个值是完整模型的对数似然
loglik_full <- final_firth$loglik["full"]
# 计算偏差，偏差 = -2 * 对数似然
deviance_val <- -2 * loglik_full
str(loglik_full)
# 计算拟合优度，给出随机变量取值大于给定值的概率
p_chisq <- pchisq(deviance_val, df = final_firth$df, lower.tail = FALSE)
print(p_chisq)
# 拟合优度为0，但这是正常的，因为样本量过大，即使微小偏差也会发生显著偏离，拟合优度检验只适用于<50的样本

# ---------- 11. 输出 & 保存结果 ----------
outdir <- "sim_cohort_outputs"
dir.create(outdir, showWarnings = FALSE)
write.csv(children, file.path(outdir,"children_sim.csv"), row.names=FALSE)
write.csv(tests, file.path(outdir,"tests_sim.csv"), row.names=FALSE)
write.csv(episodes, file.path(outdir,"episodes_sim.csv"), row.names=FALSE)
write.csv(rates, file.path(outdir,"rates_by_year_simulated.csv"), row.names=FALSE)
write.csv(coef_tab, file.path(outdir,"multivar_or_table.csv"), row.names=FALSE)
# RSV周发病率图像
weekly <- tests %>% group_by(year_week = lubridate::floor_date(test_date, unit="week")) %>%
  summarise(total_tests=n(), RSV_pos=sum(RSV), RSV_rate_per1000 = 1000*RSV_pos/total_tests)
png(file.path(outdir,"weekly_rsv_rate.png"), width=1000, height=400)
plot(weekly$year_week, weekly$RSV_rate_per1000, type="l", xlab="Week", ylab="RSV per 1000 tests", main="Weekly RSV positivity per 1000 tests (simulated)")
dev.off()
# 按年份划分的区域时间序列
reg_rates <- tests %>% group_by(region, year) %>% summarise(total_tests=n(), RSV_pos=sum(RSV)) %>%
  mutate(rate_per_1000 = 1000*RSV_pos/total_tests)
png(file.path(outdir,"regional_rsv_rate.png"), width=1000, height=400)
matplot(unlist(tapply(reg_rates$rate_per_1000, reg_rates$year, mean)), type="l")

ggplot(reg_rates, aes(x=year, y=rate_per_1000, color=region)) + geom_line() + ggtitle("Regional RSV rate per 1000 tests by year") + xlab("Year") + ylab("Rate per 1000 tests")
ggsave(filename=file.path(outdir,"regional_rsv_rate_gg.png"), width=10, height=4)
