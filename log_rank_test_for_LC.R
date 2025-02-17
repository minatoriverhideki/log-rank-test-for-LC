# 必要なライブラリのインストール（初回のみ）
install.packages("survival")

# ライブラリの読み込み
library(survival)

# データの読み込み
df <- read.csv("LC_analysis_ver2.csv")

#データの確認
colnames(df)

#列名の修正
colnames(df) <- c("group_by_margin", "local_relapse", "time")


# 不要な列を除外
df <- df[, c("group_by_margin","local_relapse","time")]

# サバイバルオブジェクトを作成
surv_obj <- Surv(time = df$time, event = df$local_relapse)

# Kaplan-Meier曲線をグループ別に作成
fit <- survfit(surv_obj ~ group_by_margin, data = df)

# 生存曲線のプロット
plot(fit, col = c("blue", "red"), xlab = "Time (months)", ylab = "Survival Probability")
legend("topright", legend = c("Group 1", "Group 2"), col = c("blue", "red"), lty = 1)

# ログランク検定
logrank_test <- survdiff(surv_obj ~ group_by_margin, data = df)
print(logrank_test)

# 5年生存率（60ヶ月時点）の95%信頼区間を取得
summary_fit <- summary(fit, times = 60)

# グループごとの5年生存率と95%信頼区間を表示
survival_5y <- data.frame(
  Group = summary_fit$strata,
  Survival_5Y = summary_fit$surv,
  Lower_CI = summary_fit$lower,
  Upper_CI = summary_fit$upper
)

print(survival_5y)
