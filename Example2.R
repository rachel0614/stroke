# 我们使用vcd包中的Arthritis数据集。该数据来自Kock & Edward (1988)，是一项风湿关节炎新疗法的双盲临床实验结果。
# 被试号（ID）
# 治疗情况（Treatment）：安慰剂治疗（Placebo）、用药治疗（Treated）
# 性别：男性（Male）、女性（Female）
# 年龄（Age）
# 改善情况：无改善（None）、一定程度的改善（Some）、显著改善（Marked）
# 我们想知道治疗情况和改善情况是否相关或独立

library(vcd)
head(Arthritis)
mytable <- xtabs(~Treatment+Improved, data = Arthritis)
mytable
str(Arthritis)
chisq.test(mytable)
# 患者接受的治疗和改善水平看上去存在某种关系（ [公式] ）
# 以我们拒绝了治疗类型和治疗结果相互独立的原假设。
# 都属于非参数检验。
# 卡方检验适用于名义量表，而曼-惠特尼、威尔克松、克-瓦氏适用于顺序量表；
# 曼-惠特尼用于比较两个独立样本，对应参数检验中的独立样本t检验；
# 威尔克松用于比较两个相关样本，对应参数检验中的相关样本t检验；
# 克-瓦氏用于比较两个或两个以上独立样本，对应参数检验中的F检验。
