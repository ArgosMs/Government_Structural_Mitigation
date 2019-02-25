dat <- PhD_Dataset

dev.off()

par(mfrow=c(1, 1))

# Barplots for IDs

id_dam_reduced_damage <- table(dat$DAM_REDUCED_DAMAGE)

barplot(main = "Dam Reduced Damage", table(dat$DAM_REDUCED_DAMAGE))

id_dam_will_reduce_damage <- table(dat$DAM_WILL_REDUCE)

barplot(main = "Dam Will Reduce Damage", table(dat$DAM_WILL_REDUCE))

id_water_release <- table(dat$WATER_RELEASE)

barplot(main = "Water Released Increased Damage", table(dat$WATER_RELEASE))

id_flood_damage_control <- table(dat$FLOOD_DAMAGE_CONTROL)

barplot(main = "Flood Damage Control Through Dam Water Release", table(dat$FLOOD_DAMAGE_CONTROL))

id_damage_accept <- table(dat$DAMAGE_ACCEPT)

barplot(main = "Percentage of Flood Damage Acceptance to Dam Water Release", table(dat$DAMAGE_ACCEPT))

id_financial_compensation <- table(dat$FINANCIAL_COMPENSATION)

barplot(main = "Percentage of Financial Compensation Pursued", table(dat$FINANCIAL_COMPENSATION))

id_new_dams <- table(dat$NEW_DAMS)

barplot(main = "Perceived Damage Reduction by New Dams", table(dat$NEW_DAMS))

id_wall_raising <- table(dat$WALL_RAISING)

barplot(main = "Perception of Damage Reduction by Raising Wivenhoe Dam Wall", table(dat$WALL_RAISING))

# DVs

id_wall_release_dependent <- table(dat$WATER_RELEASE_DEPENDENT)

barplot(main = "Dam Water Release Increased Property Damage", table(dat$WATER_RELEASE_DEPENDENT))

id_flood_damage_control_dependent <- table(dat$FLOOD_DAMAGE_CONTROL_DEPENDENT)

barplot(main = "Flood Damage Control Through Dam Water Release", table(dat$FLOOD_DAMAGE_CONTROL_DEPENDENT))

dat1 <- dat

dependent_insurance <- table(dat1$INSURANCE_DEPENDENT)
dependent_house_raising <- table(dat1$HOUSE_RAISING_DEPENDENT)
dependent_measure <- table(dat1$MEASURE_DEPENDENT)
dependent_relocation <- table(dat1$RELOCATION_DEPENDENT)

dependent_combined <- cbind(dependent_insurance, dependent_house_raising, dependent_measure, dependent_relocation)

barplot(dependent_combined, 
        width = 0.53,
        mgp=c(2.5, .6, -.4), # First: xlab and ylab location. Second: Tick-mark labels. Third: Tick marks.
        cex.main = 1.3,
        cex.names = .65,
        legend = c("No", "Yes"),
        xlim = c(2.6, -.45),               
        names.arg = c("Insurance", "Raising", "Home Improv.", "Relocation"),
        ylab = "Frequency", 
        main = "Comparative Analysis of Protective Actions",
        las = 1,
        args.legend = 
          list("topright",
               bty = "n")) # 1:hor 2:perpendicular to axis 3:vertical

# Models

logitInsurance_gov <- glm(INSURANCE_DEPENDENT ~ 
                            DAM_REDUCED_DAMAGE +
                            DAM_WILL_REDUCE +
                            WATER_RELEASE +
                            FLOOD_DAMAGE_CONTROL +
                            DAMAGE_ACCEPT +
                            FINANCIAL_COMPENSATION +
                            NEW_DAMS +
                            WALL_RAISING,
                                data = dat)

summary(logitInsurance_gov)

vif(logitInsurance_gov)

logitMeasure_gov <- glm(MEASURE_DEPENDENT ~ 
                            DAM_REDUCED_DAMAGE +
                            DAM_WILL_REDUCE +
                            WATER_RELEASE +
                            FLOOD_DAMAGE_CONTROL +
                            DAMAGE_ACCEPT +
                            FINANCIAL_COMPENSATION +
                            NEW_DAMS +
                            WALL_RAISING,
                          data = dat)

summary(logitMeasure_gov)

logitRaising_gov <- glm(HOUSE_RAISING_DEPENDENT ~ 
                            DAM_REDUCED_DAMAGE +
                            DAM_WILL_REDUCE +
                            WATER_RELEASE +
                            FLOOD_DAMAGE_CONTROL +
                            DAMAGE_ACCEPT +
                            FINANCIAL_COMPENSATION +
                            NEW_DAMS +
                            WALL_RAISING,
                          data = dat)

summary(logitRaising_gov)

logitRelocation_gov <- glm(RELOCATION_DEPENDENT ~ 
                            DAM_REDUCED_DAMAGE +
                            DAM_WILL_REDUCE +
                            WATER_RELEASE +
                            FLOOD_DAMAGE_CONTROL +
                            DAMAGE_ACCEPT +
                            FINANCIAL_COMPENSATION +
                            NEW_DAMS +
                            WALL_RAISING,
                          data = dat)

summary(logitRelocation_gov)

logitDamage_Control_gov <- glm(FLOOD_DAMAGE_CONTROL_DEPENDENT ~ 
                             DAM_REDUCED_DAMAGE +
                             DAM_WILL_REDUCE +
                             WATER_RELEASE +
                             DAMAGE_ACCEPT +
                             FINANCIAL_COMPENSATION +
                             NEW_DAMS +
                             WALL_RAISING,
                           data = dat)

summary(logitDamage_Control_gov)

logitWater_Release_Dependent_gov <- glm(WATER_RELEASE_DEPENDENT ~ 
                                 DAM_REDUCED_DAMAGE +
                                 DAM_WILL_REDUCE +
                                 FLOOD_DAMAGE_CONTROL +
                                 DAMAGE_ACCEPT +
                                 FINANCIAL_COMPENSATION +
                                 NEW_DAMS +
                                 WALL_RAISING,
                               data = dat)

summary(logitWater_Release_Dependent_gov)

# Outputs

# Regression Outputs

Insurance_gov_table <- xtable(logitInsurance_gov)
print.xtable(Insurance_gov_table, type="html", file="PhD_Paper_Government_Insurancetable.html")

Measure_gov_table <- xtable(logitMeasure_gov)
print.xtable(Measure_gov_table, type="html", file="PhD_Paper_Government_Measuretable.html")

Insurance_gov_table <- xtable(logitInsurance_gov)
print.xtable(Insurance_gov_table, type="html", file="PhD_Paper_Government_Insurancetable.html")

Raising_gov_table <- xtable(logitRaising_gov)
print.xtable(Raising_gov_table, type="html", file="PhD_Paper_Government_Raisingtable.html")

Relocation_gov_table <- xtable(logitRelocation_gov)
print.xtable(Relocation_gov_table, type="html", file="PhD_Paper_Government_Relocationtable.html")

Damage_Control_gov_table <- xtable(logitDamage_Control_gov)
print.xtable(Damage_Control_gov_table, type="html", file="PhD_Paper_Government_Damage_Controltable.html")

Water_Release_Dependent_gov_table <- xtable(logitWater_Release_Dependent_gov)
print.xtable(Water_Release_Dependent_gov_table, type="html", file="PhD_Paper_Government_Water_Release_Dependenttable.html")




