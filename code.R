barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("Energy_kcal", "Phosphorus_mg", "Total_fat_gm", "Total_monounsaturated_fatty_acids_gm", 
"Protein_gm", "Potassium_mg", "Total_saturated_fatty_acids_gm", 
"Sodium_mg", "Total_folate_mcg", "Selenium_mcg", "Magnesium_mg", 
"Iron_mg", "Thiamin_Vitamin_B1_mg", "Folate_DFE_mcg", "Riboflavin_Vitamin_B2_mg", 
"Total_choline_mg", "Zinc_mg", "Carbohydrate_gm", "Total_polyunsaturated_fatty_acids_gm", 
"Niacin_mg")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("Vitamin_K_mcg", "Vitamin_B12_mcg", "Lutein_zeaxanthin_mcg", 
"beta_carotene_mcg", "Folic_acid_mcg", "Added_vitamin_B12_mcg", 
"Retinol_mcg", "Vitamin_A_RAE_mcg", "Folate_DFE_mcg", "Dietary_fiber_gm"
)
plot.PCA(res, select = drawn, axes = 3:4, choix = 'var', title = '', cex = cex)
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 5:6, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("Caffeine_mg", "Total_sugars_gm", "Theobromine_mg", "Alcohol_gm", 
"Moisture_gm", "Vitamin_B6_mg", "Retinol_mcg", "Vitamin_A_RAE_mcg", 
"Niacin_mg", "Carbohydrate_gm")
plot.PCA(res, select = drawn, axes = 5:6, choix = 'var', title = '', cex = cex)
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 7:8, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("alpha_carotene_mcg", "tocopherol_Vitamin_E_mg", "tocopherol_mg", 
"beta_carotene_mcg", "Added_vitamin_B12_mcg", "Vitamin_K_mcg", 
"Lutein_zeaxanthin_mcg", "Copper_mg", "cryptoxanthin_mcg", "Retinol_mcg"
)
plot.PCA(res, select = drawn, axes = 7:8, choix = 'var', title = '', cex = cex)
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 9:10, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("cryptoxanthin_mcg", "alpha_carotene_mcg", "tocopherol_Vitamin_E_mg", 
"Lutein_zeaxanthin_mcg", "Vitamin_C_mg", "tocopherol_mg", "Vitamin_K_mcg", 
"Caffeine_mg", "Total_sugars_gm", "Alcohol_gm")
plot.PCA(res, select = drawn, axes = 9:10, choix = 'var', title = '', cex = cex)
