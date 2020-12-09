# Download new LAFA

prefix="https://www.ers.usda.gov/webdocs/DataFiles/50472/"
suffix=".xls?v=2267.7"

for food in "calories" "Dairy" "fat" "servings" "Fruit" "grain" "meat" "sugar" "veg";
do
	wget -O ${food}.xls ${prefix}${food}${suffix}
done	
