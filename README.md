- Pricing pure premiums for vehicle insurance policy holders
- Extension of an assignment in ACTL30008. View the report here [report/ACTL30004report.pdf]
- Data is simulated
- To access the pricing app, visit: [https://doitfornoreason.shinyapps.io/VehicleInsurancePricing/](https://doitfornoreason.shinyapps.io/VehicleInsurancePricing/)
# Brief description:
## Pre-processing and EDA
Missing data was:
- Tested via independence tests to determine if they were Missing at Random
- Imputed via KNN imputation to avoid wasting data points
    - This doubled as a method to approximate pure premiums for individual policyholders when you don't know all of their risk factors.
 
Acknowledging the heavy tailed nature of insurance data:
- Extreme Value Theory (EVT) analysis was conducted

## Model selection
Instead of black-box Machine Learning models, GLMs were utilised for their explainability, using the standard frequency-severity approach to calculate pure premiums for individuals. i.e:

$$\text{Pure premium for policy holder }i = P(\text{nonzero claim from policy holder }i) \times E(\text{Claim size }|\text{ nonzero claim from policy holder }i)$$

## Deployment
Shiny was a natural tool to publish the pricing app, as this project was conducted in RStudio

![alt text](https://github.com/doitfornoreason/VehicleInsurancePricing/blob/main/pricingapp1.jpg "Pricing App")
