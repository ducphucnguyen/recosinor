
# recosinor: Enhancing Endogenous Circadian Period Estimation

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/recosinor)](https://cran.r-project.org/package=recosinor)
[![Build Status](https://travis-ci.org/ducphucnguyen/recosinor.svg?branch=main)](https://travis-ci.org/ducphucnguyen/recosinor)

## Overview

**recosinor** is a flexible R package for modeling circadian rhythms using measured core body temperature (CBT) data. It is specifically designed to enhance the estimation of endogenous circadian rhythms by removing masking effects. **recosinor** utilizes a generalized additive model (GAM), leveraging the advantages of this powerful modeling framework.

Key Features:
- Fitting a recursive cosinor function to CBT data
- Estimating endogenous circadian period by removing sleep and activity effects
- Estimating homeostatic sleep drive
- Leveraging all other features of the GAM model for comprehensive analysis

## Installation

You can install the latest stable release of **recosinor** from CRAN using:

```r
install.packages("recosinor")
```

To install the development version from GitHub, use:

```r
# install.packages("devtools")
devtools::install_github("ducphucnguyen/recosinor")
```

## Usage

To analyze core body temperature (CBT) data and estimate circadian rhythms using **recosinor**, follow these steps:

1. Load the **recosinor** package:

   ```r
   library(recosinor)
   ```

2. Load your CBT data. You can use the example data provided in the package:

  ```r
    # Load the example CBT data
    data("cbt_data")
    
    # Show the structure of the dataset
    str(cbt_data)
    
    # Show the first few rows of the dataset
    head(cbt_data)
  ```
  
  Here's the output of `head(cbt_data)`: hrs: hours from starting measurement;
  cbt: core body temperature in oC; sw: is binary data where 0 is wake and 1 is
  sleep; MovingAvgHR: heart rate data that are applied moving average to reduce
  the noise.
  
  ```
              hrs   cbt sw MovingAvgHR
  314 0.000000000 36.78  0    76.18105
  315 0.008333333 36.82  0    76.18105
  316 0.016666667 36.83  0    76.18105
  317 0.025000000 36.83  0    76.18105
  318 0.033333333 36.85  0    76.18105
  319 0.041666667 36.86  0    76.18105
  ```
**Column Descriptions**:
- **hrs**: Hours from the starting measurement.
- **cbt**: Core body temperature in degrees Celsius.
- **sw**: Binary data where 0 represents wake and 1 represents sleep.
- **MovingAvgHR**: Heart rate data that have been smoothed using moving average to reduce noise.


3. Fit a recosinor model to the CBT data, specifying the formula and other parameters:

```markdown

# Fit a recosinor model to the CBT data, specifying the formula and other parameters
model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR),
                       cbt_data,
                       tau = c(24))

# View a summary of the recosinor model to examine the results
summary(model)
```

Here's the output of `summary(model)`:

```
Family: gaussian 
Link function: identity 

Formula:
cbt ~ s(MovingAvgHR) + ysin + ycos + sleep_response

Parametric coefficients:
                Estimate Std. Error  t value Pr(>|t|)    
(Intercept)    36.777567   0.002166 16979.87   <2e-16 ***
ysin           -0.388020   0.003518  -110.29   <2e-16 ***
ycos            0.146950   0.003098    47.44   <2e-16 ***
sleep_response -1.280282   0.023049   -55.55   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                 edf Ref.df     F p-value    
s(MovingAvgHR) 8.655  8.967 177.8  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =   0.86   Deviance explained =   86%
fREML = -2684.1  Scale est. = 0.018493  n = 4724
```


4. Create a recosinor plot to visualize the estimated circadian rhythms and other relevant information:

```markdown
   recosinor.plot(model, cbt_data)
```

![Recosinor Plot](example/example_plot.png)


## Example: Optimising model parameters


In the previous example, only the circadian period was specified explicitly, while other model parameters used default values. However, optimizing key parameters can significantly improve the model fit. In the following code, we will optimize three crucial parameters:

1. **Circadian Period**: This parameter determines the length of the circadian rhythm.
2. **k Parameter**: The `k` parameter controls the shape of the cosine function. Higher values make it more symmetric, while lower values skew it.
3. **Shape Factor of Gamma Function**: This factor influences the shape of the homeostatic sleep drive.

You can customize these parameters to fine-tune the model and improve its accuracy.


```r
recosinor_obj <- function(params) {
  
  period <- params[1] # this controls tau
  k <- params[2] # controls the shape of the cosine function (skew vs symmetric)
  shape <- params[3] # controls the shape of the sleep drive function
  
  model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR),
                         cbt_data,
                         tau = c(period),
                         k = k, 
                         shape = shape) 
  
  # Save the best model
  assign("best_model", model, envir = .GlobalEnv)
  
  return(sum(residuals(model)^2))
}

library(optimx)
# Use the optimx function 
recosinor_opt <- optimx(c(24,10,3), 
                        recosinor_obj, 
                        method=c("L-BFGS-B","bobyqa"),
                        control=list( 
                          save.failures=TRUE, 
                          trace=1),
                        itnmax = 100,
                        lower = c(23, -100,1), 
                        upper = c(25, 100, 5) )

params <- as.matrix(summary(recosinor_opt, order = value)[1, 1:3])
recosinor_obj(params) # get the best model parameters
summary(best_model)
```

The best model is the period of 23.2, k = 100 and shape = 4.4:

```
Family: gaussian 
Link function: identity 

Formula:
cbt ~ s(MovingAvgHR) + ysin + ycos + sleep_response

Parametric coefficients:
                Estimate Std. Error  t value Pr(>|t|)    
(Intercept)    36.787790   0.001850 19882.48   <2e-16 ***
ysin           -0.386768   0.003131  -123.52   <2e-16 ***
ycos            0.080496   0.002560    31.45   <2e-16 ***
sleep_response -1.606467   0.022619   -71.02   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                 edf Ref.df     F p-value    
s(MovingAvgHR) 8.658  8.967 204.8  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.899   Deviance explained = 89.9%
fREML = -3446.6  Scale est. = 0.013387  n = 4724

Optimization Parameters:
               p1  p2       p3
L-BFGS-B 23.24315 100 4.401075
```


![Optimised Recosinor](example/example_plot_opt.png)


## Contributing

We welcome contributions to **recosinor**! If you'd like to contribute to this project, please read our [Contributing Guidelines](CONTRIBUTING.md). We appreciate bug reports, documentation improvements, and new feature implementations.

## Issues

If you encounter any issues or have questions, please [open an issue](https://github.com/ducphucnguyen/recosinor/issues) on our GitHub repository.

## License

**recosinor** is licensed under the MIT License. See the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

We extend our appreciation to the R community and all contributors for their invaluable support and contributions to **recosinor**.

## Contact

- Phuc D. Nguyen
- ducphuc.nguyen@flinders.edu.au
